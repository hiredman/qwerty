#!/usr/bin/java -jar /Users/hiredman/src/clojure/target/clojure-1.5.0-master-SNAPSHOT.jar

(let [pom-uber-jar
      (str "http://thelibraryofcongress.s3.amazonaws.com/"
           "pomegranate-0.0.13-SNAPSHOT-jar-with-dependencies.jar")
      cl (java.net.URLClassLoader. (into-array [(java.net.URL. pom-uber-jar)]))
      cx (.getContextClassLoader (Thread/currentThread))]
  (push-thread-bindings {clojure.lang.Compiler/LOADER cl})
  (.setContextClassLoader (Thread/currentThread) cl)
  (try
    (require '[cemerick.pomegranate :as pom])
    (finally
      (.setContextClassLoader (Thread/currentThread) cx)
      (pop-thread-bindings))))

(pom/add-dependencies :coordinates '[[com.google.code.javaparser/javaparser "1.0.8"]]
                      :repositories (merge cemerick.pomegranate.aether/maven-central
                                           {"clojars" "http://clojars.org/repo"}))

(def types
  {})

(defmulti type-convert type)
(defmethod type-convert japa.parser.ast.type.ReferenceType [t]
  (symbol (.getName (.getType t))))
(defmethod type-convert japa.parser.ast.type.PrimitiveType [t]
  (symbol (.toString (.getType t))))
(defmethod type-convert japa.parser.ast.type.ClassOrInterfaceType [t]
  (symbol (.getName t)))


(defn static? [o]
  (java.lang.reflect.Modifier/isStatic
   (.getModifiers o)))

(def imports (atom '#{Symbol Pattern Keyword Var}))


(defmulti goify type)
(defmethod goify japa.parser.ast.CompilationUnit [cu]
  (doseq [i (.getImports cu)]
    (swap! imports conj (symbol (last (.split (.getName (.getName i)) "\\.")))))
  `(qwerty/do
     ~@(map goify (.getTypes cu))))

(declare ^:dynamic *type*)

(defmethod goify japa.parser.ast.body.ClassOrInterfaceDeclaration [cu]
  (let [fields (filter #(instance? japa.parser.ast.body.FieldDeclaration %) (.getMembers cu))
        methods (remove #(instance? japa.parser.ast.body.FieldDeclaration %) (.getMembers cu))
        static-fields (filter static? fields)
        instance-fields (remove static? fields)
        type-name (symbol (.getName cu))
        init-this (gensym)]
    (binding [*type* type-name]
      `(qwerty/do
         (qwerty/struct ~type-name
                        ~@(for [field instance-fields
                                v (.getVariables field)
                                x [(symbol (.getName (.getId v))) (type-convert (.getType field))]]
                            x))
         (qwerty/defgofun ~(symbol (str "Init" type-name)) (~init-this)
           ((~(symbol (str "*" type-name))) ())
           (qwerty/do
             ~@(for [field instance-fields
                     v (.getVariables field)]
                 `(qwerty/set!
                   (qwerty/.- ~init-this ~(symbol (.getName (.getId v))))
                   ~(goify (.getInit v))))))
         ~@(doall (map goify static-fields))
         ~@(doall (map goify methods))))))

(defmethod goify japa.parser.ast.body.FieldDeclaration [field]
  (cons 'qwerty/do
        (for [v (.getVariables field)
              x [`(qwerty/local ~(symbol (.getName (.getId v)))
                                ~(type-convert (.getType field)))
                 `(qwerty/set! (qwerty/goref ~(symbol (.getName (.getId v))))
                               ~(goify (.getInit v)))]]
          x)))

(defmethod goify japa.parser.ast.expr.MethodCallExpr [cu]
  (let [scope (goify (.getScope cu))]
    (if (@imports scope)
      `(qwerty/. ~(symbol (str scope "_" (.getName cu)))
                 ~@(map
                    (fn [x]
                      (if (symbol? x)
                        `(qwerty/goref ~x)
                        x))
                    (map goify (.getArgs cu))))
      `(qwerty/go-method-call ~scope
                              ~(symbol (.getName cu))
                              ~@(map goify (.getArgs cu))))))
(defmethod goify japa.parser.ast.expr.ArrayCreationExpr [cu]
  `(qwerty/make  ~(str "[]" (type-convert (.getType cu))
                       " "
                       (apply str
                              (interpose \space (map goify (.getDimensions cu)))))))
(defmethod goify japa.parser.ast.expr.ObjectCreationExpr [cu]
  `(qwerty/. ~(symbol (str "New" (type-convert (.getType cu))))
             ~@(map (fn [x]
                      (if (symbol? x)
                        `(qwerty/goref ~x)
                        x))
                    (map goify (.getArgs cu)))))
(defmethod goify japa.parser.ast.body.InitializerDeclaration [cu]
  (assert (.isStatic cu))
  `(qwerty/defgofun init ()
     (() ())
     ~(goify (.getBlock cu))))
(defmethod goify japa.parser.ast.body.MethodDeclaration [cu]
  (if (static? cu)
    (let [return-type (symbol (str (.getType cu)))]
      `(qwerty/defgofun ~(symbol (str *type* "_" (.getName cu))) ~(map goify (.getParameters cu))
         (() ~(cond
               (= return-type 'boolean)
               '(bool)
               (= return-type 'void)
               ()
               :else
               (list return-type)))
         ~(goify (.getBody cu))))
    `(method-decl)))
(defmethod goify japa.parser.ast.body.Parameter [cu]
  (symbol (.getName (.getId cu))))
(defmethod goify japa.parser.ast.body.ConstructorDeclaration [cu]
  `(qwerty/defgofun  ~(symbol (str "New" (.getName cu))) ()
     (() (~(symbol (str "*" (.getName cu)))))
     (qwerty/let* ((i# (qwerty/new ~(symbol (.getName cu)))))
                  (qwerty/. ~(symbol (str "Init" (.getName cu))) i#)
                  i#)))
(defmethod goify nil [cu]
  `(qwerty/do))
(defmethod goify japa.parser.ast.expr.NameExpr [cu]
  (symbol (.getName cu)))
(defmethod goify japa.parser.ast.expr.StringLiteralExpr [cu]
  (.getValue cu))
(defmethod goify japa.parser.ast.expr.NullLiteralExpr [cu]
  nil)
(defmethod goify japa.parser.ast.expr.BinaryExpr [cu]
  (let [op (goify (.getOperator cu))
        a (goify (.getLeft cu))
        b (goify (.getRight cu))
        op (symbol "qwerty" op)]
    (cond
     (= 'qwerty/or op)
     `(qwerty/let* ((a# ~a)) (qwerty/if a# a# ~b))
     (= 'qwerty/equals op)
     `(qwerty/= ~a ~b)
     :else
     `(~op ~a ~b))))
(defmethod goify japa.parser.ast.expr.CastExpr [cu]
  `(qwerty/cast ~(type-convert (.getType cu)) ~(goify (.getExpr cu))))
(defmethod goify japa.parser.ast.expr.FieldAccessExpr [cu]
  `(qwerty/.- ~(goify (.getScope cu)) ~(symbol (.getField cu))))
(defmethod goify japa.parser.ast.expr.EnclosedExpr [cu]
  (goify (.getInner cu)))
(defmethod goify japa.parser.ast.expr.BinaryExpr$Operator [cu]
  (.toString cu))
(defmethod goify japa.parser.ast.expr.VariableDeclarationExpr [cu]
  `(qwerty/variable))
(defmethod goify japa.parser.ast.stmt.BlockStmt [cu]
  `(qwerty/do
     ~@(map goify (.getStmts cu))))
(defmethod goify japa.parser.ast.stmt.ReturnStmt [cu]
  `(qwerty/do
     (qwerty/comment "return" (str cu))
     ~(goify (.getExpr cu))))
(defmethod goify japa.parser.ast.stmt.IfStmt [cu]
  `(qwerty/if))
(defmethod goify japa.parser.ast.stmt.TryStmt [cu]
  `(qwerty/try))
(defmethod goify japa.parser.ast.stmt.ForStmt [cu]
  `(qwerty/for))
(defmethod goify japa.parser.ast.stmt.ThrowStmt [cu]
  `(qwerty/throw))
(defmethod goify japa.parser.ast.stmt.ExpressionStmt [cu]
  (goify (.getExpression cu)))
(defmethod goify japa.parser.ast.expr.AssignExpr
  [cu]
  `(qwerty/set! (qwerty/goref ~(goify (.getTarget cu)))
                ~(goify (.getValue cu))))
(defmethod goify japa.parser.ast.expr.ArrayAccessExpr
  [cu]
  `(qwerty/array-access))




(use 'clojure.pprint)

(prn `(qwerty/package ~'main))
(pprint (goify (japa.parser.JavaParser/parse System/in)))
