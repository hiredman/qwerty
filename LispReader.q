(qwerty/package main)
(qwerty/do
 (qwerty/do
  (qwerty/struct LispReader)
  (qwerty/defgofun
   InitLispReader
   (G__415)
   ((*LispReader) ())
   (qwerty/do))
  (qwerty/do
   (qwerty/local QUOTE Symbol)
   (qwerty/set! QUOTE (qwerty/. Symbol_intern "quote")))
  (qwerty/do
   (qwerty/local THE_VAR Symbol)
   (qwerty/set! THE_VAR (qwerty/. Symbol_intern "var")))
  (qwerty/do
   (qwerty/local UNQUOTE Symbol)
   (qwerty/set!
    UNQUOTE
    (qwerty/. Symbol_intern "clojure.core" "unquote")))
  (qwerty/do
   (qwerty/local UNQUOTE_SPLICING Symbol)
   (qwerty/set!
    UNQUOTE_SPLICING
    (qwerty/. Symbol_intern "clojure.core" "unquote-splicing")))
  (qwerty/do
   (qwerty/local CONCAT Symbol)
   (qwerty/set!
    CONCAT
    (qwerty/. Symbol_intern "clojure.core" "concat")))
  (qwerty/do
   (qwerty/local SEQ Symbol)
   (qwerty/set! SEQ (qwerty/. Symbol_intern "clojure.core" "seq")))
  (qwerty/do
   (qwerty/local LIST Symbol)
   (qwerty/set! LIST (qwerty/. Symbol_intern "clojure.core" "list")))
  (qwerty/do
   (qwerty/local APPLY Symbol)
   (qwerty/set! APPLY (qwerty/. Symbol_intern "clojure.core" "apply")))
  (qwerty/do
   (qwerty/local HASHMAP Symbol)
   (qwerty/set!
    HASHMAP
    (qwerty/. Symbol_intern "clojure.core" "hash-map")))
  (qwerty/do
   (qwerty/local HASHSET Symbol)
   (qwerty/set!
    HASHSET
    (qwerty/. Symbol_intern "clojure.core" "hash-set")))
  (qwerty/do
   (qwerty/local VECTOR Symbol)
   (qwerty/set!
    VECTOR
    (qwerty/. Symbol_intern "clojure.core" "vector")))
  (qwerty/do
   (qwerty/local WITH_META Symbol)
   (qwerty/set!
    WITH_META
    (qwerty/. Symbol_intern "clojure.core" "with-meta")))
  (qwerty/do
   (qwerty/local META Symbol)
   (qwerty/set! META (qwerty/. Symbol_intern "clojure.core" "meta")))
  (qwerty/do
   (qwerty/local DEREF Symbol)
   (qwerty/set! DEREF (qwerty/. Symbol_intern "clojure.core" "deref")))
  (qwerty/do
   (qwerty/local UNKNOWN Keyword)
   (qwerty/set! UNKNOWN (qwerty/. Keyword_intern nil "unknown")))
  (qwerty/do
   (qwerty/local macros IFn)
   (qwerty/set! macros (qwerty/make "[]IFn 256")))
  (qwerty/do
   (qwerty/local dispatchMacros IFn)
   (qwerty/set! dispatchMacros (qwerty/make "[]IFn 256")))
  (qwerty/do
   (qwerty/local symbolPat Pattern)
   (qwerty/set!
    symbolPat
    (qwerty/.
     Pattern_compile
     "[:]?([\\\\D&&[^/]].*/)?([\\\\D&&[^/]][^/]*)")))
  (qwerty/do
   (qwerty/local intPat Pattern)
   (qwerty/set!
    intPat
    (qwerty/.
     Pattern_compile
     "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")))
  (qwerty/do
   (qwerty/local ratioPat Pattern)
   (qwerty/set!
    ratioPat
    (qwerty/. Pattern_compile "([-+]?[0-9]+)/([0-9]+)")))
  (qwerty/do
   (qwerty/local floatPat Pattern)
   (qwerty/set!
    floatPat
    (qwerty/.
     Pattern_compile
     "([-+]?[0-9]+(\\\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")))
  (qwerty/do
   (qwerty/local SLASH Symbol)
   (qwerty/set! SLASH (qwerty/. Symbol_intern "/")))
  (qwerty/do
   (qwerty/local CLOJURE_SLASH Symbol)
   (qwerty/set!
    CLOJURE_SLASH
    (qwerty/. Symbol_intern "clojure.core" "/")))
  (qwerty/do
   (qwerty/local GENSYM_ENV Var)
   (qwerty/set!
    GENSYM_ENV
    (qwerty/go-method-call (qwerty/. Var_create nil) setDynamic)))
  (qwerty/do
   (qwerty/local ARG_ENV Var)
   (qwerty/set!
    ARG_ENV
    (qwerty/go-method-call (qwerty/. Var_create nil) setDynamic)))
  (qwerty/do
   (qwerty/local ctorReader IFn)
   (qwerty/set! ctorReader (qwerty/. NewCtorReader)))
  (user/init-decl)
  (user/method-decl)
  (user/method-decl)
  (qwerty/do
   (qwerty/struct ReaderException line Int column Int)
   (qwerty/defgofun
    InitReaderException
    (G__416)
    ((*ReaderException) ())
    (qwerty/do
     (qwerty/set! line (qwerty/do))
     (qwerty/set! column (qwerty/do))))
   (qwerty/defgofun
    NewReaderException
    ()
    (user/r)
    (() (*ReaderException))
    (qwerty/let*
     ((i__388__auto__ (qwerty/new ReaderException)))
     (qwerty/. InitReaderException i__388__auto__)
     i__388__auto__)))
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (user/method-decl)
  (qwerty/do
   (qwerty/struct RegexReader)
   (qwerty/defgofun
    InitRegexReader
    (G__417)
    ((*RegexReader) ())
    (qwerty/do))
   (qwerty/do
    (qwerty/local stringrdr StringReader)
    (qwerty/set! stringrdr (qwerty/. NewStringReader)))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct StringReader)
   (qwerty/defgofun
    InitStringReader
    (G__418)
    ((*StringReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct CommentReader)
   (qwerty/defgofun
    InitCommentReader
    (G__419)
    ((*CommentReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct DiscardReader)
   (qwerty/defgofun
    InitDiscardReader
    (G__420)
    ((*DiscardReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct WrappingReader sym Symbol)
   (qwerty/defgofun
    InitWrappingReader
    (G__421)
    ((*WrappingReader) ())
    (qwerty/do (qwerty/set! sym (qwerty/do))))
   (qwerty/defgofun
    NewWrappingReader
    ()
    (user/r)
    (() (*WrappingReader))
    (qwerty/let*
     ((i__388__auto__ (qwerty/new WrappingReader)))
     (qwerty/. InitWrappingReader i__388__auto__)
     i__388__auto__))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct DeprecatedWrappingReader sym Symbol macro String)
   (qwerty/defgofun
    InitDeprecatedWrappingReader
    (G__422)
    ((*DeprecatedWrappingReader) ())
    (qwerty/do
     (qwerty/set! sym (qwerty/do))
     (qwerty/set! macro (qwerty/do))))
   (qwerty/defgofun
    NewDeprecatedWrappingReader
    ()
    (user/r)
    (() (*DeprecatedWrappingReader))
    (qwerty/let*
     ((i__388__auto__ (qwerty/new DeprecatedWrappingReader)))
     (qwerty/. InitDeprecatedWrappingReader i__388__auto__)
     i__388__auto__))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct VarReader)
   (qwerty/defgofun
    InitVarReader
    (G__423)
    ((*VarReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct DispatchReader)
   (qwerty/defgofun
    InitDispatchReader
    (G__424)
    ((*DispatchReader) ())
    (qwerty/do))
   (user/method-decl))
  (user/method-decl)
  (qwerty/do
   (qwerty/struct FnReader)
   (qwerty/defgofun InitFnReader (G__425) ((*FnReader) ()) (qwerty/do))
   (user/method-decl))
  (user/method-decl)
  (qwerty/do
   (qwerty/struct ArgReader)
   (qwerty/defgofun
    InitArgReader
    (G__426)
    ((*ArgReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct MetaReader)
   (qwerty/defgofun
    InitMetaReader
    (G__427)
    ((*MetaReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct SyntaxQuoteReader)
   (qwerty/defgofun
    InitSyntaxQuoteReader
    (G__428)
    ((*SyntaxQuoteReader) ())
    (qwerty/do))
   (user/method-decl)
   (user/method-decl)
   (user/method-decl)
   (user/method-decl))
  (user/method-decl)
  (user/method-decl)
  (qwerty/do
   (qwerty/struct UnquoteReader)
   (qwerty/defgofun
    InitUnquoteReader
    (G__429)
    ((*UnquoteReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct CharacterReader)
   (qwerty/defgofun
    InitCharacterReader
    (G__430)
    ((*CharacterReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct ListReader)
   (qwerty/defgofun
    InitListReader
    (G__431)
    ((*ListReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct EvalReader)
   (qwerty/defgofun
    InitEvalReader
    (G__432)
    ((*EvalReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct VectorReader)
   (qwerty/defgofun
    InitVectorReader
    (G__433)
    ((*VectorReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct MapReader)
   (qwerty/defgofun
    InitMapReader
    (G__434)
    ((*MapReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct SetReader)
   (qwerty/defgofun
    InitSetReader
    (G__435)
    ((*SetReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct UnmatchedDelimiterReader)
   (qwerty/defgofun
    InitUnmatchedDelimiterReader
    (G__436)
    ((*UnmatchedDelimiterReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct UnreadableReader)
   (qwerty/defgofun
    InitUnreadableReader
    (G__437)
    ((*UnreadableReader) ())
    (qwerty/do))
   (user/method-decl))
  (user/method-decl)
  (qwerty/do
   (qwerty/struct CtorReader)
   (qwerty/defgofun
    InitCtorReader
    (G__438)
    ((*CtorReader) ())
    (qwerty/do))
   (user/method-decl)
   (user/method-decl)
   (user/method-decl))))
