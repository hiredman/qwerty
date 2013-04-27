(qwerty/package main)
(qwerty/varize false)

(qwerty/struct LispReader)


(qwerty/defgofun InitLispReader (G__542) ((*LispReader) ()) (qwerty/do))


(qwerty/godef QUOTE (qwerty/. Symbol_intern "quote"))



(qwerty/godef THE_VAR (qwerty/. Symbol_intern "var"))



(qwerty/godef UNQUOTE (qwerty/. Symbol_intern "clojure.core" "unquote"))



(qwerty/godef
 UNQUOTE_SPLICING
 (qwerty/. Symbol_intern "clojure.core" "unquote-splicing"))



(qwerty/godef CONCAT (qwerty/. Symbol_intern "clojure.core" "concat"))



(qwerty/godef SEQ (qwerty/. Symbol_intern "clojure.core" "seq"))



(qwerty/godef LIST (qwerty/. Symbol_intern "clojure.core" "list"))



(qwerty/godef APPLY (qwerty/. Symbol_intern "clojure.core" "apply"))



(qwerty/godef
 HASHMAP
 (qwerty/. Symbol_intern "clojure.core" "hash-map"))



(qwerty/godef
 HASHSET
 (qwerty/. Symbol_intern "clojure.core" "hash-set"))



(qwerty/godef VECTOR (qwerty/. Symbol_intern "clojure.core" "vector"))



(qwerty/godef
 WITH_META
 (qwerty/. Symbol_intern "clojure.core" "with-meta"))



(qwerty/godef META (qwerty/. Symbol_intern "clojure.core" "meta"))



(qwerty/godef DEREF (qwerty/. Symbol_intern "clojure.core" "deref"))



(qwerty/godef UNKNOWN (qwerty/. Keyword_intern nil "unknown"))



(qwerty/godef macros (qwerty/make "[]IFn, 256"))



(qwerty/godef dispatchMacros (qwerty/make "[]IFn, 256"))



(qwerty/godef
 symbolPat
 (qwerty/.
  Pattern_compile
  "[:]?([\\\\D&&[^/]].*/)?([\\\\D&&[^/]][^/]*)"))



(qwerty/godef
 intPat
 (qwerty/.
  Pattern_compile
  "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?"))



(qwerty/godef
 ratioPat
 (qwerty/. Pattern_compile "([-+]?[0-9]+)/([0-9]+)"))



(qwerty/godef
 floatPat
 (qwerty/.
  Pattern_compile
  "([-+]?[0-9]+(\\\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?"))



(qwerty/godef SLASH (qwerty/. Symbol_intern "/"))



(qwerty/godef CLOJURE_SLASH (qwerty/. Symbol_intern "clojure.core" "/"))



(qwerty/godef
 GENSYM_ENV
 (qwerty/go-method-call (qwerty/. Var_create nil) setDynamic))



(qwerty/godef
 ARG_ENV
 (qwerty/go-method-call (qwerty/. Var_create nil) setDynamic))



(qwerty/godef ctorReader (qwerty/. NewCtorReader))



(qwerty/defgofun
 init
 ()
 (() ())
 (qwerty/do
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) "\"")
    (qwerty/. NewStringReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) ";")
    (qwerty/. NewCommentReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) "\\'")
    (qwerty/. NewWrappingReader (qwerty/goref QUOTE)))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) "@")
    (qwerty/. NewWrappingReader (qwerty/goref DEREF)))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) "(")
    (qwerty/. NewListReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) ")")
    (qwerty/. NewUnmatchedDelimiterReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) "\\\\")
    (qwerty/. NewCharacterReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref macros) "#")
    (qwerty/. NewDispatchReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref dispatchMacros) "\\'")
    (qwerty/. NewVarReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref dispatchMacros) "\"")
    (qwerty/. NewRegexReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref dispatchMacros) "<")
    (qwerty/. NewUnreadableReader))
   nil)
  (qwerty/do
   (qwerty/set!
    (qwerty/aget (qwerty/goref dispatchMacros) "_")
    (qwerty/. NewDiscardReader))
   nil)))


(qwerty/defgofun
 LispReader_isWhitespace
 (ch)
 (() (bool))
 (qwerty/do
  (qwerty/do
   (qwerty/comment "return Character.isWhitespace(ch) || ch == ',';")
   (qwerty/let*
    ((a__413__auto__
      (qwerty/. Character_isWhitespace (qwerty/goref ch))))
    (qwerty/if a__413__auto__ a__413__auto__ (qwerty/= ch ","))))))


(qwerty/defgofun
 LispReader_unread
 (r ch)
 (() ())
 (qwerty/do
  (qwerty/if
   (qwerty/if
    (qwerty/=
     ch
     (qwerty/let*
      ((a__425__auto__ (qwerty/cast int "1")))
      (qwerty/* a__425__auto__ -1)))
    false
    true)
   (qwerty/do
    (qwerty/comment "try")
    (qwerty/defer (qwerty/fn* ()))
    (qwerty/do (qwerty/go-method-call r unread ch)))
   (qwerty/do))))


(qwerty/struct ReaderException line Int column Int)


(qwerty/defgofun
 InitReaderException
 (G__543)
 ((*ReaderException) ())
 (qwerty/do
  (qwerty/set! (qwerty/.- G__543 line) (qwerty/do))
  (qwerty/set! (qwerty/.- G__543 column) (qwerty/do))))


(qwerty/defgofun
 NewReaderException
 ()
 (() (*ReaderException))
 (qwerty/let*
  ((i__388__auto__ (qwerty/new ReaderException)))
  (qwerty/. InitReaderException i__388__auto__)
  i__388__auto__))



(qwerty/defgofun
 LispReader_read1
 (r)
 (() (int))
 (qwerty/do
  (qwerty/do
   (qwerty/comment "try")
   (qwerty/defer (qwerty/fn* ()))
   (qwerty/do
    (qwerty/do
     (qwerty/comment "return r.read();")
     (qwerty/go-method-call r read))))))


(qwerty/defgofun
 LispReader_read
 (r eofIsError eofValue isRecursive)
 (() (Object))
 (qwerty/do
  (qwerty/if
   (qwerty/=
    (qwerty/go-method-call (qwerty/.- RT READEVAL) deref)
    UNKNOWN)
   (qwerty/.
    user/panic
    (qwerty/go-method-call
     Util
     runtimeException
     "Reading disallowed - *read-eval* bound to :unknown"))
   (qwerty/do))
  (qwerty/do
   (qwerty/comment "try")
   (qwerty/defer (qwerty/fn* ()))
   (qwerty/do
    (qwerty/do
     (qwerty/labels
      start
      (qwerty/test (qwerty/do) continue)
      (qwerty/goto end)
      continue
      (qwerty/do
       (qwerty/do
        (qwerty/do
         (qwerty/do
          (qwerty/local ch Int)
          (qwerty/set!
           ch
           (qwerty/go-method-call (qwerty/do) read1 r))))
        (qwerty/labels
         start
         (qwerty/test
          (qwerty/go-method-call (qwerty/do) isWhitespace ch)
          continue)
         (qwerty/goto end)
         continue
         (qwerty/do
          (qwerty/do
           (qwerty/set! ch (qwerty/go-method-call (qwerty/do) read1 r))
           nil))
         end)
        (qwerty/if
         (qwerty/=
          ch
          (qwerty/let*
           ((a__425__auto__ (qwerty/cast int "1")))
           (qwerty/* a__425__auto__ -1)))
         (qwerty/do
          (qwerty/if
           eofIsError
           (qwerty/.
            user/panic
            (qwerty/go-method-call
             Util
             runtimeException
             "EOF while reading"))
           (qwerty/do))
          (qwerty/do (qwerty/comment "return eofValue;") eofValue))
         (qwerty/do))
        (qwerty/if
         (qwerty/. Character_isDigit (qwerty/goref ch))
         (qwerty/do
          (qwerty/do
           (qwerty/do
            (qwerty/local n Object)
            (qwerty/set!
             n
             (qwerty/go-method-call
              (qwerty/do)
              readNumber
              r
              (qwerty/cast Char ch)))))
          (qwerty/if
           (qwerty/go-method-call RT suppressRead)
           (qwerty/do (qwerty/comment "return null;") nil)
           (qwerty/do))
          (qwerty/do (qwerty/comment "return n;") n))
         (qwerty/do))
        (qwerty/do
         (qwerty/do
          (qwerty/local macroFn IFn)
          (qwerty/set!
           macroFn
           (qwerty/go-method-call (qwerty/do) getMacro ch))))
        (qwerty/if
         (qwerty/if (qwerty/= macroFn nil) false true)
         (qwerty/do
          (qwerty/do
           (qwerty/do
            (qwerty/local ret Object)
            (qwerty/set!
             ret
             (qwerty/go-method-call
              macroFn
              invoke
              r
              (qwerty/cast Char ch)))))
          (qwerty/if
           (qwerty/go-method-call RT suppressRead)
           (qwerty/do (qwerty/comment "return null;") nil)
           (qwerty/do))
          (qwerty/if
           (qwerty/= ret r)
           (qwerty/do (qwerty/comment "continue") nil)
           (qwerty/do))
          (qwerty/do (qwerty/comment "return ret;") ret))
         (qwerty/do))
        (qwerty/if
         (qwerty/let*
          ((a__413__auto__ (qwerty/= ch "+")))
          (qwerty/if a__413__auto__ a__413__auto__ (qwerty/= ch "-")))
         (qwerty/do
          (qwerty/do
           (qwerty/do
            (qwerty/local ch2 Int)
            (qwerty/set!
             ch2
             (qwerty/go-method-call (qwerty/do) read1 r))))
          (qwerty/if
           (qwerty/. Character_isDigit (qwerty/goref ch2))
           (qwerty/do
            (qwerty/go-method-call (qwerty/do) unread r ch2)
            (qwerty/do
             (qwerty/do
              (qwerty/local n Object)
              (qwerty/set!
               n
               (qwerty/go-method-call
                (qwerty/do)
                readNumber
                r
                (qwerty/cast Char ch)))))
            (qwerty/if
             (qwerty/go-method-call RT suppressRead)
             (qwerty/do (qwerty/comment "return null;") nil)
             (qwerty/do))
            (qwerty/do (qwerty/comment "return n;") n))
           (qwerty/do))
          (qwerty/go-method-call (qwerty/do) unread r ch2))
         (qwerty/do))
        (qwerty/do
         (qwerty/do
          (qwerty/local token String)
          (qwerty/set!
           token
           (qwerty/go-method-call
            (qwerty/do)
            readToken
            r
            (qwerty/cast Char ch)))))
        (qwerty/if
         (qwerty/go-method-call RT suppressRead)
         (qwerty/do (qwerty/comment "return null;") nil)
         (qwerty/do))
        (qwerty/do
         (qwerty/comment "return interpretToken(token);")
         (qwerty/go-method-call (qwerty/do) interpretToken token))))
      end))))))


(qwerty/defgofun
 LispReader_readToken
 (r initch)
 (() (String))
 (qwerty/do
  (qwerty/do
   (qwerty/do
    (qwerty/local sb StringBuilder)
    (qwerty/set! sb (qwerty/. NewStringBuilder))))
  (qwerty/go-method-call sb append initch)
  (qwerty/do
   (qwerty/labels
    start
    (qwerty/test (qwerty/do) continue)
    (qwerty/goto end)
    continue
    (qwerty/do
     (qwerty/do
      (qwerty/do
       (qwerty/do
        (qwerty/local ch Int)
        (qwerty/set! ch (qwerty/go-method-call (qwerty/do) read1 r))))
      (qwerty/if
       (qwerty/let*
        ((a__413__auto__
          (qwerty/let*
           ((a__413__auto__
             (qwerty/=
              ch
              (qwerty/let*
               ((a__425__auto__ (qwerty/cast int "1")))
               (qwerty/* a__425__auto__ -1)))))
           (qwerty/if
            a__413__auto__
            a__413__auto__
            (qwerty/go-method-call (qwerty/do) isWhitespace ch)))))
        (qwerty/if
         a__413__auto__
         a__413__auto__
         (qwerty/go-method-call (qwerty/do) isTerminatingMacro ch)))
       (qwerty/do
        (qwerty/go-method-call (qwerty/do) unread r ch)
        (qwerty/do
         (qwerty/comment "return sb.toString();")
         (qwerty/go-method-call sb toString)))
       (qwerty/do))
      (qwerty/go-method-call sb append (qwerty/cast Char ch))))
    end))))


(qwerty/defgofun
 LispReader_readNumber
 (r initch)
 (() (Object))
 (qwerty/do
  (qwerty/do
   (qwerty/do
    (qwerty/local sb StringBuilder)
    (qwerty/set! sb (qwerty/. NewStringBuilder))))
  (qwerty/go-method-call sb append initch)
  (qwerty/do
   (qwerty/labels
    start
    (qwerty/test (qwerty/do) continue)
    (qwerty/goto end)
    continue
    (qwerty/do
     (qwerty/do
      (qwerty/do
       (qwerty/do
        (qwerty/local ch Int)
        (qwerty/set! ch (qwerty/go-method-call (qwerty/do) read1 r))))
      (qwerty/if
       (qwerty/let*
        ((a__413__auto__
          (qwerty/let*
           ((a__413__auto__
             (qwerty/=
              ch
              (qwerty/let*
               ((a__425__auto__ (qwerty/cast int "1")))
               (qwerty/* a__425__auto__ -1)))))
           (qwerty/if
            a__413__auto__
            a__413__auto__
            (qwerty/go-method-call (qwerty/do) isWhitespace ch)))))
        (qwerty/if
         a__413__auto__
         a__413__auto__
         (qwerty/go-method-call (qwerty/do) isMacro ch)))
       (qwerty/do
        (qwerty/go-method-call (qwerty/do) unread r ch)
        (qwerty/do (qwerty/comment "break") nil))
       (qwerty/do))
      (qwerty/go-method-call sb append (qwerty/cast Char ch))))
    end))
  (qwerty/do
   (qwerty/do
    (qwerty/local s String)
    (qwerty/set! s (qwerty/go-method-call sb toString))))
  (qwerty/do
   (qwerty/do
    (qwerty/local n Object)
    (qwerty/set! n (qwerty/go-method-call (qwerty/do) matchNumber s))))
  (qwerty/if
   (qwerty/= n nil)
   (qwerty/.
    user/panic
    (qwerty/.
     NewNumberFormatException
     (qwerty/let*
      ((a__416__auto__ (qwerty/cast int "Invalid number: "))
       (b__417__auto__ (qwerty/cast int s)))
      (qwerty/+ a__416__auto__ b__417__auto__))))
   (qwerty/do))
  (qwerty/do (qwerty/comment "return n;") n)))


(qwerty/defgofun
 LispReader_readUnicodeChar
 (token offset length base)
 (() (int))
 (qwerty/do
  (qwerty/if
   (qwerty/if
    (qwerty/=
     (qwerty/go-method-call token length)
     (qwerty/let*
      ((a__416__auto__ (qwerty/cast int offset))
       (b__417__auto__ (qwerty/cast int length)))
      (qwerty/+ a__416__auto__ b__417__auto__)))
    false
    true)
   (qwerty/.
    user/panic
    (qwerty/.
     NewIllegalArgumentException
     (qwerty/let*
      ((a__416__auto__
        (qwerty/cast int "Invalid unicode character: \\\\"))
       (b__417__auto__ (qwerty/cast int token)))
      (qwerty/+ a__416__auto__ b__417__auto__))))
   (qwerty/do))
  (qwerty/do (qwerty/do (qwerty/local uc Int) (qwerty/set! uc "0")))
  (qwerty/do
   (qwerty/do (qwerty/do (qwerty/local i Int) (qwerty/set! i offset)))
   (qwerty/labels
    start
    (qwerty/test
     (qwerty/let*
      ((a__414__auto__ (qwerty/cast int i))
       (b__415__auto__
        (qwerty/cast
         int
         (qwerty/let*
          ((a__416__auto__ (qwerty/cast int offset))
           (b__417__auto__ (qwerty/cast int length)))
          (qwerty/+ a__416__auto__ b__417__auto__)))))
      (qwerty/< a__414__auto__ b__415__auto__))
     continue)
    (qwerty/goto end)
    continue
    (qwerty/do
     (qwerty/do
      (qwerty/set!
       i
       (qwerty/let*
        ((a__424__auto__ (qwerty/cast int i)))
        (qwerty/+ a__424__auto__ 1)))
      i)
     (qwerty/do
      (qwerty/do
       (qwerty/do
        (qwerty/local d Int)
        (qwerty/set!
         d
         (qwerty/.
          Character_digit
          (qwerty/go-method-call token charAt i)
          (qwerty/goref base)))))
      (qwerty/if
       (qwerty/=
        d
        (qwerty/let*
         ((a__425__auto__ (qwerty/cast int "1")))
         (qwerty/* a__425__auto__ -1)))
       (qwerty/.
        user/panic
        (qwerty/.
         NewIllegalArgumentException
         (qwerty/let*
          ((a__416__auto__ (qwerty/cast int "Invalid digit: "))
           (b__417__auto__
            (qwerty/cast int (qwerty/go-method-call token charAt i))))
          (qwerty/+ a__416__auto__ b__417__auto__))))
       (qwerty/do))
      (qwerty/do
       (qwerty/set!
        uc
        (qwerty/let*
         ((a__416__auto__
           (qwerty/cast
            int
            (qwerty/let*
             ((a__418__auto__ (qwerty/cast int uc))
              (b__419__auto__ (qwerty/cast int base)))
             (qwerty/* a__418__auto__ b__419__auto__))))
          (b__417__auto__ (qwerty/cast int d)))
         (qwerty/+ a__416__auto__ b__417__auto__)))
       nil)))
    end))
  (qwerty/do
   (qwerty/comment "return (char) uc;")
   (qwerty/cast Char uc))))


(qwerty/defgofun
 LispReader_readUnicodeChar
 (r initch base length exact)
 (() (int))
 (qwerty/do
  (qwerty/do
   (qwerty/do
    (qwerty/local uc Int)
    (qwerty/set!
     uc
     (qwerty/.
      Character_digit
      (qwerty/goref initch)
      (qwerty/goref base)))))
  (qwerty/if
   (qwerty/=
    uc
    (qwerty/let*
     ((a__425__auto__ (qwerty/cast int "1")))
     (qwerty/* a__425__auto__ -1)))
   (qwerty/.
    user/panic
    (qwerty/.
     NewIllegalArgumentException
     (qwerty/let*
      ((a__416__auto__ (qwerty/cast int "Invalid digit: "))
       (b__417__auto__ (qwerty/cast int (qwerty/cast Char initch))))
      (qwerty/+ a__416__auto__ b__417__auto__))))
   (qwerty/do))
  (qwerty/do (qwerty/do (qwerty/local i Int) (qwerty/set! i "1")))
  (qwerty/do
   (qwerty/labels
    start
    (qwerty/test
     (qwerty/let*
      ((a__414__auto__ (qwerty/cast int i))
       (b__415__auto__ (qwerty/cast int length)))
      (qwerty/< a__414__auto__ b__415__auto__))
     continue)
    (qwerty/goto end)
    continue
    (qwerty/do
     (qwerty/do
      (qwerty/set!
       i
       (qwerty/let*
        ((a__424__auto__ (qwerty/cast int i)))
        (qwerty/+ a__424__auto__ 1)))
      i)
     (qwerty/do
      (qwerty/do
       (qwerty/do
        (qwerty/local ch Int)
        (qwerty/set! ch (qwerty/go-method-call (qwerty/do) read1 r))))
      (qwerty/if
       (qwerty/let*
        ((a__413__auto__
          (qwerty/let*
           ((a__413__auto__
             (qwerty/=
              ch
              (qwerty/let*
               ((a__425__auto__ (qwerty/cast int "1")))
               (qwerty/* a__425__auto__ -1)))))
           (qwerty/if
            a__413__auto__
            a__413__auto__
            (qwerty/go-method-call (qwerty/do) isWhitespace ch)))))
        (qwerty/if
         a__413__auto__
         a__413__auto__
         (qwerty/go-method-call (qwerty/do) isMacro ch)))
       (qwerty/do
        (qwerty/go-method-call (qwerty/do) unread r ch)
        (qwerty/do (qwerty/comment "break") nil))
       (qwerty/do))
      (qwerty/do
       (qwerty/do
        (qwerty/local d Int)
        (qwerty/set!
         d
         (qwerty/.
          Character_digit
          (qwerty/goref ch)
          (qwerty/goref base)))))
      (qwerty/if
       (qwerty/=
        d
        (qwerty/let*
         ((a__425__auto__ (qwerty/cast int "1")))
         (qwerty/* a__425__auto__ -1)))
       (qwerty/.
        user/panic
        (qwerty/.
         NewIllegalArgumentException
         (qwerty/let*
          ((a__416__auto__ (qwerty/cast int "Invalid digit: "))
           (b__417__auto__ (qwerty/cast int (qwerty/cast Char ch))))
          (qwerty/+ a__416__auto__ b__417__auto__))))
       (qwerty/do))
      (qwerty/do
       (qwerty/set!
        uc
        (qwerty/let*
         ((a__416__auto__
           (qwerty/cast
            int
            (qwerty/let*
             ((a__418__auto__ (qwerty/cast int uc))
              (b__419__auto__ (qwerty/cast int base)))
             (qwerty/* a__418__auto__ b__419__auto__))))
          (b__417__auto__ (qwerty/cast int d)))
         (qwerty/+ a__416__auto__ b__417__auto__)))
       nil)))
    end))
  (qwerty/if
   (qwerty/if
    (qwerty/if (qwerty/= i length) false true)
    (qwerty/if exact true false)
    false)
   (qwerty/.
    user/panic
    (qwerty/.
     NewIllegalArgumentException
     (qwerty/let*
      ((a__416__auto__
        (qwerty/cast
         int
         (qwerty/let*
          ((a__416__auto__
            (qwerty/cast
             int
             (qwerty/let*
              ((a__416__auto__
                (qwerty/cast int "Invalid character length: "))
               (b__417__auto__ (qwerty/cast int i)))
              (qwerty/+ a__416__auto__ b__417__auto__))))
           (b__417__auto__ (qwerty/cast int ", should be: ")))
          (qwerty/+ a__416__auto__ b__417__auto__))))
       (b__417__auto__ (qwerty/cast int length)))
      (qwerty/+ a__416__auto__ b__417__auto__))))
   (qwerty/do))
  (qwerty/do (qwerty/comment "return uc;") uc)))


(qwerty/defgofun
 LispReader_interpretToken
 (s)
 (() (Object))
 (qwerty/do
  (qwerty/if
   (qwerty/go-method-call s equals "nil")
   (qwerty/do (qwerty/do (qwerty/comment "return null;") nil))
   (qwerty/if
    (qwerty/go-method-call s equals "true")
    (qwerty/do
     (qwerty/do (qwerty/comment "return RT.T;") (qwerty/.- RT T)))
    (qwerty/if
     (qwerty/go-method-call s equals "false")
     (qwerty/do
      (qwerty/do (qwerty/comment "return RT.F;") (qwerty/.- RT F)))
     (qwerty/if
      (qwerty/go-method-call s equals "/")
      (qwerty/do (qwerty/do (qwerty/comment "return SLASH;") SLASH))
      (qwerty/if
       (qwerty/go-method-call s equals "clojure.core//")
       (qwerty/do
        (qwerty/do
         (qwerty/comment "return CLOJURE_SLASH;")
         CLOJURE_SLASH))
       (qwerty/do))))))
  (qwerty/do
   (qwerty/do (qwerty/local ret Object) (qwerty/set! ret nil)))
  (qwerty/do
   (qwerty/set! ret (qwerty/go-method-call (qwerty/do) matchSymbol s))
   nil)
  (qwerty/if
   (qwerty/if (qwerty/= ret nil) false true)
   (qwerty/do (qwerty/comment "return ret;") ret)
   (qwerty/do))
  (qwerty/.
   user/panic
   (qwerty/go-method-call
    Util
    runtimeException
    (qwerty/let*
     ((a__416__auto__ (qwerty/cast int "Invalid token: "))
      (b__417__auto__ (qwerty/cast int s)))
     (qwerty/+ a__416__auto__ b__417__auto__))))))


(qwerty/defgofun
 LispReader_matchSymbol
 (s)
 (() (Object))
 (qwerty/do
  (qwerty/do
   (qwerty/do
    (qwerty/local m Matcher)
    (qwerty/set! m (qwerty/go-method-call symbolPat matcher s))))
  (qwerty/if
   (qwerty/go-method-call m matches)
   (qwerty/do
    (qwerty/do
     (qwerty/do
      (qwerty/local gc Int)
      (qwerty/set! gc (qwerty/go-method-call m groupCount))))
    (qwerty/do
     (qwerty/do
      (qwerty/local ns String)
      (qwerty/set! ns (qwerty/go-method-call m group "1"))))
    (qwerty/do
     (qwerty/do
      (qwerty/local name String)
      (qwerty/set! name (qwerty/go-method-call m group "2"))))
    (qwerty/if
     (qwerty/let*
      ((a__413__auto__
        (qwerty/let*
         ((a__413__auto__
           (qwerty/if
            (qwerty/if (qwerty/= ns nil) false true)
            (qwerty/if
             (qwerty/go-method-call ns endsWith ":/")
             true
             false)
            false)))
         (qwerty/if
          a__413__auto__
          a__413__auto__
          (qwerty/go-method-call name endsWith ":")))))
      (qwerty/if
       a__413__auto__
       a__413__auto__
       (qwerty/if
        (qwerty/=
         (qwerty/go-method-call s indexOf "::" "1")
         (qwerty/let*
          ((a__425__auto__ (qwerty/cast int "1")))
          (qwerty/* a__425__auto__ -1)))
        false
        true)))
     (qwerty/do (qwerty/comment "return null;") nil)
     (qwerty/do))
    (qwerty/if
     (qwerty/go-method-call s startsWith "::")
     (qwerty/do
      (qwerty/do
       (qwerty/do
        (qwerty/local ks Symbol)
        (qwerty/set!
         ks
         (qwerty/.
          Symbol_intern
          (qwerty/go-method-call s substring "2")))))
      (qwerty/do
       (qwerty/do
        (qwerty/local kns Namespace)
        (qwerty/set! kns (qwerty/do))))
      (qwerty/if
       (qwerty/if (qwerty/= (qwerty/.- ks ns) nil) false true)
       (qwerty/do
        (qwerty/set!
         kns
         (qwerty/go-method-call Compiler namespaceFor ks))
        nil)
       (qwerty/do
        (qwerty/set! kns (qwerty/go-method-call Compiler currentNS))
        nil))
      (qwerty/if
       (qwerty/if (qwerty/= kns nil) false true)
       (qwerty/do
        (qwerty/comment
         "return Keyword.intern(kns.name.name, ks.name);")
        (qwerty/.
         Keyword_intern
         (qwerty/.- (qwerty/.- kns name) name)
         (qwerty/.- ks name)))
       (qwerty/do (qwerty/comment "return null;") nil)))
     (qwerty/do))
    (qwerty/do
     (qwerty/do
      (qwerty/local isKeyword Boolean)
      (qwerty/set!
       isKeyword
       (qwerty/= (qwerty/go-method-call s charAt "0") ":"))))
    (qwerty/do
     (qwerty/do
      (qwerty/local sym Symbol)
      (qwerty/set!
       sym
       (qwerty/.
        Symbol_intern
        (qwerty/go-method-call
         s
         substring
         (qwerty/if isKeyword "1" "0"))))))
    (qwerty/if
     isKeyword
     (qwerty/do
      (qwerty/comment "return Keyword.intern(sym);")
      (qwerty/. Keyword_intern (qwerty/goref sym)))
     (qwerty/do))
    (qwerty/do (qwerty/comment "return sym;") sym))
   (qwerty/do))
  (qwerty/do (qwerty/comment "return null;") nil)))


(qwerty/defgofun
 LispReader_matchNumber
 (s)
 (() (Object))
 (qwerty/do
  (qwerty/do
   (qwerty/do
    (qwerty/local m Matcher)
    (qwerty/set! m (qwerty/go-method-call intPat matcher s))))
  (qwerty/if
   (qwerty/go-method-call m matches)
   (qwerty/do
    (qwerty/if
     (qwerty/if
      (qwerty/= (qwerty/go-method-call m group "2") nil)
      false
      true)
     (qwerty/do
      (qwerty/if
       (qwerty/if
        (qwerty/= (qwerty/go-method-call m group "8") nil)
        false
        true)
       (qwerty/do
        (qwerty/comment "return BigInt.ZERO;")
        (qwerty/.- BigInt ZERO))
       (qwerty/do))
      (qwerty/do
       (qwerty/comment "return Numbers.num(0);")
       (qwerty/go-method-call Numbers num "0")))
     (qwerty/do))
    (qwerty/do
     (qwerty/do
      (qwerty/local negate Boolean)
      (qwerty/set!
       negate
       (qwerty/go-method-call
        (qwerty/go-method-call m group "1")
        equals
        "-"))))
    (qwerty/do
     (qwerty/do (qwerty/local n String) (qwerty/set! n (qwerty/do))))
    (qwerty/do
     (qwerty/do (qwerty/local radix Int) (qwerty/set! radix "10")))
    (qwerty/if
     (qwerty/if
      (qwerty/=
       (qwerty/do
        (qwerty/set! n (qwerty/go-method-call m group "3"))
        nil)
       nil)
      false
      true)
     (qwerty/do (qwerty/set! radix "10") nil)
     (qwerty/if
      (qwerty/if
       (qwerty/=
        (qwerty/do
         (qwerty/set! n (qwerty/go-method-call m group "4"))
         nil)
        nil)
       false
       true)
      (qwerty/do (qwerty/set! radix "16") nil)
      (qwerty/if
       (qwerty/if
        (qwerty/=
         (qwerty/do
          (qwerty/set! n (qwerty/go-method-call m group "5"))
          nil)
         nil)
        false
        true)
       (qwerty/do (qwerty/set! radix "8") nil)
       (qwerty/if
        (qwerty/if
         (qwerty/=
          (qwerty/do
           (qwerty/set! n (qwerty/go-method-call m group "7"))
           nil)
          nil)
         false
         true)
        (qwerty/do
         (qwerty/set!
          radix
          (qwerty/.
           Integer_parseInt
           (qwerty/go-method-call m group "6")))
         nil)
        (qwerty/do)))))
    (qwerty/if
     (qwerty/= n nil)
     (qwerty/do (qwerty/comment "return null;") nil)
     (qwerty/do))
    (qwerty/do
     (qwerty/do
      (qwerty/local bn BigInteger)
      (qwerty/set!
       bn
       (qwerty/.
        NewBigInteger
        (qwerty/goref n)
        (qwerty/goref radix)))))
    (qwerty/if
     negate
     (qwerty/do (qwerty/set! bn (qwerty/go-method-call bn negate)) nil)
     (qwerty/do))
    (qwerty/if
     (qwerty/if
      (qwerty/= (qwerty/go-method-call m group "8") nil)
      false
      true)
     (qwerty/do
      (qwerty/comment "return BigInt.fromBigInteger(bn);")
      (qwerty/go-method-call BigInt fromBigInteger bn))
     (qwerty/do))
    (qwerty/do
     (qwerty/comment
      "return bn.bitLength() < 64 ? Numbers.num(bn.longValue()) : BigInt.fromBigInteger(bn);")
     (qwerty/if
      (qwerty/let*
       ((a__414__auto__
         (qwerty/cast int (qwerty/go-method-call bn bitLength)))
        (b__415__auto__ (qwerty/cast int "64")))
       (qwerty/< a__414__auto__ b__415__auto__))
      (qwerty/go-method-call
       Numbers
       num
       (qwerty/go-method-call bn longValue))
      (qwerty/go-method-call BigInt fromBigInteger bn))))
   (qwerty/do))
  (qwerty/do
   (qwerty/set! m (qwerty/go-method-call floatPat matcher s))
   nil)
  (qwerty/if
   (qwerty/go-method-call m matches)
   (qwerty/do
    (qwerty/if
     (qwerty/if
      (qwerty/= (qwerty/go-method-call m group "4") nil)
      false
      true)
     (qwerty/do
      (qwerty/comment "return new BigDecimal(m.group(1));")
      (qwerty/. NewBigDecimal (qwerty/go-method-call m group "1")))
     (qwerty/do))
    (qwerty/do
     (qwerty/comment "return Double.parseDouble(s);")
     (qwerty/go-method-call Double parseDouble s)))
   (qwerty/do))
  (qwerty/do
   (qwerty/set! m (qwerty/go-method-call ratioPat matcher s))
   nil)
  (qwerty/if
   (qwerty/go-method-call m matches)
   (qwerty/do
    (qwerty/do
     (qwerty/do
      (qwerty/local numerator String)
      (qwerty/set! numerator (qwerty/go-method-call m group "1"))))
    (qwerty/if
     (qwerty/go-method-call numerator startsWith "+")
     (qwerty/do
      (qwerty/set!
       numerator
       (qwerty/go-method-call numerator substring "1"))
      nil)
     (qwerty/do))
    (qwerty/do
     (qwerty/comment
      "return Numbers.divide(Numbers.reduceBigInt(BigInt.fromBigInteger(new BigInteger(numerator))), Numbers.reduceBigInt(BigInt.fromBigInteger(new BigInteger(m.group(2)))));")
     (qwerty/go-method-call
      Numbers
      divide
      (qwerty/go-method-call
       Numbers
       reduceBigInt
       (qwerty/go-method-call
        BigInt
        fromBigInteger
        (qwerty/. NewBigInteger (qwerty/goref numerator))))
      (qwerty/go-method-call
       Numbers
       reduceBigInt
       (qwerty/go-method-call
        BigInt
        fromBigInteger
        (qwerty/.
         NewBigInteger
         (qwerty/go-method-call m group "2")))))))
   (qwerty/do))
  (qwerty/do (qwerty/comment "return null;") nil)))


(qwerty/defgofun
 LispReader_getMacro
 (ch)
 (() (IFn))
 (qwerty/do
  (qwerty/if
   (qwerty/let*
    ((a__414__auto__ (qwerty/cast int ch))
     (b__415__auto__ (qwerty/cast int (qwerty/.- macros length))))
    (qwerty/< a__414__auto__ b__415__auto__))
   (qwerty/do
    (qwerty/comment "return macros[ch];")
    (qwerty/aget (qwerty/goref macros) ch))
   (qwerty/do))
  (qwerty/do (qwerty/comment "return null;") nil)))


(qwerty/defgofun
 LispReader_isMacro
 (ch)
 (() (bool))
 (qwerty/do
  (qwerty/do
   (qwerty/comment
    "return (ch < macros.length && macros[ch] != null);")
   (qwerty/if
    (qwerty/let*
     ((a__414__auto__ (qwerty/cast int ch))
      (b__415__auto__ (qwerty/cast int (qwerty/.- macros length))))
     (qwerty/< a__414__auto__ b__415__auto__))
    (qwerty/if
     (qwerty/if
      (qwerty/= (qwerty/aget (qwerty/goref macros) ch) nil)
      false
      true)
     true
     false)
    false))))


(qwerty/defgofun
 LispReader_isTerminatingMacro
 (ch)
 (() (bool))
 (qwerty/do
  (qwerty/do
   (qwerty/comment
    "return (ch != '#' && ch != '\\'' && ch != '%' && isMacro(ch));")
   (qwerty/if
    (qwerty/if
     (qwerty/if
      (qwerty/if (qwerty/= ch "#") false true)
      (qwerty/if (qwerty/if (qwerty/= ch "\\'") false true) true false)
      false)
     (qwerty/if (qwerty/if (qwerty/= ch "%") false true) true false)
     false)
    (qwerty/if
     (qwerty/go-method-call (qwerty/do) isMacro ch)
     true
     false)
    false))))


(qwerty/struct RegexReader)


(qwerty/defgofun
 InitRegexReader
 (G__544)
 ((*RegexReader) ())
 (qwerty/do))


(qwerty/godef stringrdr (qwerty/. NewStringReader))



(user/method-decl)



(qwerty/struct StringReader)


(qwerty/defgofun
 InitStringReader
 (G__545)
 ((*StringReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/struct CommentReader)


(qwerty/defgofun
 InitCommentReader
 (G__546)
 ((*CommentReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/struct DiscardReader)


(qwerty/defgofun
 InitDiscardReader
 (G__547)
 ((*DiscardReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/struct WrappingReader sym Symbol)


(qwerty/defgofun
 InitWrappingReader
 (G__548)
 ((*WrappingReader) ())
 (qwerty/do (qwerty/set! (qwerty/.- G__548 sym) (qwerty/do))))


(qwerty/defgofun
 NewWrappingReader
 ()
 (() (*WrappingReader))
 (qwerty/let*
  ((i__388__auto__ (qwerty/new WrappingReader)))
  (qwerty/. InitWrappingReader i__388__auto__)
  i__388__auto__))


(user/method-decl)



(qwerty/struct DeprecatedWrappingReader sym Symbol macro String)


(qwerty/defgofun
 InitDeprecatedWrappingReader
 (G__549)
 ((*DeprecatedWrappingReader) ())
 (qwerty/do
  (qwerty/set! (qwerty/.- G__549 sym) (qwerty/do))
  (qwerty/set! (qwerty/.- G__549 macro) (qwerty/do))))


(qwerty/defgofun
 NewDeprecatedWrappingReader
 ()
 (() (*DeprecatedWrappingReader))
 (qwerty/let*
  ((i__388__auto__ (qwerty/new DeprecatedWrappingReader)))
  (qwerty/. InitDeprecatedWrappingReader i__388__auto__)
  i__388__auto__))


(user/method-decl)



(qwerty/struct VarReader)


(qwerty/defgofun InitVarReader (G__550) ((*VarReader) ()) (qwerty/do))


(user/method-decl)



(qwerty/struct DispatchReader)


(qwerty/defgofun
 InitDispatchReader
 (G__551)
 ((*DispatchReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/struct CharacterReader)


(qwerty/defgofun
 InitCharacterReader
 (G__552)
 ((*CharacterReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/struct ListReader)


(qwerty/defgofun InitListReader (G__553) ((*ListReader) ()) (qwerty/do))


(user/method-decl)



(qwerty/struct EvalReader)


(qwerty/defgofun InitEvalReader (G__554) ((*EvalReader) ()) (qwerty/do))


(user/method-decl)



(qwerty/struct VectorReader)


(qwerty/defgofun
 InitVectorReader
 (G__555)
 ((*VectorReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/struct MapReader)


(qwerty/defgofun InitMapReader (G__556) ((*MapReader) ()) (qwerty/do))


(user/method-decl)



(qwerty/struct SetReader)


(qwerty/defgofun InitSetReader (G__557) ((*SetReader) ()) (qwerty/do))


(user/method-decl)



(qwerty/struct UnmatchedDelimiterReader)


(qwerty/defgofun
 InitUnmatchedDelimiterReader
 (G__558)
 ((*UnmatchedDelimiterReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/struct UnreadableReader)


(qwerty/defgofun
 InitUnreadableReader
 (G__559)
 ((*UnreadableReader) ())
 (qwerty/do))


(user/method-decl)



(qwerty/defgofun
 LispReader_readDelimitedList
 (delim r isRecursive)
 (() (List))
 (qwerty/do
  (qwerty/do
   (qwerty/do
    (qwerty/local firstline Int)
    (qwerty/set!
     firstline
     (qwerty/if
      (user/foo)
      (qwerty/go-method-call
       (qwerty/cast LineNumberingPushbackReader r)
       getLineNumber)
      (qwerty/let*
       ((a__425__auto__ (qwerty/cast int "1")))
       (qwerty/* a__425__auto__ -1))))))
  (qwerty/do
   (qwerty/do
    (qwerty/local a ArrayList)
    (qwerty/set! a (qwerty/. NewArrayList))))
  (qwerty/do
   (qwerty/labels
    start
    (qwerty/test (qwerty/do) continue)
    (qwerty/goto end)
    continue
    (qwerty/do
     (qwerty/do
      (qwerty/do
       (qwerty/do
        (qwerty/local ch Int)
        (qwerty/set! ch (qwerty/go-method-call (qwerty/do) read1 r))))
      (qwerty/labels
       start
       (qwerty/test
        (qwerty/go-method-call (qwerty/do) isWhitespace ch)
        continue)
       (qwerty/goto end)
       continue
       (qwerty/do
        (qwerty/do
         (qwerty/set! ch (qwerty/go-method-call (qwerty/do) read1 r))
         nil))
       end)
      (qwerty/if
       (qwerty/=
        ch
        (qwerty/let*
         ((a__425__auto__ (qwerty/cast int "1")))
         (qwerty/* a__425__auto__ -1)))
       (qwerty/do
        (qwerty/if
         (qwerty/let*
          ((a__414__auto__ (qwerty/cast int firstline))
           (b__415__auto__ (qwerty/cast int "0")))
          (qwerty/< a__414__auto__ b__415__auto__))
         (qwerty/.
          user/panic
          (qwerty/go-method-call
           Util
           runtimeException
           "EOF while reading"))
         (qwerty/.
          user/panic
          (qwerty/go-method-call
           Util
           runtimeException
           (qwerty/let*
            ((a__416__auto__
              (qwerty/cast int "EOF while reading, starting at line "))
             (b__417__auto__ (qwerty/cast int firstline)))
            (qwerty/+ a__416__auto__ b__417__auto__))))))
       (qwerty/do))
      (qwerty/if
       (qwerty/= ch delim)
       (qwerty/do (qwerty/comment "break") nil)
       (qwerty/do))
      (qwerty/do
       (qwerty/do
        (qwerty/local macroFn IFn)
        (qwerty/set!
         macroFn
         (qwerty/go-method-call (qwerty/do) getMacro ch))))
      (qwerty/if
       (qwerty/if (qwerty/= macroFn nil) false true)
       (qwerty/do
        (qwerty/do
         (qwerty/do
          (qwerty/local mret Object)
          (qwerty/set!
           mret
           (qwerty/go-method-call
            macroFn
            invoke
            r
            (qwerty/cast Char ch)))))
        (qwerty/if
         (qwerty/if (qwerty/= mret r) false true)
         (qwerty/go-method-call a add mret)
         (qwerty/do)))
       (qwerty/do
        (qwerty/go-method-call (qwerty/do) unread r ch)
        (qwerty/do
         (qwerty/do
          (qwerty/local o Object)
          (qwerty/set!
           o
           (qwerty/go-method-call
            (qwerty/do)
            read
            r
            true
            nil
            isRecursive))))
        (qwerty/if
         (qwerty/if (qwerty/= o r) false true)
         (qwerty/go-method-call a add o)
         (qwerty/do))))))
    end))
  (qwerty/do (qwerty/comment "return a;") a)))


(qwerty/struct CtorReader)


(qwerty/defgofun InitCtorReader (G__560) ((*CtorReader) ()) (qwerty/do))


(user/method-decl)


(user/method-decl)


(user/method-decl)




