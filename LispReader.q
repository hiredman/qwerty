(qwerty/package main)
(qwerty/do
 (qwerty/do
  (qwerty/struct LispReader)
  (qwerty/defgofun
   InitLispReader
   (G__484)
   ((*LispReader) ())
   (qwerty/do))
  (qwerty/do
   (qwerty/local QUOTE Symbol)
   (qwerty/set! (qwerty/goref QUOTE) (qwerty/. Symbol_intern "quote")))
  (qwerty/do
   (qwerty/local THE_VAR Symbol)
   (qwerty/set! (qwerty/goref THE_VAR) (qwerty/. Symbol_intern "var")))
  (qwerty/do
   (qwerty/local UNQUOTE Symbol)
   (qwerty/set!
    (qwerty/goref UNQUOTE)
    (qwerty/. Symbol_intern "clojure.core" "unquote")))
  (qwerty/do
   (qwerty/local UNQUOTE_SPLICING Symbol)
   (qwerty/set!
    (qwerty/goref UNQUOTE_SPLICING)
    (qwerty/. Symbol_intern "clojure.core" "unquote-splicing")))
  (qwerty/do
   (qwerty/local CONCAT Symbol)
   (qwerty/set!
    (qwerty/goref CONCAT)
    (qwerty/. Symbol_intern "clojure.core" "concat")))
  (qwerty/do
   (qwerty/local SEQ Symbol)
   (qwerty/set!
    (qwerty/goref SEQ)
    (qwerty/. Symbol_intern "clojure.core" "seq")))
  (qwerty/do
   (qwerty/local LIST Symbol)
   (qwerty/set!
    (qwerty/goref LIST)
    (qwerty/. Symbol_intern "clojure.core" "list")))
  (qwerty/do
   (qwerty/local APPLY Symbol)
   (qwerty/set!
    (qwerty/goref APPLY)
    (qwerty/. Symbol_intern "clojure.core" "apply")))
  (qwerty/do
   (qwerty/local HASHMAP Symbol)
   (qwerty/set!
    (qwerty/goref HASHMAP)
    (qwerty/. Symbol_intern "clojure.core" "hash-map")))
  (qwerty/do
   (qwerty/local HASHSET Symbol)
   (qwerty/set!
    (qwerty/goref HASHSET)
    (qwerty/. Symbol_intern "clojure.core" "hash-set")))
  (qwerty/do
   (qwerty/local VECTOR Symbol)
   (qwerty/set!
    (qwerty/goref VECTOR)
    (qwerty/. Symbol_intern "clojure.core" "vector")))
  (qwerty/do
   (qwerty/local WITH_META Symbol)
   (qwerty/set!
    (qwerty/goref WITH_META)
    (qwerty/. Symbol_intern "clojure.core" "with-meta")))
  (qwerty/do
   (qwerty/local META Symbol)
   (qwerty/set!
    (qwerty/goref META)
    (qwerty/. Symbol_intern "clojure.core" "meta")))
  (qwerty/do
   (qwerty/local DEREF Symbol)
   (qwerty/set!
    (qwerty/goref DEREF)
    (qwerty/. Symbol_intern "clojure.core" "deref")))
  (qwerty/do
   (qwerty/local UNKNOWN Keyword)
   (qwerty/set!
    (qwerty/goref UNKNOWN)
    (qwerty/. Keyword_intern nil "unknown")))
  (qwerty/do
   (qwerty/local macros IFn)
   (qwerty/set! (qwerty/goref macros) (qwerty/make "[]IFn 256")))
  (qwerty/do
   (qwerty/local dispatchMacros IFn)
   (qwerty/set!
    (qwerty/goref dispatchMacros)
    (qwerty/make "[]IFn 256")))
  (qwerty/do
   (qwerty/local symbolPat Pattern)
   (qwerty/set!
    (qwerty/goref symbolPat)
    (qwerty/.
     Pattern_compile
     "[:]?([\\\\D&&[^/]].*/)?([\\\\D&&[^/]][^/]*)")))
  (qwerty/do
   (qwerty/local intPat Pattern)
   (qwerty/set!
    (qwerty/goref intPat)
    (qwerty/.
     Pattern_compile
     "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")))
  (qwerty/do
   (qwerty/local ratioPat Pattern)
   (qwerty/set!
    (qwerty/goref ratioPat)
    (qwerty/. Pattern_compile "([-+]?[0-9]+)/([0-9]+)")))
  (qwerty/do
   (qwerty/local floatPat Pattern)
   (qwerty/set!
    (qwerty/goref floatPat)
    (qwerty/.
     Pattern_compile
     "([-+]?[0-9]+(\\\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")))
  (qwerty/do
   (qwerty/local SLASH Symbol)
   (qwerty/set! (qwerty/goref SLASH) (qwerty/. Symbol_intern "/")))
  (qwerty/do
   (qwerty/local CLOJURE_SLASH Symbol)
   (qwerty/set!
    (qwerty/goref CLOJURE_SLASH)
    (qwerty/. Symbol_intern "clojure.core" "/")))
  (qwerty/do
   (qwerty/local GENSYM_ENV Var)
   (qwerty/set!
    (qwerty/goref GENSYM_ENV)
    (qwerty/go-method-call (qwerty/. Var_create nil) setDynamic)))
  (qwerty/do
   (qwerty/local ARG_ENV Var)
   (qwerty/set!
    (qwerty/goref ARG_ENV)
    (qwerty/go-method-call (qwerty/. Var_create nil) setDynamic)))
  (qwerty/do
   (qwerty/local ctorReader IFn)
   (qwerty/set! (qwerty/goref ctorReader) (qwerty/. NewCtorReader)))
  (qwerty/defgofun
   user/init
   ()
   (() ())
   (qwerty/do
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewStringReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewCommentReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewWrappingReader (qwerty/goref QUOTE)))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewWrappingReader (qwerty/goref DEREF)))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewListReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewUnmatchedDelimiterReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewCharacterReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewDispatchReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewVarReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewRegexReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewUnreadableReader))
    (qwerty/set!
     (qwerty/goref (qwerty/array-access))
     (qwerty/. NewDiscardReader))))
  (qwerty/defgofun
   LispReader_isWhitespace
   (ch)
   (() (bool))
   (qwerty/do
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     (qwerty/let*
      ((a__417__auto__
        (qwerty/. Character_isWhitespace (qwerty/goref ch))))
      (qwerty/if a__417__auto__ a__417__auto__ (qwerty/= ch ","))))))
  (qwerty/defgofun
   LispReader_unread
   (r ch)
   (() ())
   (qwerty/do (qwerty/if)))
  (qwerty/do
   (qwerty/struct ReaderException line Int column Int)
   (qwerty/defgofun
    InitReaderException
    (G__485)
    ((*ReaderException) ())
    (qwerty/do
     (qwerty/set! (qwerty/.- G__485 line) (qwerty/do))
     (qwerty/set! (qwerty/.- G__485 column) (qwerty/do))))
   (qwerty/defgofun
    NewReaderException
    ()
    (() (*ReaderException))
    (qwerty/let*
     ((i__396__auto__ (qwerty/new ReaderException)))
     (qwerty/. InitReaderException i__396__auto__)
     i__396__auto__)))
  (qwerty/defgofun
   LispReader_read1
   (r)
   (() (int))
   (qwerty/do (qwerty/try)))
  (qwerty/defgofun
   LispReader_read
   (r eofIsError eofValue isRecursive)
   (() (Object))
   (qwerty/do (qwerty/if) (qwerty/try)))
  (qwerty/defgofun
   LispReader_readToken
   (r initch)
   (() (String))
   (qwerty/do
    (qwerty/variable)
    (qwerty/go-method-call sb append initch)
    (qwerty/for)))
  (qwerty/defgofun
   LispReader_readNumber
   (r initch)
   (() (Object))
   (qwerty/do
    (qwerty/variable)
    (qwerty/go-method-call sb append initch)
    (qwerty/for)
    (qwerty/variable)
    (qwerty/variable)
    (qwerty/if)
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     n)))
  (qwerty/defgofun
   LispReader_readUnicodeChar
   (token offset length base)
   (() (int))
   (qwerty/do
    (qwerty/if)
    (qwerty/variable)
    (qwerty/for)
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     (qwerty/cast Char uc))))
  (qwerty/defgofun
   LispReader_readUnicodeChar
   (r initch base length exact)
   (() (int))
   (qwerty/do
    (qwerty/variable)
    (qwerty/if)
    (qwerty/variable)
    (qwerty/for)
    (qwerty/if)
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     uc)))
  (qwerty/defgofun
   LispReader_interpretToken
   (s)
   (() (Object))
   (qwerty/do
    (qwerty/if)
    (qwerty/variable)
    (qwerty/set!
     (qwerty/goref ret)
     (qwerty/go-method-call (qwerty/do) matchSymbol s))
    (qwerty/if)
    (qwerty/throw)))
  (qwerty/defgofun
   LispReader_matchSymbol
   (s)
   (() (Object))
   (qwerty/do
    (qwerty/variable)
    (qwerty/if)
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     nil)))
  (qwerty/defgofun
   LispReader_matchNumber
   (s)
   (() (Object))
   (qwerty/do
    (qwerty/variable)
    (qwerty/if)
    (qwerty/set!
     (qwerty/goref m)
     (qwerty/go-method-call floatPat matcher s))
    (qwerty/if)
    (qwerty/set!
     (qwerty/goref m)
     (qwerty/go-method-call ratioPat matcher s))
    (qwerty/if)
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     nil)))
  (qwerty/defgofun
   LispReader_getMacro
   (ch)
   (() (IFn))
   (qwerty/do
    (qwerty/if)
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     nil)))
  (qwerty/defgofun
   LispReader_isMacro
   (ch)
   (() (bool))
   (qwerty/do
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     (qwerty/and
      (qwerty/less ch (qwerty/.- macros length))
      (qwerty/notEquals (qwerty/array-access) nil)))))
  (qwerty/defgofun
   LispReader_isTerminatingMacro
   (ch)
   (() (bool))
   (qwerty/do
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     (qwerty/and
      (qwerty/and
       (qwerty/and
        (qwerty/notEquals ch "#")
        (qwerty/notEquals ch "\\'"))
       (qwerty/notEquals ch "%"))
      (qwerty/go-method-call (qwerty/do) isMacro ch)))))
  (qwerty/do
   (qwerty/struct RegexReader)
   (qwerty/defgofun
    InitRegexReader
    (G__486)
    ((*RegexReader) ())
    (qwerty/do))
   (qwerty/do
    (qwerty/local stringrdr StringReader)
    (qwerty/set! (qwerty/goref stringrdr) (qwerty/. NewStringReader)))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct StringReader)
   (qwerty/defgofun
    InitStringReader
    (G__487)
    ((*StringReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct CommentReader)
   (qwerty/defgofun
    InitCommentReader
    (G__488)
    ((*CommentReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct DiscardReader)
   (qwerty/defgofun
    InitDiscardReader
    (G__489)
    ((*DiscardReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct WrappingReader sym Symbol)
   (qwerty/defgofun
    InitWrappingReader
    (G__490)
    ((*WrappingReader) ())
    (qwerty/do (qwerty/set! (qwerty/.- G__490 sym) (qwerty/do))))
   (qwerty/defgofun
    NewWrappingReader
    ()
    (() (*WrappingReader))
    (qwerty/let*
     ((i__396__auto__ (qwerty/new WrappingReader)))
     (qwerty/. InitWrappingReader i__396__auto__)
     i__396__auto__))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct DeprecatedWrappingReader sym Symbol macro String)
   (qwerty/defgofun
    InitDeprecatedWrappingReader
    (G__491)
    ((*DeprecatedWrappingReader) ())
    (qwerty/do
     (qwerty/set! (qwerty/.- G__491 sym) (qwerty/do))
     (qwerty/set! (qwerty/.- G__491 macro) (qwerty/do))))
   (qwerty/defgofun
    NewDeprecatedWrappingReader
    ()
    (() (*DeprecatedWrappingReader))
    (qwerty/let*
     ((i__396__auto__ (qwerty/new DeprecatedWrappingReader)))
     (qwerty/. InitDeprecatedWrappingReader i__396__auto__)
     i__396__auto__))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct VarReader)
   (qwerty/defgofun
    InitVarReader
    (G__492)
    ((*VarReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct DispatchReader)
   (qwerty/defgofun
    InitDispatchReader
    (G__493)
    ((*DispatchReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct CharacterReader)
   (qwerty/defgofun
    InitCharacterReader
    (G__494)
    ((*CharacterReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct ListReader)
   (qwerty/defgofun
    InitListReader
    (G__495)
    ((*ListReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct EvalReader)
   (qwerty/defgofun
    InitEvalReader
    (G__496)
    ((*EvalReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct VectorReader)
   (qwerty/defgofun
    InitVectorReader
    (G__497)
    ((*VectorReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct MapReader)
   (qwerty/defgofun
    InitMapReader
    (G__498)
    ((*MapReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct SetReader)
   (qwerty/defgofun
    InitSetReader
    (G__499)
    ((*SetReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct UnmatchedDelimiterReader)
   (qwerty/defgofun
    InitUnmatchedDelimiterReader
    (G__500)
    ((*UnmatchedDelimiterReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/do
   (qwerty/struct UnreadableReader)
   (qwerty/defgofun
    InitUnreadableReader
    (G__501)
    ((*UnreadableReader) ())
    (qwerty/do))
   (user/method-decl))
  (qwerty/defgofun
   LispReader_readDelimitedList
   (delim r isRecursive)
   (() (List))
   (qwerty/do
    (qwerty/variable)
    (qwerty/variable)
    (qwerty/for)
    (qwerty/do
     (qwerty/comment "return" (clojure.core/str user/cu))
     a)))
  (qwerty/do
   (qwerty/struct CtorReader)
   (qwerty/defgofun
    InitCtorReader
    (G__502)
    ((*CtorReader) ())
    (qwerty/do))
   (user/method-decl)
   (user/method-decl)
   (user/method-decl))))
