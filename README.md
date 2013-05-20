# Qwerty

A lisp on the Go runtime. Compiles to Go source.

The compiler is written in Clojure.

Largely abandoned because I don't think Go's reflection library can
delivery good interop for an interpreter.

## Compilation

One Qwerty source file compiles to one Go source file. Qwerty package
structure is the same as Go package structure. Qwerty top level forms
mostly compile in to static init fuctions (init() in Go). Qwerty
ensures they are run in order for a file, but among the files in a
package the inits run in an arbitrary order. 

## Todo

missing an interpreter
missing macros (for which I want an interpreter)

![no-erlangs](http://members.iinet.net.au/~clark/images/ErlangMeter3MP.jpg)
