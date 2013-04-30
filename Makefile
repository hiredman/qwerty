stdliblispfiles=$(wildcard src/qwerty/lisp/*.q)
stdlibgofiles=$(stdliblispfiles:%.q=%.go)
stdlibenvfiles=$(stdliblispfiles:%.q=%.env)

test: foo
	./foo

foo: foo.go
	GOPATH=$$PWD go build foo.go

repl: repl.go
	GOPATH=$$PWD go build repl.go

.compiler: qwerty.clj alpha.clj Makefile expand.clj free.clj var.clj
	touch .compiler

#.compiler
%.go: %.q compilation-env .compiler
	./qwerty.clj < $< > /tmp/bar.go
	gofmt /tmp/bar.go > /tmp/foo.go
	cp /tmp/foo.go $@
	cp compilation-env $(@:%.go=%.env)

%.env: %.go
	touch $@

compilation-env: ${stdlibenvfiles}
	find ${stdlibenvfiles} -exec cat {} \; > compilation-env

stdlib: ${stdlibgofiles}
	GOPATH=$$PWD go install qwerty/lisp
	touch stdlib

clean:
	rm -rf ${stdlibgofiles}
	rm -rf ${stdlibenvfiles}
	rm -rf compilation-env
	rm -rf pkg
	rm -rf foo
	rm -rf repl
	rm -rf LispReader.q

LispReader.q: LispReader.java java-to-go.clj
	./java-to-go.clj < LispReader.java > LispReader.q

LispReader: LispReader.go
	GOPATH=$$PWD go build LispReader.go

