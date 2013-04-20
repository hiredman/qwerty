stdliblispfiles=$(wildcard src/qwerty/lisp/*.lisp)
stdlibgofiles=$(stdliblispfiles:%.lisp=%.go)

test: foo
	./foo
foo: foo.go
	GOPATH=$$PWD go build foo.go

%.go: %.lisp qwerty.clj alpha.clj Makefile expand.clj free.clj stdlib
	./qwerty.clj < $< > $@
	gofmt $@ > /tmp/foo.go
	cp /tmp/foo.go $@

stdlib: ${stdlibgofiles}
	GOPATH=$$PWD go install qwerty/lisp
	touch stdlib
