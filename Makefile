test: foo
	./foo
foo: foo.go
	go build foo.go
foo.go: foo.lisp qwerty.clj alpha.clj Makefile expand.clj
	./qwerty.clj < foo.lisp > foo.go
	gofmt foo.go > /tmp/foo.go
	cp /tmp/foo.go foo.go
