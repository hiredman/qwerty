test: foo
	./foo
foo: foo.go
	go build foo.go
foo.go: foo.lisp qwerty.clj Makefile
	./qwerty.clj < foo.lisp > foo.go
	gofmt foo.go > /tmp/foo.go
	cp /tmp/foo.go foo.go
