test: foo
	./foo
foo: foo.go
	go build foo.go

# foo.go: foo.lisp qwerty.clj alpha.clj Makefile expand.clj free.clj
# 	./qwerty.clj < foo.lisp > foo.go
# 	gofmt foo.go > /tmp/foo.go
# 	cp /tmp/foo.go foo.go
# qwerty.go: qwerty.lisp qwerty.clj alpha.clj Makefile expand.clj free.clj
# 	./qwerty.clj < qwerty.lisp > qwerty.go
# 	gofmt qwerty.go > /tmp/qwerty.go
# 	cp /tmp/qwerty.go qwerty.go

%.go: %.lisp qwerty.clj alpha.clj Makefile expand.clj free.clj
	./qwerty.clj < $< > $@
	gofmt $@ > /tmp/foo.go
	cp /tmp/foo.go $@
