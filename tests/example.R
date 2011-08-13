dyn.load('tests/example.so')
x <- .Call("create",list("blah",1:5))
.Call("get",x)
.Call("set",x,"new words")
.Call("get",x)
x <- NULL

x <- .Call("createFoo","blah")
.Call("getFooVal",x)
