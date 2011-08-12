require(mypackage)

rcpp_hello_world <- function(){
	.Call( "rcpp_hello_world", PACKAGE = "mypackage" )
}

rcpp_hello_world()

show(World)
b <- new(World)
b$greet()
b$set("hello everybody")
b$greet()
rm(b)

show(ChangescoreNetwork)
a <- new(ChangescoreNetwork)
a$greet()
a$set("hello everybody")
a$greet()
rm(a)

