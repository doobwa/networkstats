# A few additional functions being tested

changestatwrap <- function(ntoggles,tails,heads,nw,model) {
  z <- .C("ChangeStats", as.integer(ntoggles), as.integer(tails), as.integer(heads),
          nw, model,  PACKAGE="ergm")
  return(z)
}

