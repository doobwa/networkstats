# Author: Christian Gunning

# Grabbed from this thread:
# http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2011-March/001968.html
# Analogous to cxxfunction for C++ code that defines modules rather
# than simply a function.
modfunction <- function ( modname, file_, plugin = "Rcpp", 
    includes = "", settings = getPlugin(plugin), ..., verbose = FALSE) 
{
    ## tempfile
    f <- basename(tempfile())
    settings_includes <- if (is.null(settings$includes)) 
                                ""
                        else paste(settings$includes, collapse = "\n")
    src <- paste(readLines(file_), collapse='\n')
    ## code
    code <- sprintf("// includes from the plugin\n%s\n\n// user includes\n%s\n\n %s",
        settings_includes, paste(includes, collapse = "\n"), src)
    if (!is.null(env <- settings$env)) {
        do.call(Sys.setenv, env)
        if (isTRUE(verbose)) {
            cat(" >> setting environment variables: \n")
            writeLines(sprintf("%s = %s", names(env), env))
        }
    }
    LinkingTo <- settings$LinkingTo
    if (!is.null(LinkingTo)) {
        paths <- .find.package(LinkingTo, quiet = TRUE)
        if (length(paths)) {
            flag <- paste(paste("-I\"", paths, "/include\"", sep=""), 
                collapse = " ")
            Sys.setenv(CLINK_CPPFLAGS = flag)
            if (isTRUE(verbose)) {
                cat(sprintf("\n >> LinkingTo : %s\n", paste(LinkingTo, 
                  collapse = ", ")))
                cat("CLINK_CPPFLAGS = ", flag, "\n\n")
            }
        }
    }
    if (isTRUE(verbose)) {
        writeLines(" >> Program source :\n")
        writeLines(inline:::addLineNumbers(code))
    }
    language <- "C++"
    libLFile <- inline:::compileCode(f, code, language = language, verbose = verbose)
    dyn.load(libLFile)
    retmod <- Module(modname, PACKAGE=f)
    return(retmod)
}  ## end function

## if (FALSE) {
##     ## not run -- finalizer from cxxfunction, needed?  where to unlink libLFile? 
##     cleanup <- function(env) {
##         if (f %in% names(getLoadedDLLs()))
##             dyn.unload(libLFile)
##         unlink(libLFile)
##     }
##     reg.finalizer(environment(), cleanup, onexit = TRUE)
## }
