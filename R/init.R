
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("You have loaded the ",pkgname,
                          " package from ", libname)
    tradeharvester <- list(template = "template")
    options(tradeharvester = tradeharvester)
    setcomextconfig()
}
