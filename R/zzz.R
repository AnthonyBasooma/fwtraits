
# .onLoad <- function(libname, pkgname) {
#   getdata <<- memoise::memoise(getdata)
# }

.onAttach <- function(libname, pkgname){

  getinfo <- utils::packageDescription('fwtraits')

  packageStartupMessage(paste("fwtraits", getinfo[["Version"]]))#, "(", getinfo[["Date"]], ")"
}


