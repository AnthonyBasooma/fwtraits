
.onLoad <- function(libname, pkgname) {

  tokenkey <<- memoise::memoise(tokenkey)
}


.onAttach <- function(libname, pkgname){

  getinfo <- utils::packageDescription('fwtraits')

  packageStartupMessage(paste("fwtraits", getinfo[["Version"]]))#, "(", getinfo[["Date"]], ")"))
}


