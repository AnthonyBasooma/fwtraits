
utils::globalVariables(".data")

.onAttach <- function(libname, pkgname){

  getinfo <- utils::packageDescription('fwtraits')

  #packageStartupMessage(paste("fwtraits", getinfo[["Version"]]))#, "(", getinfo[["Date"]], ")"))
}


