
.onLoad <- function(libname, pkgname) {

  fw_token <<- memoise::memoise(fw_token, cache = cachem::cache_disk(dir = 'authkey'))

  getfiles <<- memoise::memoise(getfiles, cache = cachem::cache_disk(dir = 'taxadata'))
}


.onAttach <- function(libname, pkgname){

  getinfo <- utils::packageDescription('fwtraits')

  packageStartupMessage(paste("fwtraits", getinfo[["Version"]]))#, "(", getinfo[["Date"]], ")"))
}


