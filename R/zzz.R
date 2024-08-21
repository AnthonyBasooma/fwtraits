
.onLoad <- function(libname, pkgname) {

  fip_token <<- memoise::memoise(fip_token, cache = cachem::cache_disk(dir = 'authkey'))

  getfiles <<- memoise::memoise(getfiles, cache = cachem::cache_disk(dir = 'taxadata'))
}


.onAttach <- function(libname, pkgname){

  getinfo <- utils::packageDescription('fwtraits')

  packageStartupMessage(paste("fwtraits", getinfo[["Version"]]))#, "(", getinfo[["Date"]], ")"))
}


