
.onLoad <- function(libname, pkgname) {

  fw_token <<- memoise::memoise(fw_token, cache = cachem::cache_disk(dir = 'authtoken'))

  getbenthos <<- memoise::memoise(getbenthos, cache = cachem::cache_disk(dir = 'taxadata'))

  getfimppp <<- memoise::memoise(getfimppp, cache = cachem::cache_disk(dir = 'taxadata'))

  getinverts <<- memoise::memoise(getinverts, cache = cachem::cache_disk(dir = 'taxadata'))
}


.onAttach <- function(libname, pkgname){

  getinfo <- utils::packageDescription('fwtraits')

  packageStartupMessage(paste("fwtraits", getinfo[["Version"]]))#, "(", getinfo[["Date"]], ")"))
}


