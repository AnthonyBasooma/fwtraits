
.onLoad <- function(libname, pkgname) {

  #fw_token <<- memoise::memoise(fw_token, cache = cachem::cache_disk(dir = 'authkey'))

  #fw_getdata <<- memoise::memoise(fw_getdata, cache = cachem::cache_disk(dir = 'taxadata'))

  #.getfimppp <<- memoise::memoise(.getfimppp, cache = cachem::cache_disk(dir = 'taxadata'))
}


.onAttach <- function(libname, pkgname){

  getinfo <- utils::packageDescription('fwtraits')

  packageStartupMessage(paste("fwtraits", getinfo[["Version"]]))#, "(", getinfo[["Date"]], ")"))
}


