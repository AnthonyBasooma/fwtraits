#' @title Steps to follow in using the Freshwater Information Platform
#'
#' @return \code{list} steps to follow in using the FWDB
#'
#' @importFrom rstudioapi viewer
#'
#' @export
#'
#' @examples
#'
#' #b4us <- fw_be4ustart()
#'
#' @author Anthony Basooma
#'


fw_be4ustart <- function() {

  dir <- tempfile()

  dir.create(dir)

  htmlFile <- file.path(dir, "beforeustart.html")

  #user1 <- Sys.info()[["user"]]

  writeLines("
  <body style='background-color:white;'>
  <h3>fwtraits</h3>
  <hr>


  <h4> 1. Aim of the package </h4>
  <p> The package provides robust and seamless access and interaction with the
  Freshwaterecology.info database, while extracting and arranging the species
  ecological preferences, traits or indicators. The database includes traits or
  ecological preferences for five taxonomic groups including fishes,
  macrophtes, macroinvertebrates, phytoplankton, and phytobenthos including diatoms. Over
  203 traits or ecological preferences exists for about 28000 species.</p>

  <h4> 2. Step to start interacting with Freshwaterecology.info database </h4>

  <p> <b>STEP 1</b>: If you are not registered with the Freshwater Information Platform, where the Freshwaterecology.info
  database is part of, proceed to <b> STEP 2</b>, otherwise proceed to STEP 3</p>

<p> <b>Step 2</b>: Register at the Freshwaterecology.info database <a style='color: blue'
href='https://www.freshwaterecology.info/register/index.php'> registration page</a> to obtain an API key. During registration, please tick
for acquistion of the API key which will be shortly sent to the email entered. After proceed to STEP 4 </p>

 <p> <b>STEP 3</b>: Apply for the API Key by sending your request for the key to registration@freshwaterecology.info </p>

 <p> <b>STEP 4</b>: The API key is an alphanumeric 36 character string. Run the fw_setapikey() to set up
 secure the key in the R user environment. After this the key will be automatically picked
 in the all the forthcoming scripts on that particular computer.</p>

 <p> <b>STEP 5</b>: Congratulations: Ready to interact with Freshwaterecology.info database will appear.</p>

 <p> For issues about the package fucntionality, please contact anthony.basooma@boku.ac.at or post on
 <a style='color: blue'
href='https://github.com/AnthonyBasooma/fwtraits/issues pages'> isuues page</a>

 <h4>3. Literature cited </h4>

  Schmidt-Kloiber, A., & Hering, D. (2015).
  Www.freshwaterecology.info - An online tool that unifies, standardises and
  codifies more than 20,000 European freshwater organisms and their ecological
  preferences. Ecological Indicators, 53, 271-282.
  https://doi.org/10.1016/j.ecolind.2015.02.007

 <hr>
 <p> <center><b>Prepared by Anthony Basooma (anthony.basooma@boku.ac.at)  </center> </p>
 <p> <center>THANK YOU </center></p>

</br></body>" , con = htmlFile)

  viewer(htmlFile)
}



#' @title Function to retrieve all the traits in the database.
#'
#' @return \code{list} List of traits for all the taxa groups and orders in the database.
#'
#' @keywords internal
#'
#' @author Anthony Basooma
#'
fw_paramlist <- function(cachefolder = NULL) {

  # base url with parameter list

  cachedir <- fw_path(cachefolder)

  setCacheRootPath(path= cachedir)

  cache.root = getCacheRootPath()

  key <- list()

  mainparlist <- loadCache(key)

  if(!is.null(mainparlist)){

    return(mainparlist)

  }else{
    paramurl <- "https://www.freshwaterecology.info/fweapi2/v1/getecoparamlist"

    mainparlist <- request(base_url = paramurl) |>
      req_perform() |>
      resp_body_json()

    saveCache(mainparlist, key=key, comment="token code generated", compress = TRUE)

    mainparlist;
  }
}

#' @noRd
#'
fw_classes <- function(paramlist) {

  taxagrouplists <- sapply(paramlist, function(x) strsplit(x[['availableFor']], split = ", ", fixed = TRUE)[[1]])

  txall <- unique(do.call(c, taxagrouplists))

  txallfinal <- txall[!txall %in% c("all classes")]

  return(txallfinal)
}

#' @noRd
#'
fw_path <- function(dir = 'cache'){

  wd <- getwd()

  if(is.null(dir)){

    stop('Provide the cache directory.')

  }else{

    cachedir <- paste0(wd,'/',dir)

    if(dir.exists(cachedir)==FALSE){

      dir.create(dir)

      path = paste0(wd, '/', dir)

    }else{

      path = cachedir
    }
  }
  return(path)
}


#' Check for packages to install and reposnd to use
#'
#' @param pkgs list of packages to install
#'
#' @return error message for packages to install
#'
check_packages <- function(pkgs){

  pkginstall <- sapply(pkgs, requireNamespace, quietly = TRUE)

  pkgout <- pkgs[which(pkginstall==FALSE)]

  if(length(pkgout)>=1)stop('Please install ', length(pkgout), ' packages: ', paste(pkgout, collapse = ', '), ' to continue.', call. = FALSE)

  invisible(pkgs)
}

#' @title Get database citations
#' @param cachefolder \code{string}. The root path were the cached data will be saved on the user PC.
#'      If the path is not provided, the cached information will be saved in the current
#'      working directly.
#' @export
#'
fw_cite <- function(cachefolder = 'cache'){
  cat("******Please cite this website as:********", "\n", fw_paramlist(cachefolder = cachefolder)$citation, "\n")

}


