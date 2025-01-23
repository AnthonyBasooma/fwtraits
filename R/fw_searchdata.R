#' @noRd

tcheck <- function(tx, taxafile = FALSE) {

  orgroup <- sapply(tx, function(xx){

    if (xx == "fi" || xx == "fish" || xx == "fishes") {
      if (isTRUE(taxafile)) organismgroup <- "fi" else organismgroup <- "Fish"
    } else if (xx == "pp" || xx == "phyto" || xx == "phytoplankton") {
      if (isTRUE(taxafile)) organismgroup <- "pp" else organismgroup <- "Phytoplankton"
    } else if (xx == "mi" || xx == "macro" || xx == "macroinvertebrates") {
      if (isTRUE(taxafile)) organismgroup <- "mi" else organismgroup <- "Macroinvertebrates"
    } else if (xx == "di" || xx == "diatoms") {
      if (isTRUE(taxafile)) organismgroup <- "di" else organismgroup <- "Diatoms"
    } else if (xx == "pb" || xx == "phytobentho" || xx == "bentho") {
      if (isTRUE(taxafile)) organismgroup <- "pb" else organismgroup <- "Phytobentho"
    } else if (xx == "mp" || xx == "macrophyte") {
      if (isTRUE(taxafile)) organismgroup <- "mp" else organismgroup <- "Macrophytes"
    } else {
      stop("Incorrect taxanomic group are entered. Use 'pp', 'fi', 'mi','di', 'pb', and 'mp' or run tcheck()")
    }
  }, USE.NAMES = FALSE)

  return(orgroup)
}


#' @noRd
#'
getfimppp <- function(x, token, organismgroup, url, allClasses = NULL, inform = FALSE) {

  fgkey <- list(x, token, organismgroup, url, allClasses)

  finalData <- loadCache(fgkey)

  if(!is.null(finalData)){

    if(isTRUE(inform)) message(tcheck(organismgroup), ' data was already downloaded.')

    return(finalData)

  }else{

    if(isTRUE(inform)) message(tcheck(organismgroup), ' data is downloading, be calm and patient on the first run...')

    traitcodes <- x[["code"]]

    available4 <- strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]]

    # replace the all classes with all the classes

    if ("all classes" %in% available4) available4 <- allClasses else available4

    ldata <- request(base_url = url) |> req_auth_bearer_token(token = token)

    # harmonise organism group

    if (organismgroup == "fi") {

      extdata <- ldata |>
        req_body_raw(body = toJSON(
          list(
            organismgroup = organismgroup, taxagroup = available4,
            displayValues = list(ecologicalparameter = traitcodes)
          ),
          auto_unbox = TRUE
        ), type = "application/json")
    } else {
      # "mp", "pb", "di" and "pp" are at class level vs fi and mi-order

      extdata <- ldata |>
        req_body_raw(body = toJSON(
          list(
            organismgroup = organismgroup, class = available4,
            displayValues = list(ecologicalparameter = traitcodes)
          ),
          auto_unbox = TRUE
        ), type = "application/json")
    }

    reqdata <- extdata |>

      req_user_agent(string = "fwtraits, ('anthony.basooma@boku.ac.at')" )

    #try and get the error during data collation and customize the messages

    reqdata2 <- tryCatch(expr = reqdata  |> req_perform(), error = function(e) return(NULL))

    #if successfully executed

    if(!is.null(reqdata2)){

      fxdata <- reqdata2 |> resp_body_json()

      finalData <- as.data.frame(do.call(rbind, fxdata$searchResult))

      saveCache(finalData, key=fgkey, comment="fi, mp, pp downloaded.")

    finalData;

    }else{
      #confirm the status code
      lastresp <- last_response()

      if(lastresp$status_code==403) stop("Either run the fw_token() to refresh the token and try again.", call. = FALSE)
    }
  }
}

#'
#' @noRd
#'

retdata <- function(organismgroup, taxagroup = NULL, codes, family = NULL, urlx, token, class = NULL, inform = FALSE){

  invertkey <- list(organismgroup, taxagroup, codes, family, urlx, token, class)

  finalData <- loadCache(invertkey)

  if(!is.null(finalData)){

    if(isTRUE(inform)) message(tcheck(organismgroup), ' data was already downloaded.')

    return(finalData)

  }else{

    reqdata <- request(base_url = urlx) |>

      req_auth_bearer_token(token = token)

    if(organismgroup=='mi'){

      exdata <- reqdata |> req_body_raw(body = jsonlite::toJSON(
        list(
          organismgroup = organismgroup, taxagroup = taxagroup,
          family = family,
          displayValues = list(ecologicalparameter = codes)
        ),
        auto_unbox = TRUE
      ), type = "application/json") |>

        req_user_agent(string = "fwtraits, ('anthony.basooma@boku.ac.at')" )
    }else{
      exdata <- reqdata |> req_body_raw(body = jsonlite::toJSON(
        list(
          organismgroup = organismgroup, class = class,

          displayValues = list(ecologicalparameter = codes)
        ),
        auto_unbox = TRUE
      ), type = "application/json") |>

        req_user_agent(string = "fwtraits, ('anthony.basooma@boku.ac.at')" )
    }

    if(isTRUE(inform)) message(tcheck(organismgroup), ' for parameter -', codes,'- data is downloading. Be patient..')

    ldata_out <- tryCatch(expr = exdata |> req_perform() |> resp_body_json(), error = function(e) return(NULL))

    if(is.null(ldata_out)){

      if(codes=='5') {
        warning('Data download was not successful. please consider downloading first Chironomidae if ecological distribution sensu is querried.', call. = FALSE)
      }else{
        warning("Data download failed. please consider downloading in batches or contact database admin.", call. = FALSE)
        }
    }
    #if successfully executed

    if(!is.null(ldata_out)){

      finalData <- as.data.frame(do.call(rbind, ldata_out$searchResult))

      saveCache(finalData, key = invertkey, comment = 'invertebrates data')

      finalData;

    }else{
      #confirm the status code
      lastresp <- last_response()

      if(lastresp$status_code==403) stop("Either run the fw_token() to refresh the token or reduce the number of traits.", call. = FALSE)
    }

  }
}

#internal names for benthos and macroinvertebrates

#' @title To download data from the Freshwaterecology.info database.
#'
#' @description
#' The function provides a seamless access and download of species ecological parameters, traits,
#' or indicators from the Freshwaterecology.info database. The function allows multiple inclusion
#' of organism groups, which include macroinvertebrates, fishes, phytoplankton, phytobenthos,
#' macrophytes, and diatoms.
#'
#'
#' @param organismgroup \code{string}. The organism group to download from the platform.
#'      The allowed group include \code{"fi", "mi", "pp", "pb", "di","mp"} for fishes,
#'      macroinvertebrates, phytoplankton,
#'      phytobenthos, diatoms, and macrophytes, respectively. Multiple groups allowed such as \code{'pp', 'di'}.
#' \itemize{
#'         \item{\code{pp}: Pytoplankton.}
#'         \item{\code{mp}: Macrophtytes}
#'         \item{\code{mi}: Macroinvertebrates}
#'         \item{\code{fi}: Fishes}
#'         \item{\code{di}: Diatoms}
#'         \item{\code{pb}: Phytobenthos without diatoms}
#'           }
#' @param ecoparams \code{vector}. Selected traits that should be downloaded for particular organism group. Check \code{\link{fw_dbguide}} for the allowed
#'      traits in the database.
#' @param taxalevel \code{string} Allowed taxonomic levels at which data can retrieved. Default is \code{'species'} but data can also be downloaded at family level,
#'        genus, and taxa group level.
#' @param taxa_searched \code{string} An internal placeholder to accommodate the standard taxonomic names for
#'        invertebrates and phytobenthos from the database.
#' @param inform \code{logical}. To indicate if the token is successfully generated. Default \code{TRUE}.
#' @param cachefolder \code{string}. The root path were the cached data will be saved on the user PC.
#'      If the path is not provided, the cached information will be saved in the current
#'      working directly.
#' @details
#'
#' For macroinvertebrates, since there is alot of data which take a lot of time to download and yet slowing the process, the \code{taxa_searched}
#'  parameter should be provided to only search for the particular traits, family and orders or taxa group where the species falls. Also, the
#'  phytobenthos requires to provide the taxa_searched to enable search for a particular taxa group.
#'  For other organism group, the parameter should not provided because will not be used in the data search.
#'
#' @inheritParams checktrait
#' @inheritParams fw_token
#'
#' @importFrom httr2 request req_body_raw  req_perform resp_body_json req_auth_bearer_token req_user_agent last_response
#' @importFrom jsonlite toJSON
#' @importFrom R.cache getCacheRootPath loadCache saveCache setCacheRootPath
#' @importFrom utils data
#'
#' @return List of download species traits
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#
#' dfsearch <- fw_searchdata(organismgroup = 'fi', ecoparams = 'migration', cachefolder = 'cache')
#' }

fw_searchdata <- function(organismgroup, taxa_searched = NULL,
                          ecoparams = NULL,
                          apikey = NULL,
                          warn = TRUE,
                          seed = 1135,
                          secure = TRUE,
                          inform = FALSE,
                          taxalevel = NULL,
                          cachefolder = 'cache') {

  sapply (organismgroup, function(xx) match.arg(xx, choices = c('fi','mi', 'pp', 'di','pb', 'mp')))

  token <- fw_token(apikey= apikey, seed = seed, secure = secure, inform = inform, cachefolder = cachefolder)

  if(is.null(token)) stop("Provide the token key to continue or run fw_be4ustart() and learn to set the token.",
                          call. = FALSE)

  #set R.cache folder based on the user working directory

  cachedir <- fw_path(cachefolder)

  setCacheRootPath(path= cachedir)

  cache.root = getCacheRootPath()

  # get database map
  getparam_list <- fw_paramlist(cachefolder = cachefolder)

  # extract parameters
  ecolist <- getparam_list$ecologicalParameterList # requires only fi, mi, pb, pp, pd, di

  #loop through organism group

  organismdata <- sapply(unlist(organismgroup), function(gg){

    #get standard names for traits in the database

    standardtraits <- sapply(ecolist[[gg]], function(x) x[["name"]])

    #clean to standardize the trait names
    stdf <- clean_traits(standardtraits)

    if(is(ecoparams, 'list')) ecoparamlist <- unique(ecoparams[[gg]]) else ecoparamlist <- unique(ecoparams)

    #compare with the clean and user provided trait names
    ctraits <-  checktrait(x= ecoparamlist, std = stdf, grp = tcheck(tx = gg), warn = warn)

    #extract traits indicated by the user

    gettaxa <- ecolist[[gg]][which(stdf%in%ctraits ==TRUE)]

    # get the base url for the organism group data tables

    qurl <- "https://www.freshwaterecology.info/fweapi2/v1/query"

    if(gg == 'mi' | gg=='pb'){

      #get the species search list for macroinvertebrates and phytobenthos

      if(is(taxa_searched, 'list')) searched_spp <- taxa_searched[[gg]] else searched_spp <- taxa_searched

      #Load reference dataset for either macroinvertebrates or benthos

      #clean species names

      if(gg =='mi'){

        data("invertbackbone", envir = environment())

        invdata <- get("invertbackbone", envir  = environment())

        #clean taxa names before searching
        tlevels <- switch (taxalevel, species ='Taxon', family='Family', taxagroup = 'Taxagroup', genus ='Genus')

        if(tlevels=='Genus') invdata[,'Genus'] <- sub(" .*", "", invdata$Taxon)

        taxaclean <- clean_names(sp = searched_spp, prechecks = TRUE, standard_dataset = invdata, taxalevel = taxalevel)

        speciestaxa <- unique(invdata$Taxagroup[which(unlist(invdata[,tlevels])%in%taxaclean ==TRUE)])
      }else{
        data("pbenthodata", envir = environment())

        bendata <- get("pbenthodata", envir  = environment())

        #clean taxa names before searching

        tlevels <- switch (taxalevel, species ='Taxon', genus ='Genus', taxagroup = 'Taxagroup')#phytobenthos dont have taxa groups

        if(is.null(tlevels))stop("If Phytobenthos are considered, then only taxagroup, genus and species taxalevels should be allowed.", call. = FALSE)

        if(tlevels=='Genus') bendata[,'Genus'] <- sub(" .*", "", bendata$Taxon)

        taxaclean <- clean_names(sp = searched_spp, prechecks = TRUE, standard_dataset = bendata, taxalevel = taxalevel)

        speciestaxa <- unique(bendata$Taxagroup[which(unlist(bendata[,tlevels])%in%taxaclean ==TRUE)])
      }

      standardtaxa <- sapply(gettaxa, function(x) strsplit(x[["availableFor"]],split = ", ", fixed = TRUE)[[1]])

      #it return a vector of characters not a list, affects data search

      if(length(gettaxa)==1) standardtaxa <- list(c(standardtaxa))

      standardcodes <- sapply(gettaxa, function(x) x[[1]])

      #retained taxa orders or groups from user provided data vs database for each trait
      retainorders <- sapply(standardtaxa, function(xx) intersect(xx, speciestaxa), simplify = FALSE)

      #assign trait codes to taxa groups with data
      names(retainorders) <- standardcodes

      #remove trait list with no taxa group
      taxawithdata <- retainorders[sapply(retainorders, length) > 0]

      if(gg=='mi'){

        searchdata <- sapply(names(taxawithdata), function(ww){

          if(ww == "133") families <- unique(invdata$Family[which(invdata$Taxagroup=='Trichoptera' & invdata$Taxon%in%taxaclean)]) else families <- NULL

          taxain <- taxawithdata[[ww]]

          codesin <- ww

          dfout <- retdata(organismgroup = gg, taxagroup = taxain, codes = codesin, family = families,
                           urlx = qurl, token = token, inform = inform )

        }, simplify = FALSE)
      }else{

        searchdata <- sapply(names(taxawithdata), function(vv){

          classin <- taxawithdata[[vv]]

          codesin <- vv

          dfout <- retdata(organismgroup = gg, codes = codesin, class = classin,
                           urlx = qurl, token = token, inform = inform )

        }, simplify = FALSE)
      }


    }else{

        #get taxa is carried over to handle all classes anomaly in macropyhtes data

        dfout <- sapply(gettaxa, getfimppp, token = token, organismgroup = gg,
                        url = qurl, allClasses = fw_classes(paramlist = ecolist[[gg]]),
                        inform = inform,
                        simplify = FALSE, USE.NAMES = TRUE)

    }

  },simplify = FALSE)

   return(organismdata)
}
