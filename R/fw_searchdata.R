#' @noRd

tcheck <- function(tx, taxafile = FALSE) {

  orgroup <- sapply(tx, function(it){

    if (it == "fi" || it == "fish" || it == "fishes") {
      if (isTRUE(taxafile)) organismgroup <- "fi" else organismgroup <- "fishes"
    } else if (it == "pp" || it == "phyto" || it == "phytoplankton") {
      if (isTRUE(taxafile)) organismgroup <- "pp" else organismgroup <- "phytoplankton"
    } else if (it == "mi" || it == "macro" || it == "macroinvertebrates") {
      if (isTRUE(taxafile)) organismgroup <- "mi" else organismgroup <- "macroinvertebrates"
    } else if (it == "di" || it == "diatoms") {
      if (isTRUE(taxafile)) organismgroup <- "di" else organismgroup <- "diatoms"
    } else if (it == "pb" || it == "phytobentho" || it == "bentho") {
      if (isTRUE(taxafile)) organismgroup <- "pb" else organismgroup <- "phytobentho"
    } else if (it == "mp" || it == "macrophyte" || it == "grasses") {
      if (isTRUE(taxafile)) organismgroup <- "mp" else organismgroup <- "macrophyte"
    } else {
      stop("Incorrect taxanomic group are entered. Use 'pp', 'fi', 'mi','di', 'pb', and 'mp' or run tcheck()")
    }
  }, USE.NAMES = FALSE)

  return(orgroup)
}


#' @noRd
#'
.getfimppp <- memoise::memoise(function(x, token, harmztaxa, url, allClasses = NULL) {

  traitcodes <- x[["code"]]

  traitnames <- x[["name"]]

  available4 <- strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]]

  # replace the all classes with all the classes

  if ("all classes" %in% available4) available4 <- allClasses else available4

  ldata <- request(base_url = url) |> req_auth_bearer_token(token = token)

  # harmonise organism group

  if (harmztaxa == "fi") {

    extdata <- ldata |>
      req_body_raw(body = toJSON(
        list(
          organismgroup = harmztaxa, taxagroup = available4,
          displayValues = list(ecologicalparameter = traitcodes)
        ),
        auto_unbox = TRUE
      ), type = "application/json")
  } else {
    # "mp", "pb", "di" and "pp" are at class level vs fi and mi-order
    extdata <- ldata |>
      req_body_raw(body = toJSON(
        list(
          organismgroup = harmztaxa, class = available4,
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

    finaldata <- as.data.frame(do.call(rbind, fxdata$searchResult))

  }else{
    #confirm the status code
    lastresp <- last_response()

    if(lastresp$status_code==403) stop("Either run the fw_token() to refresh the token and try again.", call. = FALSE)
  }

}, cache = memoise::cache_filesystem(path = 'taxadata', compress = TRUE))


#'
#' @noRd
.getbenthos <- memoise::memoise(function(x, y, token, harmztaxa, url) {

  codes <- y[[x]]


  ldata <- request(base_url = url) |>

    req_auth_bearer_token(token = token) |>

    req_body_raw(body = jsonlite::toJSON(
      list(
        organismgroup = harmztaxa, class = x, #they are classes not taxagroup

        displayValues = list(ecologicalparameter = codes)
      ),
      auto_unbox = TRUE
    ), type = "application/json")|>

    req_user_agent(string = "fwtraits, ('anthony.basooma@boku.ac.at')" )

  #try and get the error during data collation and customize the messages

  reqdata <- tryCatch(expr = ldata  |> req_perform(), error = function(e) return(NULL))

  #if successfully executed

  if(!is.null(reqdata)){

    fxdata <- reqdata |> resp_body_json()

    phytobenthos <- as.data.frame(do.call(rbind, fxdata$searchResult))

  }else{
    #confirm the status code
    lastresp <- last_response()

    if(lastresp$status_code==403) stop("Run the fw_token() function to refresh the token and try again.", call. = FALSE)
  }
}, cache = memoise::cache_filesystem(path = 'taxadata', compress = TRUE))

#'
#' @noRd
.getinverts <- memoise::memoise(function(x, y, token, harmztaxa, url) {
  set.seed(1124)

  ordata <- y[[x]]

  #loop through the trait codes not as a group: first unlist

  xdata <- sapply(unlist(ordata), function(y) {

    reqdata <- request(base_url = url) |>

      req_auth_bearer_token(token = token)

    #this combination leads to error if not provided up to family level

    if(y == 133 && x =="Trichoptera"){

      tricofam <-  ssdfamily()

      famList <- split(tricofam , sample(3, length(tricofam) , replace = TRUE) )

      xsd <- sapply(famList, function(ff){

        exdata <- reqdata |> req_body_raw(body = jsonlite::toJSON(
          list(
            organismgroup = harmztaxa, taxagroup = x,
            family = ff,
            displayValues = list(ecologicalparameter = y)
          ),
          auto_unbox = TRUE
        ), type = "application/json")

        ldata_out <- tryCatch(expr = exdata |> req_perform(), error = function(e) return(NULL))

        #if successfully executed

        if(!is.null(ldata_out)){

          fxdata <- ldata_out |> resp_body_json()

          lmdata <- as.data.frame(do.call(rbind, fxdata$searchResult))

        }else{
          #confirm the status code
          lastresp <- last_response()

          if(lastresp$status_code==403) stop("Either run the fw_token() to refresh the token or reduce the number of traits.", call. = FALSE)
        }

      }, simplify = FALSE)

      lmdata <- do.call(rbind, xsd)

    }else{
      exdata <- reqdata |> req_body_raw(body = jsonlite::toJSON(
        list(
          organismgroup = harmztaxa, taxagroup = x,

          displayValues = list(ecologicalparameter = y)
        ),
        auto_unbox = TRUE
      ), type = "application/json") |>

        req_user_agent(string = "fwtraits, ('anthony.basooma@boku.ac.at')" )

      #try and get the error during data collation and customize the messages

      ldata_out <- tryCatch(expr = exdata |> req_perform(), error = function(e) return(NULL))

      #if successfully executed

      if(!is.null(ldata_out)){

        fxdata <- ldata_out |> resp_body_json()

        lmdata <- as.data.frame(do.call(rbind, fxdata$searchResult))

      }else{
        #confirm the status code
        lastresp <- last_response()

        if(lastresp$status_code==403) stop("Either run the fw_token() to refresh the token or reduce the number of traits.", call. = FALSE)
      }
    }

  }, USE.NAMES = TRUE, simplify = FALSE)
}, cache = memoise::cache_filesystem(path = 'taxadata', compress = TRUE))


#' @title To download data from the Freshwaterecology.info database.
#'
#' @description
#' The function provides a seamless access and download of species ecological parameters, traits,
#' or indicators from the Freshwaterecology.info database. The fucntion allows multiple inclusion
#' of organism groups, which include macroinvertebrates, fishes, phytoplankton, phytobenthos,
#' macrophytes, and diatoms. Parallelisation can also enabled to allow faster data download from
#' for the database severs.
#'
#'
#' @param organismgroup \code{string}. The organismgroup group to download from the platform.
#'      The allowed group include \code{"fi", "mi", "pp", "pb", "di","mp"} for fishes, macroinvertebrates, phytoplankton,
#'      phytobenthos, diatoms, and macrophytes. Multiple groups allowed such as \code{'pp', 'di'}.
#' \itemize{
#'         \item{\code{pp}: Pytoplankton.}
#'         \item{\code{mp}: Macrophtytes}
#'         \item{\code{mi}: Macroinvertebrates}
#'         \item{\code{fi}: Fishes}
#'         \item{\code{di}: Diatoms}
#'         \item{\code{pb}: Phytobenthos without diatoms}
#'           }
#' @param taxagroup \code{vector}. If \code{organismgroup} is \code{mi}, the \code{taxagroup} must be indicated for data to be downloaded.
#'      The different macroinvertebrates orders allowed can be obtained using \code{\link{fw_taxagroup}} function.
#' @param ecoparams \code{vector}. Selected traits that should be downloaded for particular organismgroup group. Check \code{\link{fw_dbguide}} for the allowed
#'      traits in the database.
#' @param token \code{string}. This is a required parameter to allow user authentication with the platform. To get the token, use
#'      \code{\link{fw_be4ustart}} function to get the required steps. Remember that the token is saved in memory such that
#'      the data downloaded is not re-downloaded in the next session.
#' @param parallel \code{logical}. If \code{TRUE} then the parallel data download is enabled.
#' @param cores \code{integer} An integer indicating the number of cores to be be used in parallelisng the
#'      the data download. Default is 2.
#' @inheritParams checktrait
#'
#' @importFrom httr2 request req_body_raw  req_perform resp_body_json req_auth_bearer_token req_user_agent last_response
#' @importFrom jsonlite toJSON
#' @importFrom memoise memoise cache_filesystem
#' @importFrom parallel detectCores clusterEvalQ makeCluster parSapply stopCluster
#'
#' @return List of download species traits
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #encrypted token for my api key
#'
#' enc_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"
#'
#' apikey <- httr2::secret_decrypt(encrypted = enc_api, key = 'FWTRAITS_KEY')
#'
#' #download fish catchment region data
#' #setting the FWTRAITS_KEY
#'
#' #run this usethis::edit_r_environ()
#'
#' apikeydecrypted <- fw_loadapikey(test = TRUE, encrytedkey = enc_api,
#'                               fwtraitskey =  'FWTRAITS_KEY')
#'
#' tokendata <- fw_token(key= apikeydecrypted, seed = 1234)
#'
#' dfpull <- fw_searchdata(organismgroup = 'fi', ecoparams = 'migration', token = tokendata)
#' }

fw_searchdata <- function(organismgroup, taxagroup = NULL,
                     ecoparams = NULL, token, warn = TRUE,
                     parallel= FALSE, cores = 3) {

  if (!curl::has_internet()) stop("Not connected on internet to access the database.")

  if(is.null(token)) stop("Provide the token key to continue, run fw_be4ustart() function and learn to set the token.", call. = FALSE)

  # get database map
  getparam_list <- fw_paramlist()

  # extract parameters
  ecolist <- getparam_list$ecologicalParameterList # requires only fi, mi, pb, pp, pd, di

  #loop through organism group

  organismdata <- sapply(organismgroup, function(orgx){

  harmztaxa <- tcheck(tx = orgx, taxafile = TRUE)

  gettaxa <- ecolist[[harmztaxa]]

  if(!is.null(ecoparams) && is.null(taxagroup)){

    standardtraits <- sapply(gettaxa, function(x) x[["name"]])

    #clean to standardize the names
    stdf <- clean_traits(standardtraits)

    if(is(ecoparams, 'list')) ecoparamgroup <- ecoparams[[orgx]] else ecoparamgroup <- ecoparams

    #compare with the clean and user provided trait names
    ctraits <-  checktrait(x= ecoparamgroup, std = stdf,
                           grp = tcheck(tx = orgx),
                           warn = warn)

    gettaxa_final <- gettaxa[which(stdf%in%ctraits ==TRUE)]

    #carry on a group list of taxa group for pp to handle all classes aspect

    #if(orgx=='mp') allclasses <- gettaxa

  }else{

    gettaxa_final <- gettaxa
  }

  #parallel settings

  if(isTRUE(parallel)){

    if(is.null(cores) | cores<=1) stop("If parallel is TRUE, the cores must not be NULL or less than 1")

    ncored <- detectCores()

    if(is.na(ncored) | is.null(ncored) | ncored<=1) stop("The cores detected are either NA, NULL or less to 1, please use the slower version.")

    if(cores>=ncored | (ncored-cores)<2) stop("Reduce the cores to ", ncored-2," or less for effective parallelisation.")

    clusters <- makeCluster(spec = cores, type = 'PSOCK')

    clusterEvalQ(cl = clusters, expr = c(library("httr2"), library("jsonlite")))
  }

  # get the base url for the organism group data tables

  qurl <- "https://www.freshwaterecology.info/fweapi2/v1/query"

  if (harmztaxa == "mi" | harmztaxa == "pb") {

    # extract data for macro invertebrates and phytobenthos

    # get all list available for traits
    allorders <- sapply(gettaxa_final, function(x) strsplit(x[["availableFor"]],
                                                            split = ", ", fixed = TRUE)[[1]])

    traitcodes <- sapply(gettaxa_final, function(x) x[[1]])

    # merge all lists and make them unique to get the orders or classes (phytobenthos)
    if(is.null(ecoparams)){

      names(allorders) <- traitcodes
    }else{
      if(length(allorders)>1 && length(ecoparams)==1) names(allorders) <- rep(traitcodes, length(allorders)) else names(allorders) <- traitcodes
    }

    if(length(allorders)==1) unique_ORDERS <- allorders else unique_ORDERS <- unique(Reduce(c, allorders))

    # get the traitcodes for each order

    codematrix <- matrix(NA, nrow = length(unique_ORDERS), ncol = length(allorders))

    for (v in seq_along(unique_ORDERS)) {

      order <- unique_ORDERS[v]

      for (vi in seq_along(allorders)) {

        lstoerds <- unlist(allorders[vi])

        cnames <- as.numeric(names(allorders[vi]))

        if (order %in% lstoerds == TRUE) codematrix[v, vi] <- cnames else codematrix[v, vi] <- NA
      }
    }
    row.names(codematrix) <- unique_ORDERS

    # extract all matrix rows, remove NAs and maintain the ecoparams for each organismgroup

    taxafinal0 <- apply(codematrix, 1, FUN = function(x) unlist(x)[!is.na(unlist(x))])

    taxafinal <- taxafinal0[sapply(taxafinal0, length) > 0]

    #macro are selected because they require to loop through the traits vs phtobenthos that allows combining them without the 403 error
    if (harmztaxa == "mi") {

      # remove taxa groups that are not available in the list of taxa groups

      tf <- unlist(taxafinal) %in% c("Araneae", "Kamptozoa")

      if(any(tf)==TRUE) {

        orderlist <- taxafinal[which(tf==FALSE)]

      }else{
        orderlist <- taxafinal
      }

      if(!is.null(taxagroup) && !is.null(ecoparams)) stop("If using selected species ecological traits, the taxagroup must be NULL")

      if(!is.null(taxagroup)){

        #check if the order entered are in the allowed list

        inOut <- taxagroup%in%names(orderlist)

        if(all(inOut) == TRUE){

          taxasel <- orderlist[taxagroup]
        }else{
          ordersnotin <- taxagroup[which(inOut==FALSE)]

          stop("The orders: ", paste(ordersnotin, collapse = ", "), " is/are not in the standard order list for macroinvertebrates. run fw_taxagroup() function to identify allowed orders.")
        }
      }else{
        taxasel <- orderlist
      }
      if(isTRUE(parallel)){

        dfout <- parSapply(clusters, names(taxasel), .getinverts,
                           y = taxasel, token = token,
                           harmztaxa = harmztaxa, url = qurl)

        stopCluster(cl = clusters)

        return(dfout)
      }else{
        xsp <- sapply(names(taxasel), .getinverts, y = taxasel, token = token,
                      harmztaxa = harmztaxa, url = qurl)#end of macro inverts
      }
    } else {#start phyto benthos
      taxasel <- taxafinal

      if(isTRUE(parallel)){

        dfout <- parSapply(clusters, names(taxasel), .getbenthos,
                           y = taxasel, token = token,
                           harmztaxa = harmztaxa, url = qurl,
                           simplify = FALSE, USE.NAMES = TRUE)

        stopCluster(cl = clusters)
        return(dfout)

      }else{
        dfout <- sapply(names(taxasel),.getbenthos, y = taxasel, token = token,
                        harmztaxa = harmztaxa, url = qurl,
                        simplify = FALSE, USE.NAMES = TRUE)
      }
    }#end phytobentho

    #start fish, phytoplankton, diatoms, and macrophytes
  } else {

    if(isTRUE(parallel)){

      dfout <- parSapply(clusters, gettaxa_final, .getfimppp, token = token,
                         harmztaxa = harmztaxa,
                         url = qurl, allClasses = fw_classes(paramlist = gettaxa),
                         simplify = FALSE, USE.NAMES = TRUE)

      stopCluster(cl = clusters)
      return(dfout)
    }else{

      #get taxa is carried over to handle all classes anomaly in macrophtes data

      dfout <- sapply(gettaxa_final, .getfimppp, token = token, harmztaxa = harmztaxa,
                      url = qurl, allClasses = fw_classes(paramlist = gettaxa),
                      simplify = FALSE, USE.NAMES = TRUE)
    }
  }
  },simplify = FALSE)

  return(organismdata)
}



#' @title To get the allowed macroinvertebrates in the Freshwaterecology.info.
#'
#' @return \code{vector}. A vector of allowed macro invertebrates taxonomic groups that can be set in the \code{taxagroup} parameter while
#'        while getting data.
#'
#' @export
#'
#' @examples
#'
#' x <- fw_taxagroup()
#'
fw_taxagroup <- function(){

  getparam_list <- fw_paramlist()

  # extract parameters
  ecolist <- getparam_list$ecologicalParameterList$mi

  allorders <- sapply(ecolist, function(x) strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]])

  ordernames <- unique(do.call(c, allorders))

  orderlist <- ordernames[!ordernames %in% c('Kamptozoa','Araneae')]

  return(orderlist)
}

#' @noRd
ssdfamily <- function(){
  famvec <- c("APATANIIDAE", "BERAEIDAE",  "BRACHYCENTRIDAE","CALAMOCERATIDAE","ECNOMIDAE",
  "GLOSSOSOMATIDAE","GOERIDAE","HELICOPSYCHIDAE", "HYDROPSYCHIDAE","HYDROPTILIDAE",
  "LEPIDOSTOMATIDAE", "LEPTOCERIDAE","LIMNEPHILIDAE","MOLANNIDAE" , "ODONTOCERIDAE",
  "PHILOPOTAMIDAE", "PHRYGANEIDAE" ,"POLYCENTROPODIDAE", "PSYCHOMYIIDAE","PTILOCOLEPIDAE",
  "RHYACOPHILIDAE","SERICOSTOMATIDAE","UENOIDAE")
  return(famvec)
}


