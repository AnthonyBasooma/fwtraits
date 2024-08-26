#' @noRd

tcheck <- function(tx, taxafile = FALSE) {

  sapply(tx, function(it){

    if (it == "fi" || it == "fish" || it == "fishes") {
      if (isTRUE(taxafile)) taxa <- "fi" else taxa <- "fishes"
    } else if (it == "pp" || it == "phyto" || it == "phytoplankton") {
      if (isTRUE(taxafile)) taxa <- "pp" else taxa <- "phytoplankton"
    } else if (it == "mi" || it == "macro" || it == "macroinvertebrates") {
      if (isTRUE(taxafile)) taxa <- "mi" else taxa <- "macroinvertebrates"
    } else if (it == "di" || it == "diatoms") {
      if (isTRUE(taxafile)) taxa <- "di" else taxa <- "diatoms"
    } else if (it == "pb" || it == "phytobentho" || it == "bentho") {
      if (isTRUE(taxafile)) taxa <- "pb" else taxa <- "phytobentho"
    } else if (it == "mp" || it == "macrophyte" || it == "grasses") {
      if (isTRUE(taxafile)) taxa <- "mp" else taxa <- "macrophyte"
    } else {
      stop("Incorrect taxanomic group are entered. Use 'pp', 'fi', 'mi','di', 'pb', and 'mp' or run tcheck()")
    }
    return(taxa)
  }, USE.NAMES = FALSE)
}


#' @noRd
#'
.getdata <- function(x, token, harmztaxa, url) {

  traitcodes <- x[["code"]]

  traitnames <- x[["name"]]

  available4 <- strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]]

  # replace the all classes with all the classes

  if ("all classes" %in% available4) available4 <- fw_classes(paramlist = x) else available4

  ldata <- request(base_url = url) |> req_auth_bearer_token(token = token)

  # harmonise taxa

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

}

.bentho <- function(x, y, token, harmztaxa, url) {

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
}

.macro <- function(x, y, token, harmztaxa, url) {

  ordata <- y[[x]]

  #loop through the trait codes not as a group: first unlist

  xdata <- sapply(unlist(ordata), function(y) {

    reqdata <- request(base_url = url) |>

      req_auth_bearer_token(token = token)

    #this combination leads to error if not provided upto family level

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
}


#' @title To collate data from the Freshwater Information Platform.
#'
#' @param taxa \code{string}. The taxa group to download from the platform.
#'      The allowed group include \code{"fi", "mi", "pp", "pb", "di","mp"} for fishes, macroinvertebrates, phytoplankton,
#'      phytobenthos, diatoms, and macrophytes. Multiple groups allowed such as \code{'pp', 'di'}.
#' \itemize{
#'         \item{\code{pp: }}{Pytoplankton.}
#'         \item{\code{mp: }}{Macrophtytes}
#'         \item{\code{mi: }}{Macroinvertebrates}
#'         \item{\code{fi: }}{Fishes}
#'         \item{\code{di: }}{Diatoms}
#'         \item{\code{pb: }}{Phytobenthos without diatoms}
#'           }
#' @param taxaorder \code{vector}. If \code{taxa} is \code{mi}, the \code{taxaorder} must be indicated for data to be downloaded.
#'      The different macroinvertebrates orders allowed can be obtained using \code{\link{fw_orders}} function.
#' @param ecotraits \code{vector}. Selected traits that should be downloaded for particular taxa group. Check \code{\link{fw_ecoparamdb}} for the allowed
#'      traits in the database.
#' @param token \code{string}. This is a required parameter to allow user authentication with the platform. To get the token, use
#'      \code{\link{before_u_start}} function to get the required steps. Remember that the token is saved in memory such that
#'      the data downloaded is not re-downloaded in the next session.
#' @inheritParams checktrait
#'
#' @importFrom httr2 request req_body_raw  req_perform resp_body_json req_auth_bearer_token req_user_agent last_response
#' @importFrom jsonlite toJSON
#' @importFrom memoise memoise
#' @importFrom cachem cache_disk
#' @importFrom parallel detectCores clusterEvalQ makeCluster
#'
#' @return List of download species traits
#'

getfiles <- function(taxa, taxaorder = NULL, ecotraits = NULL, token, warn = TRUE, parallel= TRUE, cores = 3) {

  if (!curl::has_internet()) stop("Not connected on internet to access the database.")

  if(is.null(token)) stop("Provide the token key to continue, run before_u_start() function and learn to set the token.", call. = FALSE)

  # get database map
  getparam_list <- fw_paramlist()

  # extract parameters
  ecolist <- getparam_list$ecologicalParameterList # requires only fi, mi, pb, pp, pd, di

  # get for each taxa

  harmztaxa <- tcheck(tx = taxa, taxafile = TRUE)

  gettaxa <- ecolist[[harmztaxa]]


  if(!is.null(ecotraits) && is.null(taxaorder)){

    standardtraits <- sapply(gettaxa, function(x) x[["name"]])

    #clean to standardize the names
    stdf <- clean_traits(standardtraits)

    #compare with the clean and user provided trait names
    ctraits <-  checktrait(x= ecotraits, std = stdf, grp = tcheck(tx = taxa), warn = warn)

    #if(length(unlist(ctraits))<1) stop("No trait data to download from FWDB, please check for spellings for traits in the fw_ecoparamdb() generated dataset in the traitname column.")

    gettaxa_final <- gettaxa[which(stdf%in%ctraits ==TRUE)]

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

  # get the base url for the taxa data tables
  qurl <- "https://www.freshwaterecology.info/fweapi2/v1/query"

  if (harmztaxa == "mi" | harmztaxa == "pb") {

    # extract data for macro invertebrates and phytobenthos

    # get all list available for traits
    allorders <- sapply(gettaxa_final, function(x) strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]])

    traitcodes <- sapply(gettaxa_final, function(x) x[[1]])

    # merge all lists and make them unique to get the orders or classes (phytobenthos)
    if(is.null(ecotraits)){
      names(allorders) <- traitcodes
    }else{
      if(length(allorders)>1 && length(ecotraits)==1) names(allorders) <- rep(traitcodes, length(allorders)) else names(allorders) <- traitcodes
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

    # extract all matrix rows, remove NAs and maintain the ecoparams for each taxa

    taxafinal0 <- apply(codematrix, 1, FUN = function(x) unlist(x)[!is.na(unlist(x))])

    taxafinal <- taxafinal0[sapply(taxafinal0, length) > 0]

    #macro are selected bse require to loop through the traits vs phtobenthos that allows combining them without the 403 error
    if (harmztaxa == "mi") {

      # remove orders that are not in available for list

      tf <- unlist(taxafinal) %in% c("Araneae", "Kamptozoa")

      if(any(tf)==TRUE) {

        orderlist <- taxafinal[which(tf==FALSE)]

      }else{
        orderlist <- taxafinal
      }

      if(!is.null(taxaorder) && !is.null(ecotraits)) stop("If using selected species ecological traits, the taxaorder must be NULL")

      if(!is.null(taxaorder)){

        #if (length(unique(taxaorder)) > 4) stop("Please a maximum of 4 macro invertebrates orders are allowed but ", length(unique(taxaorder)), " have been provided.")

        #check if the order entered are in the allowed list

        inOut <- taxaorder%in%names(orderlist)

        if(all(inOut) == TRUE){

          taxasel <- orderlist[taxaorder]
        }else{
          ordersnotin <- taxaorder[which(inOut==FALSE)]

          stop("The orders: ", paste(ordersnotin, collapse = ", "), " is/are not in the standard order list for macroinvertebrates. run fw_orders() function to identify allowed orders.")
        }
      }else{
        taxasel <- orderlist
      }
      if(isTRUE(parallel)){

        dfout <- parSapply(clusters, names(taxasel), .macro, y = taxasel, token = token,
                           harmztaxa = harmztaxa, url = qurl)

        stopCluster(cl = clusters)

        return(dfout)
      }else{
        xsp <- sapply(names(taxasel), .macro, y = taxasel, token = token,
                      harmztaxa = harmztaxa, url = qurl)#end of macro inverts
      }
    } else {#start phyto benthos
      taxasel <- taxafinal

      if(isTRUE(parallel)){

        dfout <- parSapply(clusters, names(taxasel), .bentho, y = taxasel, token = token,
                           harmztaxa = harmztaxa, url = qurl,
                           simplify = FALSE, USE.NAMES = TRUE)

        stopCluster(cl = clusters)

        return(dfout)

      }else{
        dfout <- sapply(names(taxasel),.bentho, y = taxasel, token = token, harmztaxa = harmztaxa, url = qurl,
                        simplify = FALSE, USE.NAMES = TRUE)
      }
    }#end phytobentho

    #start fish, phytoplankton, diatoms, and macrophytes
  } else {

    if(isTRUE(parallel)){

      dfout <- parSapply(clusters, gettaxa_final, .getdata, token = token, harmztaxa = harmztaxa, url = qurl,
                         simplify = FALSE, USE.NAMES = TRUE)

      stopCluster(cl = clusters)

      return(dfout)

    }else{

      dfout <- sapply(gettaxa_final, .getdata, token = token, harmztaxa = harmztaxa, url = qurl,
                      simplify = FALSE, USE.NAMES = TRUE)
    }
  }
}



#' @title Data download from the Freshwaterecology.info database.
#'
#' @details
#' The \code{getdata} is used as a wrapper to enable the user to download data from multiple groups
#' and allowing parallelising the \code{\link{getfiles}} function. The user can change the number of cores to improve the speed of
#' data downloaded. All data downloaded in this function by the user  is cached in the \code{taxongroups} folder, which
#' is automatically created in the working directory when loading the package. The data is permanently cached, even when
#' the session is restarted. The user can use the \code{\link{fw_decache}} function to remove the files. However, to extract the data
#' please check both \code{\link{extract_traits}} and \code{\link{fw_ecoparameters}}.
#'
#'
#' @inheritParams getfiles
#' @param multiple \code{logical}. Either \code{TRUE} if multiple taxa groups are considered. Default is \code{FALSE} for only
#'        single taxon group.
#' @param parallel \code{logcial}. Either \code{TRUE} to allow parallelisation most especially if multiple groups are considered.
#'      If multiple is set to TRUE and parallel to FALSE, a slow mode of data download will be executed.
#' @param cores \code{interger}. The number computer cores to be used to run while data download and its necessary only when
#'      parallel is set to \code{TRUE}. Default is 2.
#' @param quietly \code{logical}. If \code{FALSE}, a message showing the slow mode of data collation
#'      will be presented to the user. Default \code{TRUE}.
#'
#' @importFrom parallel detectCores makeCluster clusterExport stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom utils askYesNo
#'
#' @return \code{list}. Lists of taxa groups data downloaded from the Freshwater Information Platform.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ppdata <- 1
#' }
#'
collatedata <- function(taxa, ecotraits = NULL, taxaorder = NULL, token  = NULL, warn = FALSE,
                    multiple = FALSE, parallel = FALSE, cores = 2,
                    quietly = FALSE){

   if(isFALSE(multiple)){

     if(length(taxa)>1) stop("One taxa group to be downloaded if multiple is FALSE")

     getdf <- getfiles(taxa = taxa, ecotraits = ecotraits, taxaorder = taxaorder, token = token, warn = warn, parallel = parallel, cores = cores)
     return(getdf)
   }else{

     if(length(taxa)<=1)stop("If multiple is TRUE, multiple taxagroups are expected.")

     if(isFALSE(parallel)){

       if(isFALSE(quietly)) message("The slower version of getting data from FW will be implemented.")

       taxadflist <- list()

       for (i in taxa) {
         taxadflist[[i]] <- getfiles(taxa = i,  ecotraits = ecotraits, taxaorder = taxaorder, token = token, warn = warn, parallel = parallel, cores = cores)
       }
       return(taxadflist)
     }else{

       #check allowed cores

       ncdetect <- detectCores()

       if(is.na(ncdetect) | is.null(ncdetect) | ncdetect<=1) stop("The cores detected are either NA, NULL or less to 1, please use the slower version.")

       if(cores>=ncdetect | (ncdetect-cores)<2) stop("Reduce the cores to ", ncdetect-2," or less for effective parallelisation.")

       makecluster <- makeCluster(spec = cores, type = 'PSOCK')

       registerDoParallel(cl = makecluster)


       getdf <- foreach(i = taxa, .packages ="fwtraits") %dopar% {

         taxadf <- getfiles(taxa = i, ecotraits = ecotraits,  taxaorder = taxaorder, token = token, warn = warn, parallel = parallel, cores = cores)

         return(taxadf)
       }

       stopCluster(makecluster)

       return(getdf)
     }
   }
 }



#' @title To get the allowed macroinvertebrates in the Freshwaterecology.info.
#'
#' @return \code{vector}. A vector of allowed macro invertebrates taxonomic groups that can be set in the \code{taxaorder} parameter while
#'        while getting data.
#'
#' @export
#'
#' @examples
#'
#' x <- fw_orders()
#'
fw_orders <- function(){

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


