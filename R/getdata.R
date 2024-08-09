#' @noRd

harmonisetaxa <- function(tx, taxafile = FALSE) {
  if (tx == "fi" || tx == "fish" || tx == "fishes") {
    if (isTRUE(taxafile)) taxa <- "fi" else taxa <- "fishes"
  } else if (tx == "pp" || tx == "phyto" || tx == "phytoplankton") {
    if (isTRUE(taxafile)) taxa <- "pp" else taxa <- "phytoplankton"
  } else if (tx == "mi" || tx == "macro" || tx == "macroinvertebrates") {
    if (isTRUE(taxafile)) taxa <- "mi" else taxa <- "macroinvertebrates"
  } else if (tx == "di" || tx == "diatoms") {
    if (isTRUE(taxafile)) taxa <- "di" else taxa <- "diatoms"
  } else if (tx == "pb" || tx == "phytobentho" || tx == "bentho") {
    if (isTRUE(taxafile)) taxa <- "pb" else taxa <- "phytobentho"
  } else if (tx == "mp" || tx == "macrophyte" || tx == "grasses") {
    if (isTRUE(taxafile)) taxa <- "mp" else taxa <- "macrophyte"
  } else {
    stop("Incorrect taxanomic group are entered. Use 'pp', 'fi', 'mi','di', 'pb', and 'mp' or run taxonomicgroups()")
  }
  return(taxa)
}




#' Title
#'
#' @param taxa
#' @param token
#'
#' @importFrom httr2 request req_body_raw  req_perform resp_body_json req_auth_bearer_token
#' @importFrom jsonlite toJSON
#'
#' @return
#' @export
#'
#' @examples


getfiles <- memoise::memoise(function(taxa, taxaorder = NULL, token = NULL, seed = 1144) {

  if (!curl::has_internet()) stop("Not connected on internet to access the database.")

  if(is.null(token)) stop("Provide the token key to continue, run before_u_start() function and learn to set the token.")

  #set seed for memoising the downloads even when the token chnage
  set.seed(seed)

  # get database map
  getparam_list <- fip_paramlist()

  # extract parameters
  ecolist <- getparam_list$ecologicalParameterList # requires only fi, mi, pb, pp, pd, di

  # get for each taxa

  taxaharmonised <- harmonisetaxa(tx = taxa, taxafile = TRUE)

  gettaxa <- ecolist[[taxaharmonised]]


  # get the base url for the taxa data tables
  qurl <- "https://www.freshwaterecology.info/fweapi2/v1/query"

  if (taxaharmonised == "mi" | taxaharmonised == "pb") {
    # extract data for macro invertebrates and phytobenthos

    # get all list available for traits
    allorders <- sapply(gettaxa, function(x) strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]])

    traitcodes <- sapply(gettaxa, function(x) x[[1]])

    # merge all lists and make them unique to get the orders or classes (phytobenthos)
    names(allorders) <- traitcodes

    unique_ORDERS <- unique(do.call(c, allorders))

    # get the traitcodes for each order

    codematrix <- matrix(NA, nrow = length(unique_ORDERS), ncol = length(allorders))

    for (v in seq_along(unique_ORDERS)) {

      order <- unique_ORDERS[v]

      for (vi in seq_along(allorders)) {

        lstoerds <- unlist(allorders[vi])

        cnames <- as.numeric(names(allorders[vi]))

        if (order %in% lstoerds == TRUE) codematrix[v, vi] <- cnames else codematrix[v] <- NA
      }
    }

    row.names(codematrix) <- unique_ORDERS

    # extract all matrix rows, remove NAs and maintain the ecoparams for each taxa

    taxafinal0 <- apply(codematrix, 1, FUN = function(x) unlist(x)[!is.na(unlist(x))])

    taxafinal <- taxafinal0[sapply(taxafinal0, length) > 0]

    #macro are selected bse require to loop through the traits vs phtobenthos that allows combining them without the 403 error
    if (taxaharmonised == "mi") {

      # remove orders that are not in available for list

      orderlist <- within(taxafinal, rm(Araneae, Kamptozoa))

      if(is.null(taxaorder)) stop("For macroinvertebrates, provide the taxa orders, run orders() function to get allowed orders.")

      if (length(unique(taxaorder)) > 4) stop("Please a maximum of 4 macroinvertebrate orders are allowed but ", length(unique(taxaorder)), " have been provided.")

      #check if the order entered are in the allowed list

      inOut <- taxaorder%in%orderlist
      if(all(inOut) == TRUE){

        taxasel <- orderlist[taxaorder]
      }else{
       ordersnotin <- taxaorder[which(inOut==FALSE)]

       stop("The orders, ", ordersnotin, " is/are not in the standard order list for macroinvertebrates. run orders() function to identify allowed orders.")
      }
      xsp <- sapply(names(taxasel), function(x) {

        ordata <- taxasel[[x]]

        #loop through the trait codes not as a group: first unlist

        xdata <- sapply(unlist(ordata), function(y) {

          ldata <- request(base_url = qurl) |>

            req_auth_bearer_token(token = tokencode) |>

            req_body_raw(body = jsonlite::toJSON(
              list(
                organismgroup = taxaharmonised, taxagroup = x,

                displayValues = list(ecologicalparameter = y)
              ),
              auto_unbox = TRUE
            ), type = "application/json") |>

            req_perform()

          fxdata <- ldata |> httr2::resp_body_json()

          lmdata <- as.data.frame(do.call(rbind, fxdata$searchResult))

        }, USE.NAMES = TRUE, simplify = FALSE)
      })#end of macro inverts

    } else {#start phyto benthos
      taxasel <- taxafinal

      xdata <- sapply(names(taxasel), function(x) {

        codes <- taxasel[[x]]

        ldata <- request(base_url = qurl) |>

          req_auth_bearer_token(token = tokencode) |>

          req_body_raw(body = jsonlite::toJSON(
            list(
              organismgroup = taxaharmonised, class = x, #they are classes not taxagroup

              displayValues = list(ecologicalparameter = codes)
            ),
            auto_unbox = TRUE
          ), type = "application/json")

        reqdata <- ldata |> req_perform()

        fxdata <- reqdata |> resp_body_json()

        phytobenthos <- as.data.frame(do.call(rbind, fxdata$searchResult))

      }, simplify = FALSE, USE.NAMES = TRUE)
    }#end phytobentho

    #start fish, phytoplankton, diatoms, and macrophytes
  } else {
    sapply(gettaxa, function(x) {
      traitcodes <- x[["code"]]

      traitnames <- x[["name"]]

      available4 <- strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]]

      # replace the all classes with all the classes

      if ("all classes" %in% available4) available4 <- fip_classes(paramlist = gettaxa) else available4

      ldata <- request(base_url = qurl) |> req_auth_bearer_token(token = token)

      # harmonise taxa

      if (taxaharmonised == "fi") {
        extdata <- ldata |>
          req_body_raw(body = toJSON(
            list(
              organismgroup = taxaharmonised, taxagroup = available4,
              displayValues = list(ecologicalparameter = traitcodes)
            ),
            auto_unbox = TRUE
          ), type = "application/json")
      } else {
        # mp, “pb”, “dia” and “pp” are at class level vs fi and mi-order
        extdata <- ldata |>
          req_body_raw(body = toJSON(
            list(
              organismgroup = taxaharmonised, class = available4,
              displayValues = list(ecologicalparameter = traitcodes)
            ),
            auto_unbox = TRUE
          ), type = "application/json")
      }

      reqdata <- extdata |> req_perform()

      respdata <- reqdata |> resp_body_json()

      finaldata <- as.data.frame(do.call(rbind, respdata$searchResult))

      attr(finaldata, "traitname") <- traitnames


      return(finaldata)
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
}, cache = memoise::cache_filesystem(path = "taxongroups", compress = TRUE))




orders <- function(){

  getparam_list <- fip_paramlist()

  # extract parameters
  ecolist <- getparam_list$ecologicalParameterList$mi

  allorders <- sapply(ecolist, function(x) strsplit(x[["availableFor"]], split = ", ", fixed = TRUE)[[1]])

  ordernames <- unique(do.call(c, allorders))

  orderlist <- ordernames[!ordernames %in% c('Kamptozoa','Araneae')]

  return(orderlist)
}



#' Title
#'
#' @param taxa
#' @param taxaorder
#' @param token
#' @param multiple
#' @param parallel
#' @param cores
#'
#' @importFrom parallel detectCores makeCluster clusterExport stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#'
#' @return
#' @export
#'
#' @examples
getdata <- function(taxa, taxaorder = NULL, token, multiple = FALSE, parallel = FALSE, cores = NULL, seed= 1144){

  if(isFALSE(multiple)){

    if(length(taxa)>1) stop("One taxa group to be downloaded if multiple is FALSE")

    getdf <- getfiles(taxa = taxa, taxaorder = taxaorder, seed = seed, token = token)

  }else{

    if(length(taxa)<=1)stop("If multiple is TRUE, multiple taxagroups are expected.")

    if(isFALSE(parallel)){

      message("The slower version of getting data from FIP will be implemented.")

      taxadflist <- list()

      for (i in taxa) {
        taxadflist[[i]] <- getfiles(taxa = i, taxaorder = taxaorder)
      }

    }else{

      if(is.null(cores))stop("For parallel downloading of taxa groups, please set the number of cores.")

      #check allowed cores

      ncoresdetected <- detectCores()

      if(cores>=ncoresdetected) stop("Reduce the cores to ", ncoresdetected-2," or less for effective parallelisation.")

      makecluster <- makeCluster(spec = cores, type = 'PSOCK')

      #clusterExport(makecluster, varlist = c("token"))

      registerDoParallel(cl = makecluster)


      getdf <- foreach(i = taxa, .packages ="fwtraits") %dopar% {

        taxadf <- getfiles(taxa = i,  taxaorder = taxaorder, token = token)

        #on.exit(close(taxadf), add = TRUE)

      }

      stopCluster(cl= makecluster)

      # Close all open connections
      closeAllConnections()
    }
  }

}

# usethis::use_package('foreach')
#
# fip_decache <- function(taxa, folder = "taxongroups", quietly = TRUE) {
#   unlikefolder <- absolutepath(dir = folder, verbose = FALSE)
#
#   cat("0 - No \n1 - Yes\n")
#
#   check <- readline(prompt = "Are you sure you want to remove the archived files ? ")
#
#
#   if (as.integer(check) == 1) {
#     for (i in taxa) {
#       tout <- harmonisetaxa(tx = i)
#
#       unlink(file.path(unlikefolder, paste0(tout, ".RData")))
#     }
#
#     if (isFALSE(quietly)) message(taxa, " removed from the archiving folder.")
#   } else {
#     if (isFALSE(quietly)) ("Files not removed.")
#   }
# }










