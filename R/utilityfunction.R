#' Title
#'
#' @return
#' @export
#'
#' @examples
before_u_start <- function() {
  step1 <- "Step 1: Register with in the Freshwater Information Platform at https://www.freshwaterecology.info/register/index.php"

  step2 <- "Step 2: Apply for the API Key by sending your details at (irv@irv-software.at) or (ask@boku.ac.at) "

  step3 <- "Step 3: Copy and paste the API key in loadtoken function by running fip_token()"

  step4 <- "Congs: read to interact with Freshwater Information Platform will appear."

  return(list(step1, step2, step3, step4))
}




#' Title
#'
#' @return
#' @export
#'
#' @examples
fip_paramlist <- function() {
  # base url with parameter list

  paramurl <- "https://www.freshwaterecology.info/fweapi2/v1/getecoparamlist"

  paramlist <- request(base_url = paramurl) |>
    req_perform() |>
    resp_body_json()

  return(paramlist)
}

#' Title
#'
#' @param paramlist
#'
#' @return
#' @export
#'
#' @examples
fip_classes <- function(paramlist) {
  av <- sapply(paramlist, function(x) strsplit(x[[7]], split = ", ", fixed = TRUE)[[1]])

  txall <- unique(do.call(c, av))

  txallfinal <- txall[!txall %in% c("all classes")]

  return(txallfinal)
}


#' Title
#'
#' @param paramlist
#' @param trait
#'
#' @return
#' @export
#'
#' @examples
abbrtoname <- function(paramlist, trait) {
  plist <- paramlist[["ecologicalParameterList"]][["pp"]][[trait]] # trait number

  traitname <- plist[["name"]]

  traitlist <- plist[["categoryList"]]

  cName <- c()
  cAbbr <- c()
  cExpl <- c()

  for (ii in seq_along(traitlist)) {
    cName[ii] <- traitlist[[ii]][["categoryName"]]
    cAbbr[ii] <- traitlist[[ii]][["categoryAbbr"]]
    cExpl[ii] <- traitlist[[ii]][["categoryExplanation"]]
  }
  return(list(cname = cName, cabbr = cAbbr, cexplain = cExpl, traitname))
}




#' @title obtain absolute path for the user
#'
#' @param dir to user the user directory to save data for future use.
#' @param verbose to show messages during implementation or not. Default \code{FALSE}.
#'
#' @return absolute path
#'
absolutepath <- function(dir, verbose = TRUE) {
  gwd <- getwd()

  folder <- paste0(gwd, "/", dir)

  if (dir.exists(folder) == FALSE) {
    if (isTRUE(verbose) == TRUE) message("New directory ", dir, " formed")

    dir.create(dir)

    pathabs <- paste0(gwd, "/", dir)
  } else {
    if (isTRUE(verbose) == TRUE) message(dir, " already present")

    pathabs <- folder
  }

  return(pathabs)
}


#' @title create sub folders in the absolute path folder
#'
#' @param x is the absolute path set in .abspath
#' @param var is the name of the folder with specific variables.
#'
#' @return sub folder in the absolute path

absoluteinner <- function(x, var) { # x is the absolute path from absolutepath function

  folder <- paste0(x, "/", var)

  # if the folder doesn't exist, then a new one will be created to store data.

  if (dir.exists(folder) == FALSE) {
    dir.create(folder)

    px <- paste0(x, "/", var)
  } else {
    px <- folder
  }

  return(px)
}

#' @title  caching data
#'
#' @param x to indicate absolute path.
#'
#' @importFrom memoise memoise
#'
#'
cachefiles <- function(x) {
  # to allow caching the data in the particular folder.

  abpath <- absolutepath(x, verbose = F)

  d <- memoise::cache_filesystem(abpath)

  return(d)
}
