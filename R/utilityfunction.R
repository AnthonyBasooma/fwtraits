#' @title Steps to follow in using the Freshwater Infromation Platform
#'
#' @return \code{list} steps to follow in using the FIP
#'
#' @export
#'
#'
#' @examples
#'
#' b4us <- before_u_start()
#'
#' @author Anthony Basooma
#'
before_u_start <- function() {
  step1 <- "Step 1: Register with in the Freshwater Information Platform at https://www.freshwaterecology.info/register/index.php"

  step2 <- "Step 2: Apply for the API Key by sending your details at (irv@irv-software.at) or (ask@boku.ac.at) "

  step3 <- "Step 3: Copy and paste the API key by running fip_token(), which will prompt for the key"

  step4 <- "Congs: read to interact with Freshwater Information Platform will appear."

  return(list(step1, step2, step3, step4))
}



#' @title Function to retrieve all the traits in the database.
#'
#' @return \code{list} List of traits for all the taxa groups and orders in the database.
#'
#' @export
#'
#' @examples
#'
#' x <- fip_paramlist()
#'
#' @author Anthony Basooma
#'
fip_paramlist <- function() {

  # base url with parameter list

  paramurl <- "https://www.freshwaterecology.info/fweapi2/v1/getecoparamlist"

  paramlist <- request(base_url = paramurl) |>
    req_perform() |>
    resp_body_json()

  return(paramlist)
}

#' @noRd
#'
fip_classes <- function(paramlist) {
  av <- sapply(paramlist, function(x) strsplit(x[[7]], split = ", ", fixed = TRUE)[[1]])

  txall <- unique(do.call(c, av))

  txallfinal <- txall[!txall %in% c("all classes")]

  return(txallfinal)
}
