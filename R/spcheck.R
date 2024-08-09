#' Title
#'
#' @param taxadb
#' @param species
#' @param quietly
#' @param taxalist
#' @param errror_on_names
#'
#' @return
#' @export
#'
#' @examples
#'
spcheck <- function(taxadb, species = NULL, check = FALSE, quietly = TRUE, taxalist = FALSE,
                    errror_on_names = TRUE, grname = NULL) {


  txname <- unlist(unique(do.call(c, sapply(taxadb, function(xx) xx$Taxonname))))

  if (isTRUE(taxalist)) {

    #create a standard dataframe
    if(isTRUE(check)){



    }else{
      return(txname)
    }

  } else {
    # clean the standard taxa list

    tf <- species %in% txname

    if (all(tf) == FALSE) {

      if (isFALSE(errror_on_names)) {
        # error_on_names and continue with the one which are true

        spfinal <- species[which(tf == TRUE)]

        # for one species if FALSE then spfinal will be zero

        if (length(spfinal) < 1) {
          stop("All species are missing in the standard taxa list of ", grname, " in the database.", call. = FALSE)
        } else {
          spfinal

          if (isFALSE(quietly)) message("Species found in the standard taxa list.")
        }
      } else {
        # get species which missing in the main taxa list to give an informed error

        spout <- species[which(tf == FALSE)]

        stop("Species ", paste(spout, collapse = ","), " is/are missing in the standard taxa list.")
      }
    } else {
      # all species are in the data
      spfinal <- species
    }
    return(spfinal)
  }
}
