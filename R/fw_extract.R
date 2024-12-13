
#' @title Extracting the traits from the downloaded data.
#'
#' @inheritParams fw_searchdata
#' @inheritParams clean_names
#' @param data \code{vector}. The list or vector with species names for which ecological references needs to be extracted from the
#'         database.
#'
#' @importFrom methods is
#' @importFrom R.cache addMemoization
#'
#' @return \code{dataframe} A dataframe species traits for all orders.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'dfextract <- fw_split(data = "Abramis brama", organismgroup = 'fi', inform = TRUE,
#'                             ecoparams = 'migration', cachefolder = 'cache' )
#'
#' }
#'

fw_split <- function(data,
                     organismgroup,
                     ecoparams = NULL,
                     apikey  = NULL,
                     seed = 1134,
                     secure = TRUE,
                     pct = 80,
                     errorness = 20,
                     warn = FALSE,
                     inform = FALSE,
                     cachefolder = NULL) {


  #if multiple groups are considered

  cabbr <- c()
  cvalue <- c()
  cvaluedescription <- c()
  spp <- c()
  dlist <- list()
  dftraits <- list()
  dspecies <- list()
  groupData <- list()
  spdetails <- list()

  datalists <- fw_searchdata(organismgroup = organismgroup,
                                    ecoparams = ecoparams,
                                    refdata  = data,
                                    apikey =   apikey,
                                    secure = secure,
                                    seed = seed,
                                    warn = warn,
                                    inform = inform,
                                    cachefolder = cachefolder)
  #check if the lists returned data

  for (ii in seq_along(datalists)) {

    taxanames <- names(datalists)[ii]

    groupdata <- datalists[[ii]]

    # get only the organism group list

    if(is(data, 'list')) speciesin <- data[[taxanames]] else speciesin <- data

    # cleaned species names
    spdetails[[ii]] <- clean_names(sp = speciesin, grouplists = groupdata,
                                   pct = pct, errorness = errorness,
                                   group = tcheck(taxanames),
                                   full = TRUE,
                                   warn = warn)

    specol_cleaned <- spdetails[[ii]]$ clean[!is.na(spdetails[[ii]]$clean)]

    #check if there are species to check
    if(length(specol_cleaned)>=1){
      specol_cleaned
    } else{

      warning("No species name to extract the traits for the ", tcheck(taxanames), " and will be skipped. ", call. = FALSE)
      next
    }

    #form order dataframe rather than lists

    taxafile <- tcheck(tx = taxanames)

    for (iii in seq_along(groupdata)) {

      #data filtered for each trait with each orders available forms

      traitorderdf <- groupdata[[iii]]

      if(!is.null(traitorderdf)) traitorderdf else next

      if(nrow(traitorderdf)>=1) { #for phytobentho that returns no data

        #creates a complete species name on the data frame

        traitorderdf["speciesname"] <- paste0(traitorderdf$Genus,' ', traitorderdf$Species)

        #filter out the species being searched for by the user

        rowsdata <- traitorderdf[traitorderdf[, "speciesname"] %in% specol_cleaned, ]

        #even within this some fi, pp, mp, di returns no data so skip to next

        if (nrow(rowsdata) >= 1) pp1 <- rowsdata else next

        pp2 <- pp1#

      }else{

        next #skip to next
      }

      for (iv in 1:nrow(pp2)) {

        pp3 <- pp2[iv, "ecologicalParameterList"][[1]]

        spp <- unlist(pp2[, "speciesname"])[iv]

        for (v in seq_along(pp3)) {

          traitslist <- pp3[[v]][[3]]

          cname <- pp3[[v]][[2]]

          abbr <- sapply(traitslist, function(x) x[[1]]) # extract all the species parameter names

          val <- sapply(traitslist, function(x) x[[2]]) # extract all the species parameter values

          if (is.null(unlist(val)) == TRUE) { # some diatom had no values

            cvalue[v]  <- "nodata" # extracts the value

            cvaluedescription[v]  <- 'None' # extracts the value description

            if (length(abbr) > 1) cabbr[v]  <- clean_traits(x = cname) else cabbr[v]  <- clean_traits(x = abbr) # for diatoms with no values in all parameters

          } else if (length(abbr) == 1) {

            # if length = 1 then the val is the parameter to maintain since its the output
            #if the value has an empty string
            if(val !="") cvalue[v]  <- val else cvalue[v]  <- "nodata"

            #clean the traits name before saving
            cabbr[v]  <- clean_traits(x = abbr)

            cvaluedescription[v]  <- clean_traits(x = abbr)

          } else {

            valIndesc <- abbr[which(val != "0" & !is.null(val) & nzchar(val))] # maintain those with values, no 0, NA and empty strings

            valIn <- val[which(val != "0" & !is.null(val) & nzchar(val))]

            # value description
            if (length(valIndesc) <= 0) valdescript <- "nodata" else valdescript <- paste(valIndesc, collapse = "_ ") # concatenate if more than 1

            tfdescript <- grepl("&lt;", valdescript, fixed = TRUE)

            if (tfdescript == TRUE) cvaluedescription[v]  <- gsub("&lt;", "<", valdescript) else cvaluedescription[v]  <- valdescript

            # get also the values
            if (length(valIn) <= 0) valInf <- "nodata" else valInf <- paste(valIn, collapse = "_ ") # concatenate if more than 1

            tf <- grepl("&lt;", valInf, fixed = TRUE)

            if (tf == TRUE) cvalue[v] <- gsub("&lt;", "<", valInf) else cvalue[v]  <- valInf

            cabbr[v]  <- clean_traits(x = cname)
          }
          dfout <- data.frame(taxagroup = taxafile, species = spp, parameter = cabbr,
                              description = cvaluedescription, value = cvalue)

        }
        dspecies[[iv]] <- dfout

        df1 <- do.call(rbind, dspecies)

      }

      dftraits[[iii]] <- df1

      df2 <- do.call(rbind, dftraits)

    }
    groupData[[ii]] <- df2

    dfinal <- do.call(rbind, groupData)

  }
  return(dfinal)
}


fw_extract <- addMemoization(fw_split)
