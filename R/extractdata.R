#' Title
#'
#' @param species
#' @param group
#'
#' @return
#' @export
#'
#' @examples
extract_traits <- function(species, group) {


  # check if the taxa group is being downloaded for the first time to tell the user to wait
  # get the harmonized taxa name
  taxafile <- harmonisetaxa(group)

  taxapath <- list.files(
    path = file.path(absolutepath(dir = "taxongroups", verbose = FALSE)),
    pattern = paste0(taxafile, ".RData"), full.names = F
  )

  if (length(taxapath) < 1) {
    message("Please wait to download the ", taxafile, " data from the database.")

    taxagroup_data <- getdata(taxa = group)
  } else {
    taxagroup_data <- getdata(taxa = group)
  }

  # # cleaned species names

  speclean <- clean_names(sp = species, grouplists = taxagroup_data)

  #only get unique species

  specleaned <- unique(speclean)

  cabbr <- c()
  cvalue <- c()
  cvaluedescription <- c()
  spp <- c()
  dlist <- list()
  dftraits <- list()

  for (iii in seq_along(taxagroup_data)) {

    #data filtered for each trait with each orders available fors

    traitorderdf <- taxagroup_data[[iii]]

    #creates a complete species name on the data frame

    traitorderdf["speciesname"] <- paste0(traitorderdf$Genus,' ', traitorderdf$Species)


    if(nrow(traitorderdf)>=1) { #for phytobento that returns no data

      #filter out the species being searched for by the user

      rowsdata <- traitorderdf[traitorderdf[, "speciesname"] %in% specleaned, ]


      #even within this some fi, pp, mp, di returns no data so skip to next

      if (nrow(rowsdata) >= 1) pp1 <- rowsdata else next

      #remove any duplicate species

      pp2 <- as.data.frame(pp1[!duplicated(pp1[c('speciesname')]),])#add more paramaters

    }else{
      next #skip to next
    }

    for (iv in 1:nrow(pp2)) {

      if(taxafile=="phytobentho"){

        #change the traits
        traitslist <- pp2[iv, "ecologicalParameterList"][[1]][[iv]][[3]] # get the parameter values


        cname <- pp2[iv, "ecologicalParameterList"][[1]][[iv]][[2]] # get the parameter name

      }else{
        #the dataframe only changes in the first loop

        traitslist <- pp2[iv, "ecologicalParameterList"][[1]][[1]][[3]] # get the parameter values

        cname <- pp2[iv, "ecologicalParameterList"][[1]][[1]][[2]] # get the parameter name
      }

      # get the species name

      spp[iv] <- unlist(pp2[, "speciesname"])[iv]


      abbr <- sapply(traitslist, function(x) x[[1]]) # extract all the species parameter names

      val <- sapply(traitslist, function(x) x[[2]]) # extract all the species parameter values


      if (is.null(unlist(val)) == TRUE) { # some diatom had no values

        cvalue[iv] <- NA # extracts the value

        cvaluedescription[iv] <- 'None' # extracts the value description

        if (length(abbr) > 1) cabbr[iv] <- clean_traits(x = cname) else cabbr[iv] <- clean_traits(x = abbr) # for diatoms with no values in all parameters

      } else if (length(abbr) == 1) {


        # if length = 1 then the val is the parameter to maintain since its the output
        #if the value has an empty string
        if(val !="") cvalue[iv] <- val else cvalue[iv] <- 'Nd'

        #clean the traits name before saving
        cabbr[iv] <- clean_traits(x = abbr)

        cvaluedescription[iv] <- clean_traits(x = abbr)

      } else {

        valIndesc <- abbr[which(val != "0" & !is.null(val) & nzchar(val))] # maintain those with values, no 0, NA and empty strings

        valIn <- val[which(val != "0" & !is.null(val) & nzchar(val))]

        # value description
        if (length(valIndesc) <= 0) valdescript <- NA else valdescript <- paste(valIndesc, collapse = "_ ") # concatenate if more than 1

        tfdescript <- grepl("&lt;", valdescript, fixed = TRUE)

        if (tfdescript == TRUE) cvaluedescription[iv] <- gsub("&lt;", "<", valdescript) else cvaluedescription[iv] <- valdescript

        # get also the values
        if (length(valIn) <= 0) valInf <- NA else valInf <- paste(valIn, collapse = "_ ") # concatenate if more than 1

        tf <- grepl("&lt;", valInf, fixed = TRUE)

        if (tf == TRUE) cvalue[iv] <- gsub("&lt;", "<", valInf) else cvalue[iv] <- valInf

        cabbr[iv] <- clean_traits(x = cname)

      }
      dlist <- data.frame(species = spp, parameter = cabbr, description = cvaluedescription, value = cvalue)
    }

    dftraits[[iii]] <- dlist

    dfinal <- do.call(rbind, dftraits)
  }
  return(dfinal)
}
