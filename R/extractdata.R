
#' @noRd
trans_macrodata <- function(m){
  md <- list()
  for (i in seq_along(m)) {
    md[[i]] <- m[[i]]
    d <- do.call(c, md)
  }
  invisible(d)
}



#' Title
#'
#' @param species
#' @param group
#' @importFrom methods is
#'
#' @return
#' @export
#'
#' @examples
#'
extract_traits <- function(species, group, taxaorder = NULL,
                            token  = NULL, multiple = FALSE, parallel = FALSE,
                            cores = NULL, quietly = FALSE,
                            pct = 80,
                            errorness = 20) {


  #if multiple groups are considered

  cabbr <- c()
  cvalue <- c()
  cvaluedescription <- c()
  spp <- c()
  dlist <- list()
  dftraits <- list()
  dspecies <- list()
  grouplist <- list()

  for (ii in seq_along(group)) {

    # check if the taxa group is being downloaded for the first time to tell the user to wait
    # get the harmonized taxa name
    taxafile <- harmonisetaxa(group[ii])

    taxagroup_data <- getdata(taxa = group[ii], taxaorder = taxaorder,
                              token  = token, multiple = multiple,
                              parallel = parallel, cores = cores,
                              quietly = quietly)

    # # cleaned species names

    speclean <- clean_names(sp = species, grouplists = taxagroup_data,
                            pct = pct, errorness = errorness, group = taxafile )

    #only get unique species
    specleaned <- unique(unlist(speclean))

    #check if there are species to check
    if(length(specleaned)>=1) specleaned else stop("No species name to extract the traits.")

    #reshape the macroinvetebrates data if more than 1 taxorder is selected.
    #form order dataframe rather than lists

    if(taxafile=='macroinvertebrates' && is(taxagroup_data, 'list')){

      taxagroup_data_final <- trans_macrodata(taxagroup_data)
    }else{
      taxagroup_data_final <- taxagroup_data
    }

    for (iii in seq_along(taxagroup_data_final)) {

      #data filtered for each trait with each orders available fors

      traitorderdf <- taxagroup_data_final[[iii]]

      if(nrow(traitorderdf)>=1) { #for phytobento that returns no data

        #creates a complete species name on the data frame

        traitorderdf["speciesname"] <- paste0(traitorderdf$Genus,' ', traitorderdf$Species)

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
          dfout <- data.frame(taxagroup = taxafile, species = spp, parameter = cabbr, description = cvaluedescription, value = cvalue)
        }
        dspecies[[iv]] <- dfout

        df1 <- do.call(rbind, dspecies)

      }

      dftraits[[iii]] <- df1

      df2 <- do.call(rbind, dftraits)
    }
    grouplist[[ii]] <- df2

    dfinal <- do.call(rbind, grouplist)

  }
  return(dfinal)
}
