
#' @title Check and clean species names to match standard names in the database.
#'
#' @param sp \code{string or vector}. Species scientific names to be checked. Although the spellings are checked, the users
#'      should check for the species name provided to avoid not being being detected in the database.
#' @param grouplists \code{list}. List of data downloaded in the \code{\link{fw_searchdata}} function. If species considered in \code{sp}
#'      parameter are fishes, then the fishes lists should be provided otherwise the species names will be rejected.
#' @param percenterror \code{numeric}. The number used as a cutoff to infer similarity of the user provided name and what is found in the database.
#'        The higher the percentage, the higher the similarity the species name provided by the user and the one in the database. \code{percenterror}
#'        ranges from 0 to 100 but the default is 80 to ensure that wrong names are not selected at low similarity percentage cutoff.
#' @param errorness \code{numeric} Similar to \code{percenterror}, \code{errorness} parameter uses the distance differences between the user-provided
#'        names and all the taxa group species standard names. The lower the percentage error, the higher the similarity in the species
#'        names provided. Default is 20 and beyond 30, a warning is showed to avoid wrong species replace the user provided name, which leads
#'        to extracting wrong traits.
#' @param full \code{logical} \code{TRUE} if a dataframe with both cleaned and uncleaned species are required. If \code{FALSE} then the
#'       a species list will be produced after cleaning. Default \code{FALSE}.
#' @param taxalevel \code{string} Allowed taxonomic levels at which data can retrieved. Default is \code{'species'} but data can also be downloaded at family level,
#'        genus, and taxa group level.
#' @param prechecks,standard_dataset \code{logical}. If \code{TRUE} the standard prechecks will be done on both the invertebrates
#'        and bentho species names before search for ecological parameters from the database. The
#'        standard names is provided with the dataset to reduce on the time in identifying the
#'        standard tyxonomic names for the macroinvertebrates in the database.
#'        @param taxalevel \code{string} Allowed taxonomic levels at which data can retrieved. Default is \code{'species'} but data can also be downloaded at family level,
#'        genus, and taxa group level.
#' @param warn To alert user on the species names cleaning errors and warnings.
#'
#' @importFrom utils adist
#'
#' @return \code{vector or string} clean species name that is also found in the database.
#'
#'
clean_names <- function(sp,
                        grouplists,
                        prechecks = FALSE,
                        standard_dataset = NULL,
                        percenterror = 80,
                        errorness = 30,
                        full = FALSE,
                        warn,
                        taxalevel) {

  if(isTRUE(prechecks)){

    tlevels <- switch (taxalevel, species ='Taxon', family='Family', taxagroup = 'Taxagroup', genus ='Genus')

    stlist <- unique(unlist(standard_dataset[,tlevels]))

  } else{
    if(taxalevel=='species'){

      stlist <-  unlist(unique(Reduce(c, sapply(grouplists, function(xx) paste0(xx$Genus," ", xx$Species)))))
    }else{
      tlevels <- switch(taxalevel, genus ='Genus', taxagroup = 'TaxaGroup', family ='Family')

      stlist = unlist(unique(Reduce(c, sapply(grouplists, function(xx) xx[, tlevels]))))

    }
  }

  #get unique only
  spunique <- unique(sp)

  #run through the species list

  spcleandata <- sapply(spunique, function(mm){

    # convert all letters to lower
    tlw <- tolower(mm)
    # remove accents
    actr <- iconv(tlw, from = "UTF-8", to = "ASCII//TRANSLIT")

    sppt <- gsub("[[:punct:]]", "", actr)

    spc <- gsub("[^[:alnum:]]", " ", sppt)

    spc1 <- gsub("[[:digit:]]+", "", spc)

    spaces <- trimws(gsub("\\s+", " ", spc1), which = "both")

    spclean <- gsub("(^[[:alpha:]])", "\\U\\1", spaces, perl = TRUE)

    inOut <- spclean%in%stlist

    if(inOut==TRUE) {

      spch <- spclean

    }else{
      #reduce the species to get only the species name
      if(taxalevel=='species'){

        wc <- length(unlist(strsplit(spclean," ")))

        if(wc>2) taxaclean <- paste0(unlist(strsplit(spclean, " "))[1:3], collapse = ' ') else taxaclean <- paste0(unlist(strsplit(spclean, " "))[1:2], collapse = ' ')

      }else{
        taxaclean <- spclean
      }
      #second attempt to look for right name: time consuming: provide a better name

      inOut2 <- taxaclean%in%stlist

      if(inOut2==TRUE){

        spch <- taxaclean

      }else{
        #check if there is a similar name
        dst <- adist(taxaclean, stlist, ignore.case = TRUE)

        mindist <- min(dst)

        #complete match but with cases ignored
        if(mindist==0){
          #get similar name indices
          nind <- which(dst==0)

          spch <- if(nind==1) stlist[nind] else stlist[nind[1]] #select only 1

        }else if(mindist>0){

          distdiff <- (mindist/nchar(taxaclean))*100

          if(distdiff >=30 ){

            if(isTRUE(warn))  warning('No matching taxonomic names will be returned from the database for the searched taxa: ', taxaclean, " at the ", taxalevel, " level.", call. = FALSE)
            spch <- NA

          }else if(distdiff <= errorness){

            spsel <- stlist[which.min(dst)]

            if(length(spsel)==1) {

              taxacorrectnesspct <- (nchar(spsel)/nchar(taxaclean))*100

              #check for intersections
              spsel1 <- unlist(strsplit(spsel, ""))

              taxasplit <- unlist(strsplit(taxaclean, ""))

              intdist <- intersect(spsel1, taxasplit)

              intdiff <- length(intdist)/nchar(taxaclean)

              if(taxacorrectnesspct >= percenterror && intdiff >= 0.95) {

                spch <- spsel

              }else {

                if(isTRUE(warn))warning("No matching taxa name found for ", taxaclean, " and will be removed", call. = FALSE)
                spch = NA
              }
            }else if(length(spsel)>1){

              taxacorrectnesspct <- (nchar(spsel[1])/nchar(taxaclean))*100 #just pick the first one

              #check for intersections
              spsel1 <- unlist(strsplit(spsel[1], ""))
              taxasplit <- unlist(strsplit(taxaclean, ""))

              intdist <- intersect(spsel1, taxasplit)

              intdiff <- length(intdist)/nchar(taxaclean)

              if(taxacorrectnesspct >= percenterror && intdiff >= 0.97) {

                spch <- spsel[1]

              }else {
                if(isTRUE(warn))warning("No matching taxa name found for ", taxaclean, " and will be removed", call. = FALSE)
                spch = NA
              }
            }
          }
        }else{
          if(isTRUE(warn))warning("No matching taxa name found for ", taxaclean, " and will be removed", call. = FALSE)
          spch = NA
        }
      }
    }
  }, simplify = TRUE, USE.NAMES = FALSE)

  if(isTRUE(full)){
    if(!is.list(spcleandata)) spp <- spcleandata else spp <- NA

    finalout <- data.frame(raw = spunique, clean = spp)
  }else{

    finalout <- spcleandata[!is.na(spcleandata)]

    if(length(finalout)==0)finalout <- NA else finalout
  }

  return(finalout)
}



#'
#' @noRd
clean_traits <- function(x){

  sapply(unique(x), function(y){

    lowecase <- tolower(y)

    trans <- iconv(lowecase, from = "UTF-8", to = "ASCII//TRANSLIT")

    punctrem <- gsub("[[:punct:]]", "", trans)

    numberrem <- gsub("[^[:alnum:]]", " ", punctrem)

    digtitrms <- gsub("[[:digit:]]+", "", numberrem)

    rmoveof <- gsub("of ", '',
                    gsub('for ', '',
                         gsub('to ', '', digtitrms)))

    spacesrm <- trimws(gsub("\\s+", " ", rmoveof), which = "both")

    lstrings <- unlist(strsplit(spacesrm, " "))

    if(length(lstrings)>=3) {

      tclean <- paste(lstrings[1:3], collapse = ' ')

    } else if(length(lstrings)<3 && length(lstrings)==2){

      tclean <- paste(lstrings[1:2], collapse = ' ')

    }else{
      tclean <- lstrings
    }

    return(tclean)

  }, USE.NAMES = FALSE)
}



#' @title Checks the traits spelling compared to user input.
#'
#' @param x \code{string or vector}. The traits to be checked for spelling errors and matching database entries.
#' @param std \code{lits}. A list with standard traits names from the the database to compare with user entries.
#' @param mindist \code{numeric}. Set a threshold for trait similarity between the user provided and that found in the database.
#'              The lower the percentage, the higher the similarity between the user provided
#'              and standard trait names.
#' @param error \code{numeric}. Also percentage to improve the distance based checked implemented or set in
#'              mindist parameter
#' @param grp  \code{grp}. The taxa names checked for. see \code{\link{fw_searchdata}}.
#' @param warn \code{logical} To show species name warning checks and traits cleaning. Default \code{FALSE}.
#'
#' @return \code{list or string}. A list, vector or string of cleaned traits names based on
#' the user provided and standard database traits for downloading.
#'
#'
checktrait<- function(x, std, mindist = 0.3, error = 0.8, grp = NULL, warn= TRUE){

  sapply(unique(x), function(wd){

    uwd <- clean_traits(wd)

    dst <- adist(uwd, std)

    wrd <- std[which.min(dst)]

    chdd <- nchar(uwd)/nchar(wrd) # measure of errorness

    dd <- (min(dst)-0)/10 #measure of similarity

    if(chdd >= error && dd <= mindist){

      fwd <- wrd

    }else{
      fwd <- NA
      if(isTRUE(warn)) warning("The trait ", wd, " is wrongly spelt and not in ", grp, " traits and is likely to be -", wrd, "- but check fw_dbguide() for appropiate trait name",
                               call. = FALSE)
    }

    fwdlst <- fwd[!is.na(fwd)]

  }, simplify = TRUE, USE.NAMES = FALSE)
}

#' @noRd
str_sentence <- function(x){

  if(length(x)<0) stop('zero length string provided.', call. = FALSE)

  if(length(x)==1) {

    firstCap <- paste0(toupper(strtrim(x, 1)), substring(x, 2))

  }else{
    strout <- unlist(strsplit(x, " "))[1]

    strother <- paste0(unlist(strsplit(strout, " "))[-1], collapse = ' ')

    firstCap <- paste0(paste0(toupper(strtrim(strout, 1)), substring(strout, 2)),' ',strother)
  }
  return(firstCap)
}
