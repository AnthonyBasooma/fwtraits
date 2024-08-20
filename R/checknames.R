
#' @title Check and clean species names to match standard names in the database.
#'
#' @param sp \code{string or vector}. Species scientific names to be checked. Although the spellings are checked, the users
#'      should check for the species name provided to avoid not being being detected in the database.
#' @param grouplists \code{list}. List of data downloaded in the \code{\link{collatedata}} function. If species considered in \code{sp}
#'      parameter are fishes, then the fishes lists should be provided otherwise the species names will be rejected.
#' @param pct \code{numeric}. The number used as a cutoff to infer similarity of the user provided name and what is found in the database.
#'        The higher the percentage, the higher the similarity the species name provided by the user and the one in the database. \code{pct}
#'        ranges from 0 to 100 but the default is 80 to ensure that wrong names are not selected at low similarity percentage cutoff.
#' @param errorness \code{numeric} Similar to \code{pct}, \code{errorness} parameter uses the distance differences between the user-provided
#'        names and all the taxa group species standard names. The lower the percentage error, the higher the similarity in the species
#'        names provided. Default is 20 and beyond 30, a warning is showed to avoid wrong species replace the user provided name, which leads
#'        to extracting wrong traits.
#' @param group \code{string} The taxa group names should be indicated to separate the macro-invertebrates workflow from others. The accepted
#'      group names include macroinvertebrates, fishes, macrophytes, phytoplankton, diatoms, and phytobenthos.
#' @param full \code{logical} \code{TRUE} if a dataframe with both cleaned and unclened species are required. If \code{FALSE} then the
#'       a species list will be produced after cleaning. Dafualt \code{FALSE}.
#'
#' @importFrom utils adist
#'
#' @return \code{vector or string} clean species name that is also found in the database.
#'
#' @export
#'
#' @examples
#'
clean_names <- function(sp, grouplists, pct = 80, errorness = 20, group = NULL, full = FALSE) {

  #grt the standard lists
  if(is.null(group)) stop("provide the taxa group name")

  #compiles all species from all the list of the group extracted
  if(group=='macroinvertebrates'){

    #macro invertebrates is downloaded and arranged differently since the groups cannot retrieved in one file.
    if(is(grouplists, 'list')){

      stlistcheck <- unlist(unique(Reduce(c, sapply(grouplists, function(yy){sapply(yy, function(zz){paste0(zz$Genus," ", zz$Species)})}))))

      #some return list of dataframe not list of list of dataframes
      if(length(stlistcheck)<=1) stlist <- unlist(unique(sapply(grouplists, function(xx) paste0(xx$Genus," ", xx$Species)))) else stlist <- stlistcheck

    }else{
      stlist <- unlist(unique(sapply(grouplists, function(xx) paste0(xx$Genus," ", xx$Species))))

    }

  }else{

    stlist <- unlist(unique(Reduce(c, sapply(grouplists, function(xx) paste0(xx$Genus," ", xx$Species)))))
  }

  #get unique only
  spunique <- unique(sp)

  #run through the species list

  spcleandata <- sapply(spunique, function(x){

    # convert all letters to lower
    tlw <- tolower(x)
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
      #check if there is a similar name

      dst <- adist(spclean, stlist)

      errorsp <- (min(dst)/nchar(spclean))*100

      if(errorsp>30) warning("The returned species name ", stlist[which.min(dst)], " has a high percentage error compared to ", spclean, " and wrong traits may be returned.", call. = FALSE)

      #errorness of the name
      if(errorsp<errorness){

        spsel <- stlist[which.min(dst)]

         #check %length of the species replacing
         ncpct <- (nchar(spsel)/nchar(spclean))*100

         if(ncpct>pct) {
           spch <- spsel
         }
         else {
           warning("No matching species name found for ", x, " in the ", group, "  and will be removed", call. = FALSE)
           spch = NA
         }
      }else{
        warning("No matching species name found for ", x, " in the ", group, " and will be removed", call. = FALSE)
        spch = NA
      }

    }
  }, simplify = TRUE, USE.NAMES = FALSE)

  if(isTRUE(full)){

    finalout <- data.frame(raw = spunique, clean = spcleandata)
  }else{
    finalout <- spcleandata[!is.na(spcleandata)]
  }
  return(finalout)
}



#' Title
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


#' @title Traits comparison for the user and standard triat names.
#'
#' @param traits \code{string}. Trait name provided by the user that will be compared with the
#'      standard names in the database.
#'
#' @return \code{string} accepted trait names based on the database
#'
#' @export
#'
#' @examples

compare_traits <- function(traits){

  xtraits <- fip_paramlist()

  ecopa <- xtraits$ecologicalParameterList

  #get the name of all traits for the groups

  xstraits <- Reduce(c, sapply(ecopa, function(x){sapply(x, function(y) y[['name']])}))

  alltraits <- clean_traits(x = xstraits)

  #clean user input word
  traitsuser <- clean_traits(traits)

  sapply(traitsuser, function(uw){ #uw-user word

    checkIn <- uw%in%alltraits

    if(all(checkIn)==TRUE){

      #return the user input

      traitfinal <- uw
    }else{

      dst1 <- adist(uw, alltraits)

      traitfinal <- alltraits[which.min(dst1)]
    }

    return(traitfinal)

  }, USE.NAMES = FALSE)
}


#' Title
#'
#' @param x
#' @param std
#' @param mind
#' @param error
#' @param grp
#' @param warn
#'
#' @return
#'
#' @examples
#'
checktrait<- function(x, std, mind = 0.3, error = 0.8, grp = NULL, warn= TRUE){

  sapply(unique(x), function(wd){

    uwd <- clean_traits(wd)

    dst <- adist(uwd, std)

    wrd <- std[which.min(dst)]

    chdd <- nchar(uwd)/nchar(wrd) # measure of errorness

    dd <- (min(dst)-0)/10 #measure of similarity

    if(chdd >= error && dd <= mind){

      fwd <- wrd

    }else{
      fwd <- NA
      if(isTRUE(warn)) warning("The trait ", wd, " is wrongly spelt and not in ", grp, " traits and is likely to be -", wrd, "- but check traitguide() for appropiate trait name",
              call. = FALSE)
    }

    fwdlst <- fwd[!is.na(fwd)]

  }, simplify = TRUE, USE.NAMES = FALSE)
}

