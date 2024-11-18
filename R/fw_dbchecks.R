
#' @title Check and clean species names to match standard names in the database.
#'
#' @param sp \code{string or vector}. Species scientific names to be checked. Although the spellings are checked, the users
#'      should check for the species name provided to avoid not being being detected in the database.
#' @param grouplists \code{list}. List of data downloaded in the \code{\link{fw_searchdata}} function. If species considered in \code{sp}
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
#' @param full \code{logical} \code{TRUE} if a dataframe with both cleaned and uncleaned species are required. If \code{FALSE} then the
#'       a species list will be produced after cleaning. Default \code{FALSE}.
#' @param warn To alert user on the species names cleaning errors and warnings.
#'
#' @importFrom utils adist
#'
#' @return \code{vector or string} clean species name that is also found in the database.
#'
#'
clean_names <- function(sp, grouplists, pct = 80, errorness = 30,
                           group = NULL, full = FALSE,
                           warn= FALSE) {
  #get the standard lists
  if(is.null(group)) stop("Provide the organism group")

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
      #reduce the species to get only the species name
      if(length(spclean)>2){

        spclean2 <- paste0(unlist(strsplit(spclean, " "))[1:3], collapse = ' ')

      }else{
      spclean2 <- paste0(unlist(strsplit(spclean, " "))[1:2], collapse = ' ')

      }
      inOut2 <- spclean2%in%stlist

      if(inOut2==TRUE){

        spch <- spclean2

      }else{
        #check if there is a similar name

        dst <- adist(spclean2, stlist)

        errorsp <- (min(dst)/nchar(spclean2))*100

        if(isTRUE(warn))if(errorsp>30) warning("The returned species name ", stlist[which.min(dst)], " has a high percentage error compared to ", spclean2, " and wrong traits may be returned.", call. = FALSE)

        #errorness of the name
        if(errorsp < errorness){

          spsel <- stlist[which.min(dst)]

          #check %length of the species replacing
          ncpct <- (nchar(spsel)/nchar(spclean2))*100

          if(ncpct > pct) {

            spch <- spsel

          }
          else {
            if(isTRUE(warn))warning("No matching species name found for ", spclean2, " in the ", group, "  and will be removed", call. = FALSE)
            spch = NA
          }
        }else{
          if(isTRUE(warn))warning("No matching species name found for ", spclean2, " in the ", group, " and will be removed", call. = FALSE)
          spch = NA
        }
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
#'

compare_traits <- function(traits){

  xtraits <- fw_paramlist()

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
