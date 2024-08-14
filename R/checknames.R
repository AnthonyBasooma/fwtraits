
#' Title
#'
#' @param sp
#' @param grouplists
#' @importFrom utils adist
#'
#' @return
#' @export
#'
#' @examples
#'
clean_names <- function(sp, grouplists, pct = 80, errorness = 20, group) {

  #run through the species list

  sapply(sp, function(x){

    # convert all letters to lower
    tlw <- tolower(x)
    # remove accents
    actr <- iconv(tlw, from = "UTF-8", to = "ASCII//TRANSLIT")

    sppt <- gsub("[[:punct:]]", "", actr)

    spc <- gsub("[^[:alnum:]]", " ", sppt)

    spc1 <- gsub("[[:digit:]]+", "", spc)

    spaces <- trimws(gsub("\\s+", " ", spc1), which = "both")

    spclean <- gsub("(^[[:alpha:]])", "\\U\\1", spaces, perl = TRUE)

    #compiles all species from all the list of the group extracted
    if(group=='macroinvertebrates'){

      #macro invertebrates is downloaded and arranged differently since the groups cannot retrieved in one file.
      if(is(grouplists, 'list')){

        stlist <- unlist(unique(Reduce(c, sapply(grouplists, function(yy){sapply(yy, function(zz){paste0(zz$Genus," ", zz$Species)})}))))

      }else{
        stlist <- unlist(unique(sapply(grouplists, function(xx) paste0(xx$Genus," ", xx$Species))))
      }

    }else{
      print('i am here')
      stlist <- unlist(unique(Reduce(c, sapply(grouplists, function(xx) paste0(xx$Genus," ", xx$Species)))))
    }


    inOut <- spclean%in%stlist

    if(inOut==TRUE) {

      spch <- spclean

    }else{
      #check if there is a similar name

      dst <- adist(spclean, stlist)

      errorsp <- (min(dst)/nchar(spclean))*100

      if(errorness>30) warning("The returned species name ", stlist[which.min(dst)], " has a high percentage error compared to ", spclean, " and wrong traits may be returned.", call. = FALSE)

      #errorness of the name
      if(errorsp<errorness){

         spsel <- stlist[which.min(dst)]

         #check %length of the species replacing
         ncpct <- (nchar(spsel)/nchar(spclean))*100

         if(ncpct>pct) {
           spch <- spsel
         }
         else {
           spch = NA
           warning("No matching species name found for ", x, " in the ", group, "  and will be removed", call. = FALSE)
         }
      }else{
        spch = NA
        warning("No matching species name found for ", x, " in the ", group, " and will be removed", call. = FALSE)
      }

    }
    finallist <- spch[!is.na(spch)]

    return(finallist)

  }, simplify = TRUE, USE.NAMES = FALSE)
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
clean_traits <- function(x){

  sapply(x, function(y){

  lowecase <- tolower(y)

  trans <- iconv(lowecase, from = "UTF-8", to = "ASCII//TRANSLIT")

  punctrem <- gsub("[[:punct:]]", "", trans)

  numberrem <- gsub("[^[:alnum:]]", " ", punctrem)

  digtitrms <- gsub("[[:digit:]]+", "", numberrem)

  rmoveof <- gsub("of ", '', digtitrms)

  spacesrm <- trimws(gsub("\\s+", " ", rmoveof), which = "both")

  lstrings <- unlist(strsplit(spacesrm, " "))

  if(length(lstrings)>1) tclean <- paste(lstrings[1:2], collapse = ' ') else tclean <- lstrings

  return(tclean)

  }, USE.NAMES = FALSE)
}




#' Title
#'
#' @param traits
#'
#' @return
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



