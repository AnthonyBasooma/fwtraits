
#' Title
#'
#' @param sp
#' @param grouplists
#'
#' @return
#' @export
#'
#' @examples
#'
clean_names <- function(sp, grouplists) {

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

    stlist <- unlist(unique(do.call(c, sapply(grouplists, function(xx) paste0(xx$Genus," ", xx$Species)))))

    inOut <- spclean%in%stlist

    if(inOut==TRUE) {

      spch <- spclean

    }else{
      #check if there is a similar name

      dst <- adist(spclean, stlist)

      spch <- stlist[which.min(dst)]
    }

    return(spch)
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



