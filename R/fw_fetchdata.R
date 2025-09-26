
#' @title Extracting the traits from the downloaded data.
#'
#' @inheritParams fw_searchdata
#' @inheritParams clean_names
#' @param data \code{vector}. The list or vector with species names for which
#'   ecological references needs to be extracted from the database.
#' @param taxonomic_column \code{string}. If the data is a dataframe, the
#'   species column is required and provided in this parameter. The column
#'   should have complete species name and not genus and species provided
#'   separately.
#' @param organismgroup_column \code{string} If the data is a dataframe, and
#'   more than one taxonomic group exists in the data, the
#'   \code{organismgroup_column} is required to iterate over the taxonomic
#'   groups separately.
#' @param details \code{loical}. Outputs the downloaded details including the organism groups
#'      considered by the user, the functional call, and whether some groups were successful
#'      in retrieving data.
#'
#'
#' @importFrom methods is
#' @importFrom R.cache addMemoization
#'
#'
#' @return \code{dataframe} The output has four sections, including
#'  \itemize{
#'  \item ecodata, which is the complete dataframe with all the taxonomic
#'  names and ecological parameters.
#'  \ item taxasearch: is a table with the taxonomic names,both original and cleaned names.
#'  \item fetch: an indication that data has been fetched from
#'   www.freshwaterecology.info.
#'  \item fun_call: A functional call used internally to review the data
#'  cleaning process.
#'  }
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'dfextract <- fw_fetchdata(data = "Abramis brama", organismgroup = 'fi', inform = TRUE,
#'                             ecoparams = 'migration', cachefolder = 'cache' )
#'
#' }
#'

fw_fetchdata <- function(data,
                     organismgroup,
                     ecoparams = NULL,
                     taxalevel = 'species',
                     taxonomic_column = NULL,
                     organismgroup_column = NULL,
                     apikey  = NULL,
                     seed = 1134,
                     secure = TRUE,
                     percenterror = 80,
                     errorness = 20,
                     warn = FALSE,
                     inform = FALSE,
                     cachefolder = 'cache',
                     details = FALSE) {

  match.arg(taxalevel, choices = c("taxagroup",'family', 'genus', 'species'))

  if(is(data, "data.frame")){

    if(is(data, 'sf')) xdata <- as.data.frame(data) else xdata <- data

    if(is.null(taxonomic_column)){

      stop("If ", deparse(substitute(data)), " is a dataframe, column with species names must be provided.")

    }else{
      #one organism group in the data

      if(length(organismgroup)==1){

        if(taxonomic_column%in%colnames(data)==FALSE) stop(deparse(substitute(taxonomic_column)), " not in the dataset ", deparse(substitute(data))," provided.")

        #create a species list
        taxa_searched_list <- unlist(xdata[, taxonomic_column])

      }else{
        #more than one taxa group in the dataset
        if(organismgroup_column%in%colnames(data)==FALSE) stop(deparse(substitute(organismgroup_column)), " not in the dataset ", deparse(substitute(data))," provided.")

        taxagroup_split <- split(xdata, f = xdata[, organismgroup_column])

        taxa_searched_list <- sapply(taxagroup_split, function(x) x[,taxonomic_column], simplify = FALSE)
      }

    }

  }else {
    taxa_searched_list <- data
  }

  #get function to enable decaching

  .funcall <- as.list(match.call())[-1]

  #search for data
  datalists <- fw_searchdata(organismgroup = organismgroup,
                             ecoparams = ecoparams,
                             taxa_searched  = taxa_searched_list,
                             apikey =   apikey,
                             secure = secure,
                             seed = seed,
                             warn = warn,
                             inform = inform,
                             taxalevel = taxalevel,
                             cachefolder = cachefolder)

  #check if the taxa names are okay data retrieval

  dnames_check <- sapply(names(datalists), function(xout){

    if(is(data, 'list') | is(data, 'data.frame')){

      if(length(organismgroup)==1 & is.null(organismgroup_column)) spn <- taxa_searched_list else spn <- taxa_searched_list[[xout]]

    }else{
      spn <- taxa_searched_list
    }
    #print(spn)
    spcheck <- clean_names(sp = spn, grouplists = datalists[[xout]],
                           taxalevel = taxalevel,
                           full = TRUE,
                           warn = warn)

    specieschecked <- spcheck$clean[!is.na(spcheck$clean)]

    if(length(specieschecked)>=1){

      spout <- list(spl = specieschecked, spdf = spcheck)
      #spout <- species checked

    }else{

      #warning at organism group level

      if(isTRUE(warn))warning(tcheck(xout), ' will be removed since no data is found for the indicated taxonomic names.', call. = FALSE)

      #get taxa list and cleaning output

      spout <- list(spl = NULL, spdf = spcheck)
    }
  }, simplify = FALSE)

  #extract cleaned and uncleaned data

  xtabout <- sapply(names(dnames_check), function(pp){

    xt <- dnames_check[[pp]][[2]]

    xt$group <- pp

    xt

  }, USE.NAMES = TRUE, simplify = FALSE)

  xtabspp <- Reduce(rbind, xtabout)

  #get taxa list

  spout <- sapply(names(dnames_check), function(bb){dnames_check[[bb]][[1]]}, USE.NAMES = TRUE, simplify = FALSE)

  #Allowed names for each group

  taxanames_in <- spout[!sapply(spout,is.null)]

  if(length(taxanames_in)<=0) stop("The taxonomic names not found in the database at ", taxalevel ," level", call. = FALSE)

  #check if the lists returned data

  dataout <- datalists[names(taxanames_in)]

  #loop through groups with data
  grouplp <- lapply(names(dataout), function(xff){

    taxalist <- taxanames_in[[xff]]

    gdata <- dataout[[xff]]

    #loop through each traits but navigating the different taxa levels: wow
    ecolp <- lapply(gdata, function(xgg){

      tlevels <- switch (taxalevel, species = 'Taxonname', taxagroup = 'TaxaGroup', family = 'Family', genus = 'Genus' )

      #replace Taxonname with Taxonname without the author

      xgg["Taxonname"] <- paste0(xgg$Genus,' ', xgg$Species)

      #check if a particular traits returns no data and skip it

      xtraitdata <- xgg[xgg[, tlevels] %in% taxalist, ]


      if(nrow(xtraitdata)>=1) xtraitdata else return(NULL)

      ecopar <- fw_parsevalues(data = xtraitdata, org = xff, cachefolder = cachefolder)
    })
    dout <- Reduce(rbind, ecolp)
  })

  #all groups

  dfout <- Reduce(rbind, grouplp)

  #get the function call for easy decaching

  res <- list(ecodata = dfout, taxasearched = xtabspp, type =  'fetch', funcall_ = .funcall)

  if(isTRUE(details)){
    cat(" ======================================",'\n',
        '        DATA OUTPUT SUMMARY','\n',
        "======================================",'\n',
        'Number of Organism Groups Considered :',   length(organismgroup),'\n',
        'Number of Organism Groups Considered :',   if(length(organismgroup)==1) organismgroup else paste(names(data), collapse = ','),'\n',
        'Succesful organism Groups            :',   length(names(taxanames_in)),'\n',
        'Failed organism Groups               :',   length(names(spout[sapply(spout,is.null)])),'\n',
        'Taxa level used                      :',   taxalevel, '\n',
        'Failed at taxa level                 :',   NA,'\n',
        'Number of parameters                 :',   length(unlist(ecoparams)),'\n',
        'Error rate for wrong names           :',   errorness,'\n',
        'Caching folder                       :',   cachefolder, '\n',
        "======================================", '\n')
  }

 invisible(res)
}



#' @noRd
#'
fw_parsevalues <- function(data, org, cachefolder){

  cachedir <- fw_path(cachefolder)

  setCacheRootPath(path= cachedir)

  cache.root = getCacheRootPath()

  keyout <- list(data, org, cachefolder)

  finalout <- loadCache(keyout)

  if(!is.null(finalout)){
    return(finalout)

  }else{
    dbdata <- fw_dbguide(organismgroup = org)

    data[data=="NULL"] <- NA

    splitdata <-  split(data, seq(nrow(data)))

    lll <- lapply(seq_along(splitdata), function(xx){

      ecoparm <- splitdata[[xx]]$ecologicalParameterList[[1]]

      lll2 <- lapply(ecoparm, function(ww){

        traitslist <- ww[["categoryList"]]

        parnames <- ww[["parameterName"]]

        pext      <-     sapply(traitslist, function(x) x[['categoryAbbr']]) # extract some parameter names e.g for migration..

        catext    <-     sapply(traitslist, function(x) x[['categoryValue']]) #extract category names

        pextclean <-     pext[which(catext != "0" & !is.null(catext) & nzchar(catext))] # maintain those with values, no 0, NA and empty strings

        pcat <-     catext[which(catext != "0" & !is.null(catext) & nzchar(catext))]

        if(length(pcat)==1 && isTRUE(pcat == 'NULL')){
          pcatclean <- NULL

        }else if(length(pcat)==1 && grepl("[\\s\\(\\) \" \" ]", pcat) && parnames=='substrate preference' | parnames =='life form'){

          p1 <- unlist(strsplit(pcat, split = ",| "))

          p2 <- p1[nzchar(p1)]

          pcatclean <- gsub("[\\s\\(\\) \" \" ]", "", p2)

        }else{
          pcatclean <- pcat
        }

        pcatlp <- mapply(pextclean, pcatclean, FUN = function(iparam, icate){

          #handle parameter
          paramvalue <- clean_traits(parnames)

          if(is.null(icate)==TRUE) {

            cvalue <- NA

            clevel <- NA

          }else{

            if(clean_traits(iparam) == paramvalue) {

              cvalue <- dbdata$category_name[which(dbdata$parameters_cleaned== paramvalue & dbdata$category_abbrevation==icate)]

              if(length(cvalue)<=0) cvalue <- icate

              clevel <- NA
            }else{
              cvalue <- dbdata$category_name[which(dbdata$parameters_cleaned== paramvalue & dbdata$category_abbrevation==iparam)]

              if(length(cvalue)<=0) cvalue <- iparam

              clevel <- icate
            }
          }

          # #Add classification
          classificationType <- unique(dbdata$classificationSystem[which(dbdata$parameters_cleaned== paramvalue)])

          #Add data type

          DataType <- unique(dbdata$DataType[which(dbdata$parameters_cleaned== paramvalue)])

          #explanation of category names
          explanation <- unique(dbdata$category_explanation[which(dbdata$parameters_cleaned== paramvalue &
                                                                    dbdata$category_name == cvalue )])
          if(length(explanation)<=0 | is.na(cvalue)) explanation <- NA

          # #Add assignment info
          assigninfo <- unique(dbdata$AssignementInfo[which(dbdata$parameters_cleaned== paramvalue)])

          dataout <- data.frame(ID_FWE         = as.character(unlist(splitdata[[xx]][,"ID_FWE"])),
                                OrganismGroup        = org,
                                TaxaGroup            = unlist(splitdata[[xx]][,"TaxaGroup"]),
                                Family               = unlist(splitdata[[xx]][,"Family"]),
                                Genus                = unlist(splitdata[[xx]][,"Genus"]),
                                Species              = unlist(splitdata[[xx]][,"Species"]),
                                Taxonname            = unlist(splitdata[[xx]][,"Taxonname"]),
                                Author               = unlist(splitdata[[xx]][,"Author"]),
                                Parameter            = paramvalue,
                                CategoryName         = cvalue,
                                CategoryAttributes   = clevel,
                                CategoryExplanation  = explanation,
                                DataType             = DataType,
                                ClassificationType   = classificationType,
                                AssignementInfo      = assigninfo
          )


        }, SIMPLIFY = FALSE, USE.NAMES = FALSE)

        vvx <- do.call(rbind, pcatlp)

      })
      vvw <- do.call(rbind, lll2)
    })
    finalout <- do.call(rbind, lll)

    saveCache(finalout, key= keyout, comment="cache parsed data", compress = TRUE)

    finalout;
  }
}
