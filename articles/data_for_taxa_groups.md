# Retrieve data for taxomomic groups.

``` r
library(fwtraits)
```

## Step 1: Securing and loading the API key.

Please follow the
[secure_the_api_key](https://anthonybasooma.github.io/fwtraits/articles/Secure_API_key.html))
steps and secure the API in the R environment before retrieving the
species traits form the database. To check on the available traits in
the database, you can check with the
**[`fw_dbguide()`](https://anthonybasooma.github.io/fwtraits/reference/fw_dbguide.md)**
or browse the database directly in the [species
traits](https://www.freshwaterecology.info/abbreviations.php).

## Step2: Fetching groups data and visualisation

- To ensure the exact trait spellings are used, the user is strongly
  advised to run
  **[`fw_dbguide()`](https://anthonybasooma.github.io/fwtraits/reference/fw_dbguide.md)**
  function and filter out the organism groups. The trait names can be
  copied and pasted in the **`ecoparams`** parameter of the
  **[`fw_fetchdata()`](https://anthonybasooma.github.io/fwtraits/reference/fw_fetchdata.md)**
  function.

- Multiple species can be set in a list or dataframe.

- Wide data formats can be displayed, but the longer version is the
  output by default.

- Ensure the right species names are indicated to avoid detailed checks
  that delays data search and might lead to erroneous trait extraction
  or errors. Checks are conducted on the species names to ensure they
  are harmonized to trait searches.

**Data output columns and meaning**

[TABLE]

### 1. Fishes

**1.1: Single species**

``` r

migration <- fw_fetchdata(data = "Abramis brama",
                          organismgroup = 'fi',
                          ecoparams = 'migration')
#>  ======================================================================== 
#>  Congratulations!!! Ready to interact with www.freshwaterecology.info database 
#>  =========================================================================

head(migration$ecodata)
#>   ID_FWE OrganismGroup     TaxaGroup     Family   Genus Species     Taxonname
#> 1    210            fi Cypriniformes Cyprinidae Abramis   brama Abramis brama
#>             Author Parameter  CategoryName CategoryAttributes
#> 1 (Linnaeus, 1758) migration potamodromous                 NA
#>                                                CategoryExplanation DataType
#> 1 migratory fish species; whole life cycle is spent in freshwaters  Nominal
#>                  ClassificationType AssignementInfo
#> 1 single category assignment system            <NA>
```

``` r

#species data archived in fwtraits

data("speciesdata") 

traitsout <- fw_fetchdata(data = speciesdata, 
                          organismgroup = 'fi',
                          taxonomic_column = 'scientificName', 
                          ecoparams = c('migration', 'threat austria',
                                        'oxygen tolerence', 'migration fibs'),
                          taxalevel = 'species')

head(traitsout$ecodata)
#>   ID_FWE OrganismGroup       TaxaGroup        Family     Genus   Species
#> 1    210            fi   Cypriniformes    Cyprinidae   Abramis     brama
#> 2    216            fi   Cypriniformes    Cyprinidae  Alburnus  alburnus
#> 3    222            fi  Anguilliformes   Anguillidae  Anguilla  anguilla
#> 4    226            fi   Cypriniformes Nemacheilidae Barbatula barbatula
#> 5    227            fi   Cypriniformes    Cyprinidae    Barbus    barbus
#> 6    239            fi Scorpaeniformes      Cottidae    Cottus     gobio
#>             Taxonname           Author Parameter  CategoryName
#> 1       Abramis brama (Linnaeus, 1758) migration potamodromous
#> 2   Alburnus alburnus (Linnaeus, 1758) migration potamodromous
#> 3   Anguilla anguilla (Linnaeus, 1758) migration    diadromous
#> 4 Barbatula barbatula (Linnaeus, 1758) migration potamodromous
#> 5       Barbus barbus (Linnaeus, 1758) migration potamodromous
#> 6        Cottus gobio   Linnaeus, 1758 migration  no migration
#>   CategoryAttributes
#> 1               <NA>
#> 2               <NA>
#> 3               <NA>
#> 4               <NA>
#> 5               <NA>
#> 6               <NA>
#>                                                                 CategoryExplanation
#> 1                  migratory fish species; whole life cycle is spent in freshwaters
#> 2                  migratory fish species; whole life cycle is spent in freshwaters
#> 3 migratory fish species; parts of the life cycle are spent in fresh and salt water
#> 4                  migratory fish species; whole life cycle is spent in freshwaters
#> 5                  migratory fish species; whole life cycle is spent in freshwaters
#> 6                                               species with no migration movements
#>   DataType                ClassificationType AssignementInfo
#> 1  Nominal single category assignment system            <NA>
#> 2  Nominal single category assignment system            <NA>
#> 3  Nominal single category assignment system            <NA>
#> 4  Nominal single category assignment system            <NA>
#> 5  Nominal single category assignment system            <NA>
#> 6  Nominal single category assignment system            <NA>
```

**1.3 Visualizing fish traits**

``` r

fw_visualize(output = traitsout)
```

![](data_for_taxa_groups_files/figure-html/Viszualize%20outputs%20for%20fish-1.png)

### 2. Macroinvertebrates

**2.1: Single species**

``` r

mmdata_traits <- fw_fetchdata(data =  "Margaritifera margaritifera",
                              organismgroup = 'mi',
                              ecoparams = c('stream zonation preference'),
                              inform = TRUE)
#> Macroinvertebrates for parameter -1- data is downloading. Be patient..
```

**2.2: More than one species in a vector/list**

``` r

speciesvec <- c("Margaritifera margaritifera",
                "Pseudunio auricularius",
                "Musculium lacustre",
                "Musculium transversum",
                "Parastenocaris germanica")

multspp <- fw_fetchdata(data =  speciesvec,
                        organismgroup = 'mi',
                        ecoparams = c('stream zonation preference'),
                        inform = TRUE)
#> Macroinvertebrates data was already downloaded.
head(multspp$ecodata)
#>   ID_FWE OrganismGroup TaxaGroup           Family         Genus       Species
#> 1   5943            mi  Bivalvia MARGARITIFERIDAE Margaritifera margaritifera
#> 2   5943            mi  Bivalvia MARGARITIFERIDAE Margaritifera margaritifera
#> 3   5943            mi  Bivalvia MARGARITIFERIDAE Margaritifera margaritifera
#> 4   6653            mi  Bivalvia MARGARITIFERIDAE     Pseudunio  auricularius
#> 5   6653            mi  Bivalvia MARGARITIFERIDAE     Pseudunio  auricularius
#> 6   6653            mi  Bivalvia MARGARITIFERIDAE     Pseudunio  auricularius
#>                     Taxonname           Author                  Parameter
#> 1 Margaritifera margaritifera (LINNAEUS, 1758) stream zonation preference
#> 2 Margaritifera margaritifera (LINNAEUS, 1758) stream zonation preference
#> 3 Margaritifera margaritifera (LINNAEUS, 1758) stream zonation preference
#> 4      Pseudunio auricularius (SPENGLER, 1793) stream zonation preference
#> 5      Pseudunio auricularius (SPENGLER, 1793) stream zonation preference
#> 6      Pseudunio auricularius (SPENGLER, 1793) stream zonation preference
#>   CategoryName CategoryAttributes CategoryExplanation DataType
#> 1  epirhithral                  2  upper-trout region    Fuzzy
#> 2 metarhithral                  5  lower-trout region    Fuzzy
#> 3 hyporhithral                  3     grayling region    Fuzzy
#> 4         <NA>               <NA>                <NA>    Fuzzy
#> 5         <NA>               <NA>                <NA>    Fuzzy
#> 6         <NA>               <NA>                <NA>    Fuzzy
#>           ClassificationType AssignementInfo
#> 1 10 point assignment system            <NA>
#> 2 10 point assignment system            <NA>
#> 3 10 point assignment system            <NA>
#> 4 10 point assignment system            <NA>
#> 5 10 point assignment system            <NA>
#> 6 10 point assignment system            <NA>
```

**2.3: More than one species in a dataframe**

``` r

macrodf <- data.frame(organismgroup = rep('mi', 13),
                      species = c("Margaritifera margaritifera",
                                  "Pseudunio auricularius",
                                  "Musculium lacustre",
                                  "Musculium transversum",
                                  "Corbicula fluminea",
                                  "Congeria leucophaeata",
                                  "Dreissena polymorpha",
                                  "Dreissena rostriformis bugensis",
                                  "Parastenocaris germaica",
                                  "Branchiobdella balcanica",
                                  "Branchiobdella hexadonta",
                                  "Branchiobdella parasita",
                                  "Branchiobdella pentadonta"))

multspp_df <- fw_fetchdata(data =  macrodf,
                           organismgroup = 'mi',
                           taxonomic_column = 'species',
                           ecoparams = c("stream zonation preference", "feeding type"),
                           inform = TRUE)
#> Macroinvertebrates for parameter -1- data is downloading. Be patient..
#> Macroinvertebrates for parameter -3- data is downloading. Be patient..
head(multspp_df$ecodata)
#>   ID_FWE OrganismGroup TaxaGroup           Family         Genus       Species
#> 1   4999            mi  Bivalvia     DREISSENIDAE     Dreissena    polymorpha
#> 2   4999            mi  Bivalvia     DREISSENIDAE     Dreissena    polymorpha
#> 3   4999            mi  Bivalvia     DREISSENIDAE     Dreissena    polymorpha
#> 4   5943            mi  Bivalvia MARGARITIFERIDAE Margaritifera margaritifera
#> 5   5943            mi  Bivalvia MARGARITIFERIDAE Margaritifera margaritifera
#> 6   5943            mi  Bivalvia MARGARITIFERIDAE Margaritifera margaritifera
#>                     Taxonname           Author                  Parameter
#> 1        Dreissena polymorpha   (PALLAS, 1771) stream zonation preference
#> 2        Dreissena polymorpha   (PALLAS, 1771) stream zonation preference
#> 3        Dreissena polymorpha   (PALLAS, 1771) stream zonation preference
#> 4 Margaritifera margaritifera (LINNAEUS, 1758) stream zonation preference
#> 5 Margaritifera margaritifera (LINNAEUS, 1758) stream zonation preference
#> 6 Margaritifera margaritifera (LINNAEUS, 1758) stream zonation preference
#>   CategoryName CategoryAttributes
#> 1   epipotamal                  2
#> 2  metapotamal                  4
#> 3     littoral                  4
#> 4  epirhithral                  2
#> 5 metarhithral                  5
#> 6 hyporhithral                  3
#>                                    CategoryExplanation DataType
#> 1                                        barbel region    Fuzzy
#> 2                                         bream region    Fuzzy
#> 3 lake and stream shorelines, lentic sites, ponds etc.    Fuzzy
#> 4                                   upper-trout region    Fuzzy
#> 5                                   lower-trout region    Fuzzy
#> 6                                      grayling region    Fuzzy
#>           ClassificationType AssignementInfo
#> 1 10 point assignment system            <NA>
#> 2 10 point assignment system            <NA>
#> 3 10 point assignment system            <NA>
#> 4 10 point assignment system            <NA>
#> 5 10 point assignment system            <NA>
#> 6 10 point assignment system            <NA>
```

**2.4: Data visualization for macroinvertebrates**

If the species considered are more than 1, then visualization can be to
determine the frequency of each trait. This is an optional function for
post data searches.

``` r

fw_visualize(output = multspp)
```

![](data_for_taxa_groups_files/figure-html/Viszualize%20outputs%20for%20macroinvertebrates-1.png)

``` r

fw_visualize(output = multspp_df)
```

![](data_for_taxa_groups_files/figure-html/Viszualize%20outputs%20for%20macroinvertebrates-2.png)

### 3. Macrophytes

``` r

mpp <- fw_fetchdata(data = c("Amblystegium fluviatile",
                             "Amblystegium humile",
                             "Amblystegium riparium",
                             "Amblystegium serpens",
                             "Amblystegium tenax",
                             "Amblystegium varium"),
                    organismgroup = 'mp',
                    ecoparams = c('zone - systema'))
```

### 4. Phytobenthos

``` r

pbdata <- fw_fetchdata(data = c("Gongrosira debaryana",
                                "Gongrosira fluminensis",
                                "Hydrodictyon reticulatum",
                                "Sphaerobotrys fluviatilis",
                                "Stigeoclonium farctum",
                                "Stigeoclonium tenue",
                                "Tetraspora gelatinosa",
                                "Thorea hispida"),
                       organismgroup = 'pb',
                       ecoparams = c('substrate preference'))
head(pbdata$ecodata)
#>   ID_FWE OrganismGroup     TaxaGroup          Family         Genus     Species
#> 1   9503            pb Chlorophyceae Chaetophoraceae    Gongrosira   debaryana
#> 2   9503            pb Chlorophyceae Chaetophoraceae    Gongrosira   debaryana
#> 3   9504            pb Chlorophyceae Chaetophoraceae    Gongrosira fluminensis
#> 4   9576            pb Chlorophyceae   Chlorophyceae Sphaerobotrys fluviatilis
#> 5   9584            pb Chlorophyceae Chaetophoraceae Stigeoclonium     farctum
#> 6   9584            pb Chlorophyceae Chaetophoraceae Stigeoclonium     farctum
#>                   Taxonname        Author            Parameter CategoryName
#> 1      Gongrosira debaryana  Rabenhorst 0 substrate preference    epilithic
#> 2      Gongrosira debaryana  Rabenhorst 0 substrate preference    epiphytic
#> 3    Gongrosira fluminensis F.E.Fritsch 0 substrate preference    epilithic
#> 4 Sphaerobotrys fluviatilis     Butcher 0 substrate preference    epilithic
#> 5     Stigeoclonium farctum    Berthold 0 substrate preference    epilithic
#> 6     Stigeoclonium farctum    Berthold 0 substrate preference    epiphytic
#>   CategoryAttributes CategoryExplanation DataType
#> 1                 NA            on stone  Nominal
#> 2                 NA           on plants  Nominal
#> 3                 NA            on stone  Nominal
#> 4                 NA            on stone  Nominal
#> 5                 NA            on stone  Nominal
#> 6                 NA           on plants  Nominal
#>                   ClassificationType AssignementInfo
#> 1 presence/absence assignment system            <NA>
#> 2 presence/absence assignment system            <NA>
#> 3 presence/absence assignment system            <NA>
#> 4 presence/absence assignment system            <NA>
#> 5 presence/absence assignment system            <NA>
#> 6 presence/absence assignment system            <NA>
```

``` r

mipbdata <- fw_fetchdata(data = list(mi=c("Congeria kusceri",
                                          "Congeria leucophaeata",
                                          "Dreissena polymorpha",
                                          "Dreissena rostriformis bugensis"),
                                     pb= c("Gongrosira debaryana",
                                           "Gongrosira fluminensis",
                                           "Hydrodictyon reticulatum",
                                           "Sphaerobotrys fluviatilis",
                                           "Stigeoclonium farctum",
                                           "Stigeoclonium tenue",
                                           "Tetraspora gelatinosa",
                                           "Thorea hispida")),
                         organismgroup = c('mi','pb'),
                         ecoparams = list(mi = c('stream zonation preference'),
                                          pb = c('substrate preference')), details = TRUE)
#>  ====================================== 
#>          DATA OUTPUT SUMMARY 
#>  ====================================== 
#>  Number of Organism Groups Considered : 2 
#>  Number of Organism Groups Considered : mi,pb 
#>  Succesful organism Groups            : 2 
#>  Failed organism Groups               : 0 
#>  Taxa level used                      : species 
#>  Failed at taxa level                 : NA 
#>  Number of parameters                 : 2 
#>  Error rate for wrong names           : 20 
#>  Caching folder                       : cache 
#>  ======================================

head(mipbdata$ecodata)
#>   ID_FWE OrganismGroup TaxaGroup       Family     Genus      Species
#> 1   4999            mi  Bivalvia DREISSENIDAE Dreissena   polymorpha
#> 2   4999            mi  Bivalvia DREISSENIDAE Dreissena   polymorpha
#> 3   4999            mi  Bivalvia DREISSENIDAE Dreissena   polymorpha
#> 4  11585            mi  Bivalvia DREISSENIDAE  Congeria leucophaeata
#> 5  11585            mi  Bivalvia DREISSENIDAE  Congeria leucophaeata
#> 6  21963            mi  Bivalvia DREISSENIDAE  Congeria      kusceri
#>               Taxonname         Author                  Parameter CategoryName
#> 1  Dreissena polymorpha (PALLAS, 1771) stream zonation preference   epipotamal
#> 2  Dreissena polymorpha (PALLAS, 1771) stream zonation preference  metapotamal
#> 3  Dreissena polymorpha (PALLAS, 1771) stream zonation preference     littoral
#> 4 Congeria leucophaeata (CONRAD, 1831) stream zonation preference  metapotamal
#> 5 Congeria leucophaeata (CONRAD, 1831) stream zonation preference  hypopotamal
#> 6      Congeria kusceri     BOLE, 1962 stream zonation preference         <NA>
#>   CategoryAttributes                                  CategoryExplanation
#> 1                  2                                        barbel region
#> 2                  4                                         bream region
#> 3                  4 lake and stream shorelines, lentic sites, ponds etc.
#> 4                  4                                         bream region
#> 5                  6                                brackish water region
#> 6               <NA>                                                 <NA>
#>   DataType         ClassificationType AssignementInfo
#> 1    Fuzzy 10 point assignment system            <NA>
#> 2    Fuzzy 10 point assignment system            <NA>
#> 3    Fuzzy 10 point assignment system            <NA>
#> 4    Fuzzy 10 point assignment system            <NA>
#> 5    Fuzzy 10 point assignment system            <NA>
#> 6    Fuzzy 10 point assignment system            <NA>
```

### 5. Phytoplankton

``` r

ppdata <- fw_fetchdata(data = c("Navicula radiosa", 'Acanthoceras zachariasii',
                                "Achnanthes brevipes",
                                "Achnanthidium catenatum",
                                "Actinocyclus normanii",
                                "Achnanthidium subatomus"),
                       organismgroup = 'pp',
                       ecoparams = c('stenoecy factor', 'life form'),
                       cachefolder = 'cache',
                       warn = TRUE,
                       inform = TRUE)
#> Phytoplankton data is downloading, be calm and patient on the first run...
#> Phytoplankton data is downloading, be calm and patient on the first run...

head(ppdata$ecodata)
#>   ID_FWE OrganismGroup         TaxaGroup Family         Genus   Species
#> 1    221            pp Bacillariophyceae        Achnanthidium catenatum
#> 2    221            pp Bacillariophyceae        Achnanthidium catenatum
#> 3    221            pp Bacillariophyceae        Achnanthidium catenatum
#> 4    779            pp Bacillariophyceae        Achnanthidium subatomus
#> 5   6197            pp Bacillariophyceae             Navicula   radiosa
#> 6   6197            pp Bacillariophyceae             Navicula   radiosa
#>                 Taxonname                         Author Parameter CategoryName
#> 1 Achnanthidium catenatum (Bily & Marvan) Lange-Bertalot life form     planktic
#> 2 Achnanthidium catenatum (Bily & Marvan) Lange-Bertalot life form      benthic
#> 3 Achnanthidium catenatum (Bily & Marvan) Lange-Bertalot life form     pleuston
#> 4 Achnanthidium subatomus       (Hustedt) Lange-Bertalot life form    B_drifted
#> 5        Navicula radiosa                        Kützing life form     planktic
#> 6        Navicula radiosa                        Kützing life form      benthic
#>   CategoryAttributes                 CategoryExplanation DataType
#> 1               <NA>             planktic living species  Nominal
#> 2               <NA>              benthic living species  Nominal
#> 3               <NA> species living on the water surface  Nominal
#> 4               <NA>                                <NA>  Nominal
#> 5               <NA>             planktic living species  Nominal
#> 6               <NA>              benthic living species  Nominal
#>                   ClassificationType AssignementInfo
#> 1 presence/absence assignment system            <NA>
#> 2 presence/absence assignment system            <NA>
#> 3 presence/absence assignment system            <NA>
#> 4 presence/absence assignment system            <NA>
#> 5 presence/absence assignment system            <NA>
#> 6 presence/absence assignment system            <NA>
```

### Collating by family and genus taxa levels for different species

**NOTE:** Based the on the FWE database, the phytoplantkon don’t have
families but rather taxa groups, genus, and species levels. Therefore,
for data extraction the family level for phytoplankton is not
possible.**Check the table for the definition of a Taxa Group.**

``` r

ppdata_genus <- fw_fetchdata(data = c("Navicula"),
                       organismgroup = 'pp',
                       ecoparams = c('stenoecy factor', 'life form'),
                       cachefolder = 'cache',
                       taxalevel = 'genus',
                       inform = TRUE)
#> Phytoplankton data was already downloaded.
#> Phytoplankton data was already downloaded.

#Only Navicula genus but but multiple species
print(unique(ppdata_genus$ecodata$Genus)) 
#> [1] "Navicula"


ppdata_taxagroup <- fw_fetchdata(data = c("Bacillariophyceae"),
                       organismgroup = 'pp',
                       ecoparams = c('stenoecy factor', 'life form'),
                       cachefolder = 'cache',
                       taxalevel = 'taxagroup',
                       inform = TRUE)
#> Phytoplankton data was already downloaded.
#> Phytoplankton data was already downloaded.

 #More than more 1 genus 
print(unique(ppdata_taxagroup$ecodata$Genus)[1:5])
#> [1] "Achnanthidium" "Amphora"       "Asterionella"  "Aulacoseira"  
#> [5] "Caloneis"
```

## Step3. References

1.  Schmidt-Kloiber, A., & Hering, D. (2015).
    www.freshwaterecology.info - An online tool that unifies,
    standardises and codifies more than 20,000 European freshwater
    organisms and their ecological preferences. Ecological Indicators,
    53, 271-282. <https://doi.org/10.1016/j.ecolind.2015.02.007>

2.  Wickham H (2024). *httr2: Perform HTTP Requests and Process the
    Responses*. R package version
    1.0.3,<https://CRAN.R-project.org/package=httr2>

3.  <https://httr2.r-lib.org/articles/wrapping-apis.html>
