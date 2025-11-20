# How to handle the API key in public and private codes.

``` r
library(fwtraits)
```

## Steps in securing the API key.

- STEP 1: Acquire the API key, the user can follow the steps elucidated
  in
  **[`fw_be4ustart()`](https://anthonybasooma.github.io/fwtraits/reference/fw_be4ustart.md)**
  function.

- STEP 2: Edit the .Renviron file by running
  **[`fw_setapikey()`](https://anthonybasooma.github.io/fwtraits/reference/fw_setapikey.md)**.

- STEP 3: If the .Renviron window opens, please type in **API_KEY =
  “9f7be425-e099-11ee–…………..”**

- STEP 4: Save and restart the R session.

- STEP 5: The key will be ready for use and will not required to be
  provided anywhere within the codes thereafter.

- STEPT 6: When you test with the codes, a welcome message will be
  printed before data is retrieved.

### Test if the API key is well saved

``` r


migration <- fw_fetchdata(data = 'Abramis brama', 
                         organismgroup = 'fi',
                         ecoparams = 'migration',
                         cachefolder = 'cache', 
                         warn = TRUE,
                         inform = TRUE,
                         details = TRUE)#the species spelling is checked
#> Fish data was already downloaded.
#>  ====================================== 
#>          DATA OUTPUT SUMMARY 
#>  ====================================== 
#>  Number of Organism Groups Considered : 1 
#>  Number of Organism Groups Considered : fi 
#>  Succesful organism Groups            : 1 
#>  Failed organism Groups               : 0 
#>  Taxa level used                      : species 
#>  Failed at taxa level                 : NA 
#>  Number of parameters                 : 1 
#>  Error rate for wrong names           : 20 
#>  Caching folder                       : cache 
#>  ======================================

#or use the fw_keyload() function: which returns a congratulation message
```

## 3. References

1.  Wickham H (2024). *httr2: Perform HTTP Requests and Process the
    Responses*. R package version
    1.0.3,<https://CRAN.R-project.org/package=httr2>

2.  <https://httr2.r-lib.org/articles/wrapping-apis.html>
