# Extracting the traits from the downloaded data.

Extracting the traits from the downloaded data.

## Usage

``` r
fw_fetchdata(
  data,
  organismgroup,
  ecoparams = NULL,
  taxalevel = "species",
  taxonomic_column = NULL,
  organismgroup_column = NULL,
  apikey = NULL,
  seed = 1134,
  secure = TRUE,
  percenterror = 80,
  errorness = 20,
  warn = FALSE,
  inform = FALSE,
  cachefolder = "cache",
  details = FALSE
)
```

## Arguments

- data:

  `vector`. The list or vector with species names for which ecological
  references needs to be extracted from the database.

- organismgroup:

  `string`. The organism group to download from the platform. The
  allowed group includes `"fi", "mi", "pp", "pb", "di","mp"` for fishes,
  macroinvertebrates, phytoplankton, phytobenthos, diatoms, and
  macrophytes, respectively. Multiple groups allowed, such as
  `'pp', 'di'`.

  - `pp`: Pytoplankton.

  - `mp`: Macrophytes

  - `mi`: Macroinvertebrates

  - `fi`: Fishes

  - `di`: Diatoms

  - `pb`: Phytobenthos without diatoms

- ecoparams:

  `vector`. Selected traits that should be downloaded for a particular
  organism group. Check
  [`fw_dbguide`](https://anthonybasooma.github.io/fwtraits/reference/fw_dbguide.md)
  for the allowed traits in the database.

- taxalevel:

  `string` Allowed taxonomic levels at which data can be retrieved.
  Default is `'species'` but data can also be downloaded at `family`,
  `genus`, and `taxagroup` level.

- taxonomic_column:

  `string`. If the data is a dataframe, the species column is required
  and provided in this parameter. The column should have complete
  species name and not genus and species provided separately.

- organismgroup_column:

  `string` If the data is a dataframe, and more than one taxonomic group
  exists in the data, the `organismgroup_column` is required to iterate
  over the taxonomic groups separately.

- apikey:

  `string`. The API key is automatically loaded using the loadapikey()
  internal function.

- seed:

  `integer`. An integer to help track the caching of the access token
  generated during data collation. If a user wants a new token, the seed
  should be changed.

- secure:

  `logical`. If `TRUE`, the User will be prompted to set the API key in
  the .Renviron file by running the
  [`fw_setapikey`](https://anthonybasooma.github.io/fwtraits/reference/fw_setapikey.md)
  function. The User must strictly type in API_KEY = 'api key', save,
  close the file and restart the R session or RStudio for the API_KEY
  environment to be captured. If `FALSE`, then the key will be entered
  directly in the API_KEY directly in the fw_token() function. This
  method is insecure, since other users can obtain the key from the
  codes.

- percenterror:

  `numeric`. The number used as a cutoff to infer similarity of the user
  provided name and what is found in the database. The higher the
  percentage, the higher the similarity the species name provided by the
  user and the one in the database. `percenterror` ranges from 0 to 100
  but the default is 80 to ensure that wrong names are not selected at
  low similarity percentage cutoff.

- errorness:

  `numeric` Similar to `percenterror`, `errorness` parameter uses the
  distance differences between the user-provided names and all the taxa
  group species standard names. The lower the percentage error, the
  higher the similarity in the species names provided. Default is 20 and
  beyond 30, a warning is showed to avoid wrong species replace the user
  provided name, which leads to extracting wrong traits.

- warn:

  `logical` To show species name warning checks and traits cleaning.
  Default `FALSE`.

- inform:

  `logical`. This is to indicate if the token has been successfully
  generated. Default `TRUE`.

- cachefolder:

  `string`. The root path were the cached data will be saved on the user
  PC. If the path is not provided, the cached information will be saved
  in the current working directly.

- details:

  `loical`. Outputs the downloaded details including the organism groups
  considered by the user, the functional call, and whether some groups
  were successful in retrieving data.

## Value

`dataframe` The output has four sections, including

- ecodata, which is the complete dataframe with all the taxonomic names
  and ecological parameters. \\ item taxasearch: is a table with the
  taxonomic names,both original and cleaned names.

- fetch: an indication that data has been fetched from
  www.freshwaterecology.info.

- fun_call: A functional call used internally to review the data
  cleaning process.

## Examples

``` r
if (FALSE) { # \dontrun{

dfextract <- fw_fetchdata(data = "Abramis brama", organismgroup = 'fi', inform = TRUE,
                            ecoparams = 'migration', cachefolder = 'cache' )

} # }
```
