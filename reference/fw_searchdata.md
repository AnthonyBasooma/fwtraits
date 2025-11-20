# To download data from the Freshwaterecology.info database.

The function provides seamless access and download of species ecological
parameters, traits, or indicators from the www.freshwaterecology.info
database. The function allows multiple organism groups, including
macroinvertebrates, fish, phytoplankton, phytobenthos, macrophytes, and
diatoms.

## Usage

``` r
fw_searchdata(
  organismgroup,
  taxa_searched = NULL,
  ecoparams = NULL,
  apikey = NULL,
  warn = TRUE,
  seed = 1135,
  secure = TRUE,
  inform = FALSE,
  taxalevel = NULL,
  cachefolder = "cache"
)
```

## Arguments

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

- taxa_searched:

  `string` An internal placeholder to accommodate the standard taxonomic
  names for invertebrates and phytobenthos from the database.

- ecoparams:

  `vector`. Selected traits that should be downloaded for a particular
  organism group. Check
  [`fw_dbguide`](https://anthonybasooma.github.io/fwtraits/reference/fw_dbguide.md)
  for the allowed traits in the database.

- apikey:

  `string`. The API key is automatically loaded using the loadapikey()
  internal function.

- warn:

  `logical` To show species name warning checks and traits cleaning.
  Default `FALSE`.

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

- inform:

  `logical`. This is to indicate if the token has been successfully
  generated. Default `TRUE`.

- taxalevel:

  `string` Allowed taxonomic levels at which data can be retrieved.
  Default is `'species'` but data can also be downloaded at `family`,
  `genus`, and `taxagroup` level.

- cachefolder:

  `string`. The root path were the cached data will be saved on the user
  PC. If the path is not provided, the cached information will be saved
  in the current working directly.

## Value

List of download species traits, ecological paramaeters or traits.

## Details

Downloading macroinvertebrates data takes a lot of time because the
database has a lot of data, considerably slowing the process. Therefore,
the `taxa_searched` parameter is recommended for the user to indicate
only particular traits, family, orders, or taxa group where the species
falls. Also, the phytobenthos requires providing the `taxa_searched` to
enable a search for a particular taxon group. The parameter should not
be provided for other organism groups because it will not be used in the
data search.

## Examples

``` r
if (FALSE) { # \dontrun{
dfsearch <- fw_searchdata(organismgroup = 'fi',
                           ecoparams = 'migration', cachefolder = 'cache')
} # }
```
