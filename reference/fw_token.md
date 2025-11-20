# Access and loading the token key

The function updates the authentication token automatically, which the
servers generate every six hours. The function uses the API key, a
one-time key provided during registration or by database managers for
already registered users. Since the authentication token expires, the
seed parameter allows caching across user sessions. Therefore, the data
downloaded with a particular seed will be stored in memory and can be
retrieved by the user. PC rather than from servers, and hence,
tremendously optimize the speed of data access. The token is generated
in two ways, depending on whether the codes will be shared with others
or for personal use. If they are for personal use, the API key is
directly pasted into the pop-up after the fw_token is executed. However,
in the latter circumstance, it is advisable to store the API key in the
R user environment and encrypt it during code execution. Check the
vignettes for handling the API key on the fwtraits GitHub.

## Usage

``` r
fw_token(
  apikey = NULL,
  seed = NULL,
  cachefolder = NULL,
  secure = TRUE,
  inform = FALSE
)
```

## Arguments

- apikey:

  `string`. The API key is automatically loaded using the loadapikey()
  internal function.

- seed:

  `integer`. An integer to help track the caching of the access token
  generated during data collation. If a user wants a new token, the seed
  should be changed.

- cachefolder:

  `string`. The root path where the cached data will be saved on the
  user's PC. If the path is not provided, the cached information will be
  saved in the current working directly.

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

## Value

`string` token authentication token key

## See also

[`fw_setapikey`](https://anthonybasooma.github.io/fwtraits/reference/fw_setapikey.md)

## Examples

``` r
if (FALSE) { # \dontrun{

#1.Use the API key in shared R examples

} # }
```
