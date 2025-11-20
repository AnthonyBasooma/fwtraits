# Standard table with taxonomic groups and their traits explanations.

Standard table with taxonomic groups and their traits explanations.

## Usage

``` r
fw_dbguide(organismgroup = NULL, cachefolder = "cache")
```

## Arguments

- organismgroup:

  `string or vector`. Taxa group names to aid the users in filtering the
  standard table for species traits and their explanations.

- cachefolder:

  `string`. The root path were the cached data will be saved on the user
  PC. If the path is not provided, the cached information will be saved
  in the current working directly.

## Value

`dataframe` A dataset with taxonomic groups, traits and their
explanations.

## Examples

``` r
if (FALSE) { # \dontrun{

dbase <- fw_dbguide(cachefolder = 'cache')

} # }
```
