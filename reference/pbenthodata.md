# Taxonomic data for the phytobentho species in www.freshwaterecology.info database.

A `tibble`

## Usage

``` r
data(pbenthodata)
```

## Format

A `tibble` The dataset has 1857 rows and 2 columns.

- Taxgroup: Higher taxonomic grouping for the species, for example,
  Bacillariophyceae.

- Taxon: Lower taxonomic grouping for the species.eg., Achnanthes acus,
  Achnanthes brevipes, Achnanthes brevipes var. brevipes

## Details

This dataset was extracted from the www.freshwaterecology.info database
and formed a standardized reference for the species names for
phytobenthos. Therefore, all user-provided species are checked across
this database to identify whether or not they exist.

## References

Schmidt-Kloiber, A., & Hering, D. (2015). Www.freshwaterecology.info -
An online tool that unifies, standardises and codifies more than 20,000
European freshwater organisms and their ecological preferences.
Ecological Indicators, 53, 271-282.
https://doi.org/10.1016/j.ecolind.2015.02.007.

## Examples

``` r
if (FALSE) { # \dontrun{

data("pbenthodata")

pbenthodata
} # }
```
