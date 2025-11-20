# Taxonomic data for species in the freshwaterecology.info database

A `tibble`

## Usage

``` r
data(invertbackbone)
```

## Format

A `tibble` 10421 rows and 3 columns.

- Taxgroup: Higher taxonomic grouping for the species, for example,
  Bivalvia.

- Family: Taxonomic classification, e.g., CARDIIDAE

- Taxon: Lower taxonomic grouping for the species.eg., Parvicardium
  exiguum

## Details

Standard taxonomic backbone for macroinvertebrates from
www.freshwaterecology.info. All macroinvetebrates species are
crosschecked with this database improve quality control of the species
names provided by the user.

## References

Schmidt-Kloiber, A., & Hering, D. (2015). www.freshwaterecology.info -
An online tool that unifies, standardizes and codifies more than 20,000
European freshwater organisms and their ecological preferences.
Ecological Indicators, 53, 271-282.
https://doi.org/10.1016/j.ecolind.2015.02.007.

## Examples

``` r
if (FALSE) { # \dontrun{

data("invertbackbone")
invertbackbone
} # }

```
