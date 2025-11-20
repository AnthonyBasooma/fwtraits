# Data with ecological parameters classification.

A `tibble`

## Usage

``` r
data(classifydata)
```

## Format

A `tibble` The dataset has 817 rows and 4 columns.

- Organism group: Are the taxonomic groups in the
  www.freshwaterecology.info database.

- parameter_cleaned: Are the ecological parameter names in the database.

- DataType: Are the data classification for each ecological parameter.
  The data types are still under revision to improve harmony. Therefore,
  the users can provide a different classification based on their
  expertise.

## Details

The database will be used internally to assign data types such as
nominal, ordinal, ratio, and interval to ecological parameters.

## References

Schmidt-Kloiber, A., & Hering, D. (2015). Www.freshwaterecology.info -
An online tool that unifies, standardises and codifies more than 20,000
European freshwater organisms and their ecological preferences.
Ecological Indicators, 53, 271-282.
https://doi.org/10.1016/j.ecolind.2015.02.007.

## Examples

``` r
if (FALSE) { # \dontrun{

data("classifydata")

classifydata
} # }
```
