# For computing the functional dendogram

For computing the functional dendogram

## Usage

``` r
fw_fdendro(fwdata, method = "average", k = 4, plot = FALSE)
```

## Arguments

- fwdata:

  `list` A list from the `fw_fetchdata` function is the only data type
  accepted.

- method:

  `string` Clustering method, including ward, average, and complete

- k:

  `integer` Determine the number of cluster the user wants to output
  after data clustering. The default is 4.

- plot:

  `logical` Either `TRUE` to show the plot of functional dendogram.
  Defualt is `FALSE`.

## Value

dataframe, plot

## Examples

``` r
if (FALSE) { # \dontrun{
  fishtraits <- fw_fetchdata(data = speciesdata,
                        ecoparams = c('rheophily habitat', 'spawning habitat',
                        'feeding diet adult'),
                        taxonomic_column = 'scientificName',
                        organismgroup = 'fi')


 head(fdendoclust, 3)

 table(fdendoclust$cluster)

} # }
```
