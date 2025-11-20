# ggplot2 visualization.

ggplot2 visualization.

## Usage

``` r
fw_visualize(
  output,
  scales = "free",
  ncol = 2,
  params = NULL,
  organism_group = NULL,
  color = "purple",
  na.rm = TRUE
)
```

## Arguments

- output:

  fetchdata output only accepted.

- scales:

  indicate if the scales are free, free_x, and free_y.

- ncol:

  Number of columns to display the data in facet_wrap.

- params:

  species ecological parameters selected if there are more than one.

- organism_group:

  Particular organism group to filter out to allow visualization.

- color:

  `string` Change the color for the bar graphs being plotted.

- na.rm:

  `logical` To remove NA in the category names or species traits.

## Value

ggplot2 display of the retrieved ecological parameters

## Examples

``` r
if (FALSE) { # \dontrun{
fetchdata <- fw_fetchdata(data = c("Abramis brama",'Salmo trutta fario'),
          organismgroup = 'fi',
          ecoparams = c('migration',"rheophily habitat"), cachefolder = 'cache')

fw_visualize(fetchdata)

} # }


```
