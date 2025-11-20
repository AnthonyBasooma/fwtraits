# Auto generation of species sites

Auto generation of species sites

## Usage

``` r
fw_geosites(x, dist = 1000)
```

## Arguments

- x:

  `dataframe` Geo spatial data with geometry column from sf package.

- dist:

  `integer` The distance used to cluster points as sites. The default is
  1000.

## Value

dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
geospdata <- speciesdata |>
sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'),
            crs = sf::st_crs(4326))
 xx <- fw_geosites(geospdata)
} # }
```
