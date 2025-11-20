# Efiplus data used to develop ecological sensitivity parameters for riverine species in European streams and rivers.

A `tibble`

## Usage

``` r
data(speciesdata)
```

## Format

A `tibble` 99 rows and 23 columns.

- scientificName: The fish species names extracted from the EFIPLUS
  dataset.

- waterBody: The water body from which the species records were
  collected.

- decimalLatitude: Species location

- decimalLongitude: Species occurrence records.

- MRR: The locality where the species was collated or sampled from

- Date: the day, month, and year when the species record was collected.

- : year: The year when the species record was collated.

- Locality: particular locality where the species was sampled.

- country: The country where the record was made.

## Details

BQEs sensitivity to global/climate change in European rivers:
implications for reference conditions and pressure-impact-recovery
chains (Logez et al. 2012). An extract has been made for usage in this
package but for more information write to ihg@boku.ac.at

## References

Logez M, Belliard J, Melcher A, Kremser H, Pletterbauer F, Schmutz S,
Gorges G, Delaigue O, Pont D. 2012. Deliverable D5.1-3: BQEs sensitivity
to global/climate change in European rivers: implications for reference
conditions and pressure-impact-recovery chains.

## Examples

``` r
if (FALSE) { # \dontrun{

data("speciesdata")
speciesdata
} # }

```
