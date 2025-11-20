# Check and clean species names to match standard names in the database.

Check and clean species names to match standard names in the database.

## Usage

``` r
clean_names(
  sp,
  grouplists,
  prechecks = FALSE,
  standard_dataset = NULL,
  percenterror = 80,
  errorness = 30,
  full = FALSE,
  warn,
  taxalevel
)
```

## Arguments

- sp:

  `string or vector`. Species scientific names to be checked. Although
  the spellings are checked, the users should check for the species name
  provided to avoid not being being detected in the database.

- grouplists:

  `list`. List of data downloaded in the
  [`fw_searchdata`](https://anthonybasooma.github.io/fwtraits/reference/fw_searchdata.md)
  function. If species considered in `sp` parameter are fishes, then the
  fishes lists should be provided otherwise the species names will be
  rejected.

- prechecks, standard_dataset:

  `logical`. If `TRUE` the standard prechecks will be done on both the
  invertebrates and bentho species names before search for ecological
  parameters from the database. The standard names is provided with the
  dataset to reduce on the time in identifying the standard tyxonomic
  names for the macroinvertebrates in the database. @param taxalevel
  `string` Allowed taxonomic levels at which data can retrieved. Default
  is `'species'` but data can also be downloaded at family level, genus,
  and taxa group level.

- percenterror:

  `numeric`. The number used as a cutoff to infer similarity of the user
  provided name and what is found in the database. The higher the
  percentage, the higher the similarity the species name provided by the
  user and the one in the database. `percenterror` ranges from 0 to 100
  but the default is 80 to ensure that wrong names are not selected at
  low similarity percentage cutoff.

- errorness:

  `numeric` Similar to `percenterror`, `errorness` parameter uses the
  distance differences between the user-provided names and all the taxa
  group species standard names. The lower the percentage error, the
  higher the similarity in the species names provided. Default is 20 and
  beyond 30, a warning is showed to avoid wrong species replace the user
  provided name, which leads to extracting wrong traits.

- full:

  `logical` `TRUE` if a dataframe with both cleaned and uncleaned
  species are required. If `FALSE` then the a species list will be
  produced after cleaning. Default `FALSE`.

- warn:

  To alert user on the species names cleaning errors and warnings.

- taxalevel:

  `string` Allowed taxonomic levels at which data can retrieved. Default
  is `'species'` but data can also be downloaded at family level, genus,
  and taxa group level.

## Value

`vector or string` clean species name that is also found in the
database.
