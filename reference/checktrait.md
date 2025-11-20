# Checks the traits spelling compared to user input.

Checks the traits spelling compared to user input.

## Usage

``` r
checktrait(x, std, mindist = 0.3, error = 0.8, grp = NULL, warn = TRUE)
```

## Arguments

- x:

  `string or vector`. The traits to be checked for spelling errors and
  matching database entries.

- std:

  `lits`. A list with standard traits names from the the database to
  compare with user entries.

- mindist:

  `numeric`. Set a threshold for trait similarity between the user
  provided and that found in the database. The lower the percentage, the
  higher the similarity between the user provided and standard trait
  names.

- error:

  `numeric`. Also percentage to improve the distance based checked
  implemented or set in mindist parameter

- grp:

  `grp`. The taxa names checked for. see
  [`fw_searchdata`](https://anthonybasooma.github.io/fwtraits/reference/fw_searchdata.md).

- warn:

  `logical` To show species name warning checks and traits cleaning.
  Default `FALSE`.

## Value

`list or string`. A list, vector or string of cleaned traits names based
on the user provided and standard database traits for downloading.
