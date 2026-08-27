# Validate an ssfs object

Checks that an ssfs object contains the 8 required tables and that each
table contains its required columns. Returns the object invisibly if
valid; throws an informative error otherwise.

## Usage

``` r
validate_ssfs(x, verbose = TRUE)
```

## Arguments

- x:

  An object to validate, typically created by
  [`new_ssfs()`](https://croquis.comotive.net/reference/new_ssfs.md) or
  [`gtfs_to_ssfs()`](https://croquis.comotive.net/reference/gtfs_to_ssfs.md).

- verbose:

  When TRUE, prints a message if the SSFS is valid.

## Value

`x`, invisibly, if validation passes.

## Examples

``` r
# Run validation. Console messages will indicate missing tables, missing vectors (columns), or incorrect class / data type
if (FALSE) { # \dontrun{
bad <- ligne_jaune
bad$hsh <- NULL
validate_ssfs(bad)
} # }
# "ssfs is valid." message appears if ssfs is valid
validate_ssfs(ligne_jaune,verbose=TRUE)
#> ✔ ssfs is valid.
```
