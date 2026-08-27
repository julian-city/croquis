# Subset SSFS

A handy function that outputs a subset of an input ssfs. You can specify
whether you want to remove or retain a based on a set of itin_ids or
route_ids.

## Usage

``` r
ssfs_subset(
  ssfs,
  subset_id,
  operation = c("retain", "remove"),
  id_type = c("itin_id", "route_id")
)
```

## Arguments

- ssfs:

  An input SSFS

- subset_id:

  The set of itin_ids or route_ids that you would like to remove or
  retain

- operation:

  Specify whether you would like the output ssfs to retain (default) or
  remove the data associated with subset_id

- id_type:

  Specify whether subset_id is itin_id (default) or route_id

## Value

A SSFS

## Examples

``` r
#Create a subset SSFS that only includes data pertaining to route 160 of the mileend network
ssfs_160 <- ssfs_subset(mileend,subset_id="160",id_type="route_id")

#create a subset SSFS that excludes secondary itineraries for various routes in of the mileend network
ssfs_mileend_clean <- ssfs_subset(mileend,subset_id=c("160_0_2","161_1_2"),operation="remove")
```
