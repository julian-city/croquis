# Generate trip departure times for a time range

Internal function that generates trip departure times for a specified
range, based on a first departure and a last departure (for example the
bounds of a service window) and based on the hsh values of the ssfs, for
a specific itin_id and service_id. Used within ssfs_to_gtfs() as well as
within the cost calculator function

## Usage

``` r
trip_dep_generator(ssfs, first_dep, last_dep, itin_id_i, service_id_i)
```

## Arguments

- ssfs:

  A list of class SSFS

- first_dep:

  A string indicating first departure time in HH:MM:SS format

- last_dep:

  A string indicating last departure time in HH:MM:SS format

- itin_id_i:

  A string indicating a specific itin_id

- service_id_i:

  A string indicating a specific service_id

## Value

A vector of strings of trip departure times in HH:MM:SS format
