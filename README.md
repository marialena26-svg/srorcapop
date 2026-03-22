# "srorcapop"" package

## what the package does

An R package made to explore annual population counts of Southern Resident 
killer whales (*Orcinus orca*). The package provides a function to load the 
associated data as well as a Shiny application for interactive visualization.

## why this package is useful

This package allows users to visualize population trends over time and between 
pods as well as births & mortalities events.

## data source

The data used in this package was obtained from the Center for Whale Research 
(CWR) and National Oceanic and Atmospheric Administration (NOAA). All credit 
goes to the original authors.

## installation

``` r
# You can install the package from GitHub, using the development version:
# install.packages("pak")
pak::pak("marialena26-svg/srorcapop")
```
##launching the shiny app
Run:

``` r
 srkw_shiny_app()
```

## getting help

``` r
# to view documentation for the package function, use:
?srkw_shiny_app
```

## author

maintained by Maria Eleni Vogiatzi
