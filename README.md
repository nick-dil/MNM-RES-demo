
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Exercise RSE vacature MNM (INBO)

**NOTE: This is a toy package created for demonstration purposes ONLY
!**

- The R-package’s name originates from its function: to read, calculate
  and compare the **spg** or “SteekProefGrootte” (sample_size) for
  different datasets
- The GitHub repository is named for the meta-purpose: **demonstrating R
  coding skills**
  - Evidently its not to showcase my *spelling* skills as **R**esearch
    **S**oftware **E**ngineer should have been abbreviated to RSE
    instead of RES in `MNM-RES-demo`

# spg

<!-- badges: start -->

<!-- badges: end -->

The goal of spg is to demonstrate R skills in context of a vacature for
RSE at team MNM (INBO)

- Load data from shared google drive
- manipulate and aggregate data into required measure (sample_size =
  “steekproefgrootte”)
- plot measure to compare datasets

## Installation

You can install the development version of spg from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nick-dil/MNM-RES-demo")
```

## Example

> ***NOTE:*** See the `examples/00_tutorial.R`

This is a basic example which shows you how to solve the exercise:

``` r
library(spg)
library(tidyverse)

# Retrieve POC dir ids ~ version
poc_id_list = getPocIdList()

# Read and create aggregated sample size ~ dataset, see exercise
demo.v131 = readPocSampleData(poc_id_list$poc_0.13.1) %>% 
  makeSpgTable() %>% wrangleSpgTable()

demo.v140 = readPocSampleData(poc_id_list$poc_0.14.0) %>% 
  makeSpgTable() %>% wrangleSpgTable()
```

Inspect a dataset:

``` r
summary(demo.v140)
#>    meetnet          hydr_class      id            steekproefgrootte
#>  Length:2           HC1 :0     Length:2           Min.   : 382     
#>  Class :character   HC12:1     Class :character   1st Qu.: 571     
#>  Mode  :character   HC2 :1     Mode  :character   Median : 760     
#>                     HC23:0                        Mean   : 760     
#>                     HC3 :0                        3rd Qu.: 949     
#>                                                   Max.   :1138
```

And compare the sample_size between datasets with some exploratory
plots:

``` r

# Make comparison plots
demo_plots = plotSpgComparison(demo.v131, demo.v140)

demo_plots$spg_barplot
```

<img src="man/figures/README-plots-1.png" width="100%" />

``` r
demo_plots$spg_diff_barplot
```

<img src="man/figures/README-plots-2.png" width="100%" />

``` r
demo_plots$spg_scatter
```

<img src="man/figures/README-plots-3.png" width="100%" />
