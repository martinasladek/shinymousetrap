
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinymousetrap

<!-- badges: start -->
<!-- badges: end -->

The goal of shinymousetRap is to …

## Installation

Install the development version of shinymousetrap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
if(!require(mousetrap)){
  devtools::install_github("debruine/faux")
  devtools::install_github("martinasladek/mousetrap")
}

devtools::install_github("martinasladek/shinymousetrap")
```

Note that the [mousetRap
package](https://github.com/martinasladek/mousetrap) must be installed
in order to use this shiny app.

## Example

Run the app:

``` r
shinymousetrap::mousetrap_checkr()
```
