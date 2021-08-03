
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjtools

<!-- badges: start -->
<!-- badges: end -->

The goal of rjtools is to make it easier for AUTHORS of R Journal
articles to start a new paper, and to check their paper is in suitable
condition to submit.

## Installation

<!-- You can install the released version of rjtools from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("rjtools") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rjournal/rjdistill")
remotes::install_github("rjournal/rjtools")
```

## Example

To create an article template under the project directory:

``` r
library(rjtools)
create_article(file_name = "my_article")
```

## Check functions

Explore the various `check_XXX` functions that will check the format of
your article, and some submission requirements.
