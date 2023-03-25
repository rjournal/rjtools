
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/rjournal/rjtools/workflows/R-CMD-check/badge.svg)](https://github.com/rjournal/rjtools/actions)
<!-- badges: end -->

The goal of rjtools is to make it easier for AUTHORS of R Journal
articles to

1.  create a new paper from a template that will produce both html and
    pdf versions in the format needed by the journal, and
2.  to run various checks on their paper, that it satisfies various
    requirements, in preparation for submitting it to the journal.

## Installation

<!-- You can install the released version of rjtools from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("rjtools") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/rjournal/rjtools) with: -->

You can install the development version from
[GitHub](https://github.com/rjournal/rjtools) with:

``` r
# install.packages("remotes")
remotes::install_github("rjournal/rjtools")
```

## Getting started with a new article

To create an article template under the project directory:

``` r
library(rjtools)
create_article(name = "quokka-bilby")
```

To examine how the papers in this format would look in an R Journal
issue, take a look at the June 2021 issue
<https://journal.r-project.org/issues/2021-1/>. Particularly articles by
Wang, and two articles by Kane, and the editorial.

-   To learn more on the article creation, read [Create an R Journal
    Article](https://rjournal.github.io/rjtools/articles/create_article.html).

-   For more information on the R Journal format, specifically how to
    create figures, tables, among others with the new format, read [More
    details on the R Journal
    format](https://rjournal.github.io/rjtools/articles/format-details.html).

-   The rendered template article is showcased in [The Article
    Template](https://rjournal.github.io/rjtools/articles/article_template.html).

## Checking that your paper satisfies various conditions

Explore the various `check_XXX` functions that will check the format of
your article, and some submission requirements.

-   See [Check your Article Before
    Submission](https://rjournal.github.io/rjtools/articles/check_functions.html)
    for more on this.
