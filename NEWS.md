# rjtools 1.0.9

* Errant dontrun removed by CRAN request

# rjtools 1.0.8

* Remove the rendering Rmd from the test-create because plotly does not exist on the CRAN test computer and it fails the automated test.

# rjtools 1.0.7

* Added a number of tests, as requested by CRAN tester.

# rjtools 1.0.6

* Removed writing to default directories, as requested by CRAN tester.

# rjtools 1.0.5

* More tests
* Changes to checks
* Fixes to web issue articles to not have page numbers
* create_article now uses rmarkdown::draft()
* Section title "References" automatically added in pdf file
* Added section numbering to HTML output to match PDF style

# rjtools 1.0.4

* Fix a typo in the README

# rjtools 1.0.3

* Changes requested by CRAN
* DESCRIPTION and some function arguments modified

# rjtools 1.0.2

* Fixes for CRAN
* Additions to build a full issue

# rjtools 1.0.1

* Self-contained html is default
* remove check on RJwrapper.pdf (#46)

# rjtools 1.0.0

* Depends on distill instead of rjdistill
* `csl` and `sty` files built into system files
* Successfully tested article creation in different three modes, through RStudio dropdown, from RStudio console and terminal R using `create_article()`. 

# rjtools 0.0.0.9002

* Merge rjdistill with rjtools
* Use `file.path` instead of glue to construct file path in `create_article` 
* Generate the template with customised bib file name (#21)
* Add Rjournal csl file in the template
* Add details on files created in the `create_article` vignette 
* `check_title` suggests correct title format for article title (#22)
* Designated csl file for journal articles
* Add RStudio project template: R Journal Article (#19)
* Add github links (#18)

# rjtools 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.
