# rjtools 1.0.17

* Added technical editors to issue template.
* Fixed missing resources for building table of contents for issues.
* Improved identification and handling of legacy pdf articles
* Use natbib for pdf articles (fixes issues with citation rendering)
* Fixed article buttons (pdf, supplementary files, citation) not showing when abstract is missing.

# rjtools 1.0.16

* Update template article to deal better with the pandoc problem
* All CRAN lookups now use cran.R-project.org. This resolves problems with some CRAN-like repos (e.g. RSPM) not containing CRAN Views data.

# rjtools 1.0.15

* Pull request #14 fix (fix #122)

# rjtools 1.0.14

* additional issues with checks fixed to completely address issue #119 

# rjtools 1.0.13

* fix error reported by CRAN, occurring on linux, related to test code
* Fixes in response to issues #119 and #120
* web site documentation and vignettes updated

# rjtools 1.0.12

* Updates for accessibility based on updates in distill package.
* File structure for template article is now cleaner, with data, figures, scripts, motivation-letter, folders. 
* Added `rjournal_article()` as a preferred alias to `rjournal_web_article()`
  since this output format generally produces both HTML and PDF outputs.
* This is reflected in the change in the YAML header to be `rjtools::rjournal_article`. This generates both html and pdf. `rjtools::rjournal_web_article`, which will now only generate the html. `rjtools::rjournal_pdf_article` now only generates the pdf.
* Check functions updated, including DOIs in references, title case in bib file, existence of csl file
* Reference to new web site in the documentation.
* New function to assist authors prepare for a release.
* Style to better adhere to accessibility standards.
* Remove link to twitter/x.
* Handling of longtable.
* Utilise system sty file, not author supplied.

# rjtools 1.0.11

* removed return value, and set create_article to write a message
* set a default name for the paper

# rjtools 1.0.10

* available_packages() is used in check_packages_available() instead of cranlogs::cran_downloads()

# rjtools 1.0.9.9002

* Information about referencing in figure captions, and conditional figure inclusions depending on output. 

# rjtools 1.0.9.9001

* Import BioCManager
* Import artifacts
* Update authors
* Update tests
* Bad example added

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
