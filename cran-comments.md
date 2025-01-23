Updated the dependency, "R >= 4.1.0". All other NOTES at https://cran.r-project.org/web/checks/check_results_rjtools.html which refer to an old version, had already been fixed. 

This revision has fixes addressing issues posted on the Github repo. It is a reasonably small update. 

## Test environment

Checks made using the GitHub Actions at https://github.com/rjournal/rjtools/blob/main/.github/workflows/R-CMD-check.yaml

It checks against the latest release: R version 4.4.2 (2024-10-31) -- "Pile of Leaves" for 
- macOS-latest 
- ubuntu-latest 
- windows-latest

── R CMD check results ────────────────────────────────────── rjtools 1.0.18 ────
Duration: 39.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

and using R CMD CHECK results in 

Status: OK
