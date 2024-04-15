This revision has small updates, and fixes as requested by CRAN maintainers.

## Test environment

* R version 4.3.3 (2024-02-29 ucrt)

── R CMD check results ───────────────── rjtools 1.0.13 ────
Duration: 24.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* winbuilder

Installation time in seconds: 7
Check time in seconds: 65
Status: OK


With check_rhub, there are two notes, which I believe can be ignored.

── rjtools 1.0.12: NOTE

  Build ID:   rjtools_1.0.12.tar.gz-0420bebe3d074a298f5056193567f16e
  Platform:   Windows Server 2022, R-devel, 64 bit
  Submitted:  2h 53m 53.5s ago
  Build time: 6m 12.7s

❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✔ | 0 warnings ✔ | 2 notes ✖
