This revision has some changes to fix some errors, and streamline usage and submission operations.

## Test environment, using devtools::check_rhub()

* local R installation: R version 4.3.1 (2023-06-16)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Fedora Fedora Linux, R-devel, clang, gfortran
* Windows Server 2022, R-devel, 64 bit

── R CMD check results ──────────────────────────────────── rjtools 1.0.12 ────
Duration: 29.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

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
