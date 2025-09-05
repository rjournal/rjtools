Note that there is a new maintainer. This package maintainer needs to  
travel with the technical editor of the R Journal. The contact email 
is the an address, maintained by the R Foundation, will be 
forwarded to the current technical editor. 

- Fixed error reported by CRAN maintainers. 
- Several small fixes on document handling, as described in the NEWS.

## Test environment

Checks made using the GitHub Actions rhub.yml with details at https://github.com/rjournal/rjtools/actions/runs/17480905231.

Passes checks for 

- linux          R-* (any version)                     ubuntu-latest
- m1-san         R-* (any version)                     macos-15
- macos          R-* (any version)                     macos-13
- macos-arm64    R-* (any version)                     macos-13
- windows        R-* (any version)                     windows-latest

### devtools check

── R CMD check results ──────────────────────────── rjtools 1.0.20 ────
Duration: 33s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

### R CMD check

R CMD CHECK ../rjtools_1.0.20.tar.gz 

* DONE

Status: OK

## Reverse dependencies checked
── CHECK ──────────────────────────────────────────────── 1 packages ──
✔ texor 1.5.6                            ── E: 0     | W: 0     | N: 0    
OK: 1                                                                
BROKEN: 0
