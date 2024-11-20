## Test environments
* local OS X, install, 4.1.0 (2021-05-18)
* github actions : 
* windows-latest, r: 'release' 
* macOS-latest, r: 'release'
* ubuntu-latest, r: 'release' 

## R CMD check results

0 errors | 0 warnings | 0 note


We don't expect any reverse dependency failures.

This patch release contains only fix deprecated dependencies (issue #200).

## cran check v0.18.0
Some tests are commented cause NOTE during pretest

Flavor: r-devel-linux-x86_64-debian-gcc
Check: tests, Result: NOTE
    Running 'testthat.R' [143s/16s]
  Running R code in 'testthat.R' had CPU time 8.9 times elapsed time

## v0.18.1
new version to fix pb with dependencies `AntaresRead` :  
 - "M1mac" issue  
 
We have to fix dependencies with package `rhdf5` on this package before to push `antaresRead` 2.7.1

## v0.18.2
Cran check in ERROR cause test skip not skip 

## v0.18.3
To put the package back on CRAN

