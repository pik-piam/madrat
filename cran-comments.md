## NOTES

The R precheck complains that "Author field differs from that derived from Authors@R". We checked
it and the behavior is ok. It is just that the naming of affiliation and ORCID leads to slightly
different formatting when returned, but the information stays the same.

## CHANGES
* fixed CRAN precheck error related to an error detection on a german system

* fixed recently popped up testthat errors caused by stricter behavior of recent testthat versions
* pushed version to most recent release

## Test environments
* local R installation, R 4.1.2
* rhub checks
