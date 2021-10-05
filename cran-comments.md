## CHANGES
* compatibility fixes to work with magclass > 6.0.0
* improving caching algorithm (automatic detection of outdated calculations)
* various smaller improvement and fixes

## NOTE
* As this version of madrat relies on a newer version of magclass (>=5.7.0) which just recently has been published on CRAN, tests on systems which have not yet installed the newest version of magclass might fail. 
* compared to the most recent submission this one comes with adjusted skip-statements in tests to address the issue of unreachable zenodo-resources on the windows test machine.
* skipping installedMadratUniverse example as its total runtime depends on the system it is run on
