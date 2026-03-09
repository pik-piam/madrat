# Compare a madrat function's output with and without your changes

With \`compareMadratOutputs\` you can easily compare the output of a
madrat function (read, calc, ...) with and without your changes. First,
run \`compareMadratOutputs\` without your changes, so a \`.rds\` file
with the original output is saved. Then apply your changes and run
\`compareMadratOutputs\` again to compare the new output to the original
output.

## Usage

``` r
compareMadratOutputs(package, functionName, subtypes, overwriteOld = FALSE)
```

## Arguments

- package:

  \[character(1)\] The package where the given function is located. It
  will be attached via \`library\`.

- functionName:

  \[character(1)\] The name of the function from which you want to
  compare outputs. Must be a madrat function whose name starts with
  read, correct, convert, or calc.

- subtypes:

  \[character(n)\] The subtypes you want to check. For calc functions
  this must be NULL.

- overwriteOld:

  If TRUE: overwrite a "\*-old-\*.rds" previously created with
  compareMadratOutputs.

## Value

Invisibly the result of \`waldo::compare\` or \`all.equal\` if a
comparison was made, otherwise a named list of the outputs for each
subtype.

## Details

If there are differences a \`\<functionName\>-new.rds\` containing the
new output is saved for closer inspection. All files are created in the
current working directory.

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
# save original output to readTau-old.rds
compareMadratOutputs("madrat", "readTau", c("paper", "historical"))

# now apply your changes to madrat:::readTau, reinstall madrat, restart the R session

# compare new output to original output from readTau-old.rds
compareMadratOutputs("madrat", "readTau", c("paper", "historical"))
} # }
```
