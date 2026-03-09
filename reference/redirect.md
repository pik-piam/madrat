# redirect

Redirect a given dataset type to a different source folder. The
redirection is local by default, so it will be reset when the current
function call returns. See example for more details.

## Usage

``` r
redirect(type, target, linkOthers = TRUE, local = TRUE)
```

## Arguments

- type:

  Dataset name, e.g. "Tau" to set the source folder that
  [`readTau`](readTau.md) will use

- target:

  Either path to the new source folder that should be used instead of
  the default, or NULL to remove the redirection, or a vector of paths
  to files which are then symlinked into a temporary folder that is then
  used as target folder; if the vector is named the names are used as
  relative paths in the temporary folder, e.g. target = c(\`a/b/c.txt\`
  = "~/d/e/f.txt") would create a temporary folder with subfolders a/b
  and there symlink c.txt to ~/d/e/f.txt.

- linkOthers:

  If target is a list of files, whether to symlink all other files in
  the original source folder to the temporary folder.

- local:

  If TRUE the redirection is only temporary and will be reset when the
  function which calls redirect is finished. Set to FALSE for a
  permanent/global redirection or to an environment for more control.

## Value

Invisibly, the source folder that is now used for the given type

## Details

Redirecting only specific subtypes is not supported to avoid tricky
cases where the subtype is ignored (search for
"getSourceFolder\\.\*subtype = NULL\\").

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
f <- function() {
  redirect("Tau", target = "~/TauExperiment")
  # the following call will change directory
  # into ~/TauExperiment instead of <getConfig("sourcefolder")>/Tau
  readSource("Tau")
}
f()
# Tau is only redirected in the local environment of f,
# so it will use the usual source folder here
readSource("Tau")
} # }
```
