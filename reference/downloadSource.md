# downloadSource

Download a source. The function is a wrapper for specific functions
designed for the different possible source types.

## Usage

``` r
downloadSource(type, subtype = NULL, overwrite = FALSE, numberOfTries = 300)
```

## Arguments

- type:

  source type, e.g. "IEA". A list of all available source types can be
  retrieved with function [`getSources`](getSources.md)`("download")`.

- subtype:

  For some sources there are subtypes of the source, for these source
  the subtype can be specified with this argument. If a source does not
  have subtypes, subtypes should not be set.

- overwrite:

  Boolean deciding whether existing data should be overwritten or not.

- numberOfTries:

  Integer determining how often readSource will check whether a running
  download is finished before exiting with an error. Between checks
  readSource will wait 30 seconds. Has no effect if the sources that
  should be read are not currently being downloaded.

## Note

The underlying download-functions are required to provide a list of
information back to `downloadSource`. Following list entries should be
provided:

- **url** - full path to the file that should be downloaded

- **title** - title of the data source

- **author** - author(s) of the data set

- **license** - license of the data set. Put **unknown** if not
  specified.

- **description** - description of the data source

- **unit** - unit(s) of the data

- **doi** (optional) - a DOI URL to the data source

- **version** (optional) - version number of the data set

- **release_date** (optional) - release date of the data set

- **reference** (optional) - A reference for the data set (e.g. a paper,
  if the data was derived from it)

This user-provided data is enriched by automatically derived metadata:

- **call** - Information about the used madrat function call to download
  the data will check whether there are any values below the given
  threshold and warn in this case

- **accessibility** - A measure of quality for the accessibility of the
  data. Currently it distinguished between **iron** (manual access),
  **silver** (automatic access via URL) and **gold** (automatic access
  via DOI).

Besides the names above (user-provided and automatically derived) it is
possible to add custom metadata entries by extending the return list
with additional, named entries.

## See also

[`setConfig`](setConfig.md), [`readSource`](readSource.md)

## Author

Jan Philipp Dietrich, David Klein, Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
a <- downloadSource("Tau", subtype = "historical")
} # }
```
