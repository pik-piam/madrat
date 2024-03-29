% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadSource.R
\name{downloadSource}
\alias{downloadSource}
\title{downloadSource}
\usage{
downloadSource(type, subtype = NULL, overwrite = FALSE, numberOfTries = 300)
}
\arguments{
\item{type}{source type, e.g. "IEA". A list of all available source types can be retrieved with
function \code{\link{getSources}("download")}.}

\item{subtype}{For some sources there are subtypes of the source, for these source the subtype can be specified
with this argument. If a source does not have subtypes, subtypes should not be set.}

\item{overwrite}{Boolean deciding whether existing data should be overwritten or not.}

\item{numberOfTries}{Integer determining how often readSource will check whether a running download is finished
before exiting with an error. Between checks readSource will wait 30 seconds. Has no effect if the sources that
should be read are not currently being downloaded.}
}
\description{
Download a source. The function is a wrapper for specific functions designed
for the different possible source types.
}
\note{
The underlying download-functions are required to provide a list of information
back to \code{downloadSource}. Following list entries should be provided:
\itemize{
\item \bold{url} - full path to the file that should be downloaded
\item \bold{title} - title of the data source
\item \bold{author} - author(s) of the data set
\item \bold{license} - license of the data set. Put \bold{unknown} if not specified.
\item \bold{description} - description of the data source
\item \bold{unit} - unit(s) of the data
\item \bold{doi} (optional) - a DOI URL to the data source
\item \bold{version} (optional) - version number of the data set
\item \bold{release_date} (optional) - release date of the data set
\item \bold{reference} (optional) - A reference for the data set (e.g. a paper, if the data was derived from it)
}
This user-provided data is enriched by automatically derived metadata:
\itemize{
\item \bold{call} - Information about the used madrat function call to download the data
will check whether there are any values below the given threshold and warn in this case
\item \bold{accessibility} - A measure of quality for the accessibility of the data. Currently it distinguished
between \bold{iron} (manual access), \bold{silver} (automatic access via URL) and \bold{gold} (automatic access via
DOI).
}
Besides the names above (user-provided and automatically derived) it is possible to add custom metadata entries by
extending the return list with additional, named entries.
}
\examples{
\dontrun{
a <- downloadSource("Tau", subtype = "historical")
}
}
\seealso{
\code{\link{setConfig}}, \code{\link{readSource}}
}
\author{
Jan Philipp Dietrich, David Klein, Pascal Sauer
}
