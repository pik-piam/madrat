% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFromComment.R
\name{getFromComment}
\alias{getFromComment}
\title{getFromComment}
\usage{
getFromComment(x, name)
}
\arguments{
\item{x}{object the metadata should be extracted from}

\item{name}{name of the metadata to be extracted (e.g. unit)}
}
\description{
Helper function extract a metadata comment
}
\examples{
x <- as.magpie(1)
getComment(x) <- c(" description: example description", " unit: kg")
getFromComment(x, "unit")
getFromComment(x, "description")
}
\author{
Jan Philipp Dietrich
}
