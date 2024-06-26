% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scholar.R
\name{works_scholar}
\alias{works_scholar}
\title{Get works from Google Scholar}
\usage{
works_scholar(is_new = FALSE)
}
\arguments{
\item{is_new}{Logical. Whether to only process new records.}
}
\value{
A data frame for all works obtained from Google Scholar
}
\description{
Get works from Google Scholar
}
\details{
R package `scholar` is used to retrieve works from Google Scholar according to `google-scholar`
field in Tiddlywiki. As Google Scholar does not provide DOI for publications, the publication
is matched according to the same published year and journal and similar title. Consequently,
the matching might not be accurate.

The retrieved works are stored into subfolder `scholar` in the output folder which
is defined by option `output`.

The works will be re-downloaded after 90 days (modifying through option file_expired)
if `is_new` equals to `FALSE`
}
