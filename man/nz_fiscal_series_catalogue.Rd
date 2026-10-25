\name{nz_fiscal_series_catalogue}
\alias{nz_fiscal_series_catalogue}
\title{Catalogue continuous chart series in parsed fiscal data}
\usage{
nz_fiscal_series_catalogue(x)
}
\arguments{
\item{x}{An object returned by a \code{read_nz_*()} function.}
}
\value{
A data frame with one row per canonical series, exact identity hash,
coverage, observation count, and semantic replay payload.
}
\description{
Builds canonical series identities from structural dimensions such as
accounting basis, consolidation, unit, regime, and actual/forecast status.
Observation-level HFI and EFU annotations remain on source rows and do not
fragment otherwise continuous histories. Monthly financial statements remain
release-snapshot observations.
}
\seealso{
\code{\link{nz_fiscal_series_history}}
}
