\name{nz_fiscal_series_history}
\alias{nz_fiscal_series_history}
\title{Replay one continuous fiscal series}
\usage{
nz_fiscal_series_history(x, series_id)
}
\arguments{
\item{x}{An object returned by a \code{read_nz_*()} function.}
\item{series_id}{Exact SHA-256 identity returned by
\code{nz_fiscal_series_catalogue()}.}
}
\value{
An \code{nz_fiscal_data} object containing the complete, date-ordered source
rows for the selected structural identity, including point-level annotations
and the original source provenance.
}
\description{
Replays a canonical fiscal series without treating point-level annotations as
separate time series. Missing or duplicate dates fail closed.
}
\seealso{
\code{\link{nz_fiscal_series_catalogue}}
}
