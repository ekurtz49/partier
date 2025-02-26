% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partier.R
\name{duplicate_minima}
\alias{duplicate_minima}
\title{Duplicated Minimum Detection}
\usage{
duplicate_minima(x)
}
\arguments{
\item{x}{vector}
}
\description{
This helper function detects whether a given vector has a unique minimum value.
Returns a 0 (FALSE) if the minimum value in the vector is unique and a 1 (TRUE) if minimum is not unique.
}
\examples{
## Detect whether a vector contains a unique minimum
duplicate_minima(c(1,2,3)) # visually does (1 is unique)
duplicate_minima(c(1,1,3)) # visually does not (1 is not unique, appears twice)

}
\keyword{duplicate}
