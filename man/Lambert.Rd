\name{Lambert}
\alias{Lambert}
\alias{lambert_W0}
\alias{lambert_Wm1}
\title{Lambert's W  function}
\description{
  Lambert's W function as per the Gnu Scientific Library reference manual
  section 7.23
}
\usage{
lambert_W0(x, give=FALSE, strict=TRUE)
lambert_Wm1(x, give=FALSE,strict=TRUE)
}
\arguments{
  \item{x}{input: real values}
    \item{give}{Boolean, with \code{TRUE} meaning to return error info}
    \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
      if nonzero status is returned by the GSL function (\code{FALSE}
      means to return the value: use with caution)}.
}
\references{\url{http://www.gnu.org/software/gsl}}  
\author{Robin K. S. Hankin}
\keyword{array}
