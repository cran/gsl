\name{Laguerre}
\alias{Laguerre}
\alias{laguerre}
\alias{laguerre_1}
\alias{laguerre_2}
\alias{laguerre_3}
\alias{laguerre_n}
\title{Laguerre functions}
\description{
  Laguerre functons as per the Gnu Scientific Library reference manual
  section 7.22
}
\usage{
laguerre_1(a, x, give=FALSE, strict=TRUE)
laguerre_2(a, x, give=FALSE, strict=TRUE)
laguerre_3(a, x, give=FALSE, strict=TRUE)
laguerre_n(n, a, x, give=FALSE, strict=TRUE)
}
\arguments{
  \item{a}{input: real values}
  \item{x}{input: real values}
  \item{n}{input: integer values}
    \item{give}{Boolean, with \code{TRUE} meaning to return error info}
    \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
      if nonzero status is returned by the GSL function (\code{FALSE}
      means to return the value: use with caution)}.
}
\author{Robin K. S. Hankin}
\example{
  laguerre_1(0.1,0.3)
  laguerre_2(0.1,0.3)
  laguerre_3(0.1,0.3)
  laguerre_n(4,0.1,0.3)
  }
\keyword{array}
