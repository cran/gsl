\name{Powint}
\alias{pow_int}
\alias{powint}
\title{Power functions}
\description{
Power functons as per the Gnu Scientific Library
}
\usage{
pow_int(x, n, give=FALSE, strict=TRUE)
}
\arguments{
  \item{x}{input: real values}
  \item{n}{input: integer  values}
  \item{give}{Boolean with \code{TRUE} meaning to return a list of three
    items: the value, an estimate of the error, and a status number.}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if status is an error.}
}
\author{Robin K. S. Hankin}
\examples{

pow_int(pi/2,1:10)   
}

\keyword{array}
