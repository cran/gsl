\name{Dawson}
\alias{Dawson}
\alias{dawson}
\title{Dawson functions}
\description{
Dawson functons as per the Gnu Scientific Library, reference manual
section 7.9
}
\usage{
dawson(x, give=FALSE, strict=TRUE)
}
\arguments{
  \item{x}{input: real values}
  \item{give}{Boolean with \code{TRUE} meaning to return a list of three
    items: the value, an estimate of the error, and a status number.}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if status is an error.}
}
\references{\url{http://www.gnu.org/software/gsl}}
\author{Robin K. S. Hankin}
\examples{

x <- seq(from=0,to=2,by=0.01)
dawson(x)   #table 7.5 of Ab and St
}

\keyword{array}
