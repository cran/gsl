\name{Elljac}
\alias{Elljac}
\alias{elljac}
\title{Elliptic functions}
\description{
Elljac functons as per the Gnu Scientific Library, section 7.14
}
\usage{
elljac(u, m, give=FALSE, strict=TRUE)
}
\arguments{
  \item{u}{input: real values}
  \item{m}{input: real values}
  \item{give}{Boolean with \code{TRUE} meaning to return a list of three
    items: the value, an estimate of the error, and a status number.}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if status is an error.}
}
\author{Robin K. S. Hankin}
\examples{

x <- seq(from=0,to=2,by=0.01)
elljac(x,0.4)   
}

\keyword{array}
