\name{Clausen}
\alias{Clausen}
\alias{clausen}
\title{Clausen functions}
\description{
Clausen functons as per the Gnu Scientific Library section 7.6
}
\usage{
clausen(x, give=FALSE, strict=TRUE)
}
\arguments{
  \item{x}{input: real values}
  \item{give}{Boolean with \code{TRUE} meaning to return a list of three
    items: the value, an estimate of the error, and a status number.}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if status is an error.}
}
\author{Robin K. S. Hankin}
\examples{

x <- (0:30)*pi/180
clausen(x)          #table 27.8, p1006

}

\keyword{array}
