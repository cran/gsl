\name{Trig}
\alias{Trig}
\alias{gsl_sf_sin}
\alias{gsl_sf_cos}
\title{Trig functions}
\description{
Trig functons as per the Gnu Scientific Library, section 7.30
}
\usage{
gsl_sf_sin(x, give=FALSE, strict=TRUE)
gsl_sf_cos(x, give=FALSE, strict=TRUE)
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

x <- seq(from=0,to=2,by=0.01)
gsl_sf_sin(x)   #table xx of Ab and St
gsl_sf_cos(x)   #table xx of Ab and St

f <- function(x){abs(sin(x+1)-sin(x)*cos(1)-cos(x)*sin(1))}
g <-
function(x){abs(gsl_sf_sin(x+1)-gsl_sf_sin(x)*gsl_sf_cos(1)-gsl_sf_cos(x)*gsl_sf_sin(1))}

f(100000:100010)
g(100000:100010)


}


\keyword{array}
