\name{Coupling}
\alias{Coupling}
\alias{coupling_3j}
\alias{coupling_6j}
\alias{coupling_9j}
\title{Coupling functions}
\description{
Coupling functons as per the Gnu Scientific Library, reference manual
section 7.8
}
\usage{
coupling_3j(two_ja, two_jb, two_jc, two_ma, two_mb, two_mc, give=FALSE, strict=TRUE)
coupling_6j(two_ja, two_jb, two_jc, two_jd, two_je, two_jf, give=FALSE, strict=TRUE)
coupling_9j(two_ja, two_jb, two_jc, two_jd, two_je, two_jf, two_jg, two_jh, two_ji, give=FALSE, strict=TRUE)
}
\arguments{
  \item{two_ja}{see gsl ref man}
  \item{two_jb}{see gsl ref man}
  \item{two_jc}{see gsl ref man}
  \item{two_jd}{see gsl ref man}
  \item{two_je}{see gsl ref man}
  \item{two_jf}{see gsl ref man}
  \item{two_jg}{see gsl ref man} 
  \item{two_jh}{see gsl ref man}
  \item{two_ji}{see gsl ref man}
  \item{two_ma}{see gsl ref man}
  \item{two_mb}{see gsl ref man}
  \item{two_mc}{see gsl ref man}
  \item{give}{Boolean with \code{TRUE} meaning to return a list of three
    items: the value, an estimate of the error, and a status number.}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if status is an error.}
}
\author{Robin K. S. Hankin}
\examples{
coupling_3j(1,2,3,4,5,6)
coupling_6j(1,2,3,4,5,6)
coupling_9j(1,2,3,4,5,6,7,8,9)
}
\keyword{array}
