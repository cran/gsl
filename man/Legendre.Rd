\name{Legendre}
\alias{Legendre}
\alias{legendre_P1}
\alias{legendre_P2}
\alias{legendre_P3}
\alias{legendre_Pl}
\alias{legendre_Pl_array}
\alias{legendre_Q0}
\alias{legendre_Q1}
\alias{legendre_Ql}
\alias{legendre_Plm}
\alias{legendre_Plm_array}
\alias{legendre_sphPlm}
\alias{legendre_sphPlm_array}
\title{Legendre functions}
\description{
Legendre functons as per the Gnu Scientific Library
}
\usage{
legendre_P1  (x,give=FALSE,strict=TRUE)
legendre_P2  (x, give=FALSE,strict=TRUE)
legendre_P3  (x, give=FALSE,strict=TRUE)
legendre_Pl  (l,x, give=FALSE,strict=TRUE)
legendre_Pl_array(lmax,x,give=FALSE,strict=TRUE)
legendre_Q0  (x, give=FALSE,strict=TRUE)
legendre_Q1  (x, give=FALSE,strict=TRUE)
legendre_Ql  (l,x, give=FALSE,strict=TRUE)
legendre_Plm(l,m,x, give=FALSE,strict=TRUE)
legendre_Plm_array(lmax,m,x,give=FALSE,strict=TRUE)
legendre_sphPlm(l,m,x, give=FALSE,strict=TRUE)
legendre_sphPlm_array(lmax,m,x,give=FALSE,strict=TRUE)
  
}
\arguments{
  \item{x}{input: real values}
  \item{l}{input: integer values}
  \item{m}{input: integer values}
  \item{lmax}{input: integer values}
  \item{give}{Boolean, with default \code{FALSE} meaning to
    return just the answers, and \code{TRUE} meaning to return a status
    vector as well.}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if nonzero status is returned by the GSL function (\code{FALSE}
    means to return the value: use with caution)}.
}
\author{Robin K. S. Hankin}
\examples{
 theta <- seq(from=0,to=pi/2,len=100)
 plot(theta,legendre_P1(cos(theta)),type="l",ylim=c(-0.5,1),main="Figure 8.1 of Abramowitz and Stegun")
 abline(1,0)
 lines(theta,legendre_P2(cos(theta)),type="l")
 lines(theta,legendre_P3(cos(theta)),type="l")

x <- seq(from=0,to=1,len=600)
plot(x, legendre_Plm(3,1,x), type="l",lty=3,main="Figure 8.2 of A and S")
lines(x,legendre_Plm(2,1,x), type="l",lty=2)
lines(x,legendre_Plm(1,1,x), type="l",lty=1)
abline(0,0)


plot(x,legendre_Ql(0,x),xlim=c(0,1), ylim=c(-1,1.5), type="l",lty=1, main="Figure 8.4 of A and S")
lines(x,legendre_Ql(1,x),lty=2)
lines(x,legendre_Ql(2,x),lty=3)
lines(x,legendre_Ql(3,x),lty=4)
abline(0,0)

#table 8.1 of A&S:
t(legendre_Pl_array(10,seq(from=0,to=1,by=0.01))[1+c(2,3,9,10),])

#table 8.3:
f <- function(n){legendre_Ql(n,seq(from=0,to=1,by=0.01))}
sapply(c(0,1,2,3,9,10),f)


}

\keyword{array}
