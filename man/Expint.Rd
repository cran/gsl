\name{Expint}
\alias{Expint}
\alias{expint_E1}
\alias{expint_E2}
\alias{expint_Ei}
\alias{Shi}
\alias{Chi}
\alias{expint_3}
\alias{Si}
\alias{Ci}
\alias{atanint}
\title{exponential functions}
\description{
Expint functions as per the Gnu Scientific Library and A&S
}
\usage{
expint_E1(x,give=FALSE,strict=TRUE)
expint_E2(x,give=FALSE,strict=TRUE)
expint_Ei(x,give=FALSE,strict=TRUE)
Shi(x,give=FALSE,strict=TRUE)
Chi(x,give=FALSE,strict=TRUE)
expint_3(x,give=FALSE,strict=TRUE)
Si(x,give=FALSE,strict=TRUE)
Ci(x,give=FALSE,strict=TRUE)
atanint(x,give=FALSE,strict=TRUE)
}
\arguments{
  \item{x}{input: real values}
  \item{give}{Boolean, with \code{TRUE} meaning to return error info}
  \item{strict}{Boolean}
}
\references{\url{http://www.gnu.org/software/gsl}}
\author{Robin K. S. Hankin}
\examples{
x <- seq(from=0.5, to=1, by=0.01)
cbind(x,Si(x),Ci(x),expint_Ei(x),expint_E1(x))  #table 5.1 of AS, p239

x <- seq(from=0, to=12, len=100)
plot(x,Ci(x),col="black",type="l",xaxt="n",yaxt="n",bty="n",xlab="",ylab="",main="Figure 5.6, p232",xlim=c(0,12),ylim=c(-1,2.0))
lines(x,Si(x))
axis(1,pos=0)
axis(2,pos=0)
 abline(h=pi/2,lty=2)

}
\keyword{array}
