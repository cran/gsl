\name{Gegenbauer}
\alias{Gegenbauer}
\alias{gegenbauer}
\alias{gegenpoly_1}
\alias{gegenpoly_2}
\alias{gegenpoly_3}
\alias{gegenpoly_n}
\alias{gegenpoly_array}
\title{Gegenbauer functions}
\description{
Gegenbauer functions as per the Gnu Scientific Library section 7.20 and A&S
}
\usage{
gegenpoly_1(lambda, x, give=FALSE,strict=TRUE)
gegenpoly_2(lambda, x, give=FALSE,strict=TRUE)
gegenpoly_3(lambda, x, give=FALSE,strict=TRUE)
gegenpoly_n(n,lambda, x, give=FALSE,strict=TRUE)
gegenpoly_array(nmax,lambda, x, give=FALSE,strict=TRUE)
}
\arguments{
  \item{lambda}{input: real values}
  \item{x}{input: real values}
  \item{n}{input: integer value}
  \item{nmax}{input: integer value}
  \item{give}{Boolean, with \code{TRUE} meaning to return error info}
  \item{strict}{Boolean}
}

\author{Robin K. S. Hankin}
\examples{
x <- seq(from=-1, to=1, by=0.05)
gegenpoly_1(0.5 , x)
gegenpoly_2(0.5 , x)
gegenpoly_3(0.5 , x)

gegenpoly_n(6, 0.5 , x)
gegenpoly_array(6, 0.5 , x)

x <- seq(from=-1 ,to=1,len=30)
y <- gegenpoly_array(6,0.5,x)
 matplot(x,t(y[-(1:2),]),type="l")   #figure 22.4, page 776


plot(x,xlim=c(-1,1),ylim=c(-1.5,1.5),type="n",main="Figure 22.5, p777")
lines(x, gegenpoly_n(5,lambda=0.2, x,give=FALSE,strict=TRUE))
lines(x, gegenpoly_n(5,lambda=0.4, x,give=FALSE,strict=TRUE))
lines(x, gegenpoly_n(5,lambda=0.6, x,give=FALSE,strict=TRUE))
lines(x, gegenpoly_n(5,lambda=0.8, x,give=FALSE,strict=TRUE))
lines(x, gegenpoly_n(5,lambda=1.0, x,give=FALSE,strict=TRUE))
abline(h=0)
abline(v=0)




}
\keyword{array}
