\name{Ellint}
\alias{Ellint}
\alias{ellint_Kcomp}
\alias{ellint_Ecomp}
\alias{ellint_F}
\alias{ellint_E}
\alias{ellint_P}
\alias{ellint_D}
\alias{ellint_RC}
\alias{ellint_RD}
\alias{ellint_RF}
\alias{ellint_RJ}
\title{Elliptic functions}
\description{
  Elliptic functons as per the Gnu Scientific Library.  See section 7.13
  of the ref manual for  details.
}
\usage{
ellint_Kcomp(k, mode=0, give=FALSE,strict=TRUE)
ellint_Ecomp(k, mode=0, give=FALSE,strict=TRUE)
ellint_F(phi,k, mode=0, give=FALSE,strict=TRUE)
ellint_E(phi,k, mode=0, give=FALSE,strict=TRUE)
ellint_P(phi,k,n, mode=0, give=FALSE,strict=TRUE)
ellint_D(phi,k,n, mode=0, give=FALSE,strict=TRUE)
ellint_RC(x, y, mode=0, give=FALSE,strict=TRUE)
ellint_RD(x, y, z, mode=0, give=FALSE,strict=TRUE)
ellint_RF(x, y, z, mode=0, give=FALSE,strict=TRUE)
ellint_RJ(x, y, z, p, mode=0, give=FALSE,strict=TRUE)
}
\arguments{
  \item{k}{input: real values}
  \item{give}{Boolean: give error}
  \item{strict}{Boolean}
  \item{phi}{input: real values}
  \item{n}{input: real values}
  \item{x}{input: real values}
  \item{y}{input: real values}
  \item{z}{input: real values}
  \item{p}{input: real values}
  \item{mode}{input: mode.  For \code{GSL_PREC_DOUBLE} ,
    \code{GSL_PREC_SINGLE}, \code{GSL_PREC_APPROX} use \code{0,1,2}
    respectively.}
}
\author{Robin K. S. Hankin}
\examples{
ellint_Kcomp(0.3)
ellint_Ecomp(0.3)
ellint_F(0.4,0.7)
ellint_E(0.4,0.7)
ellint_P(0.4,0.7,0.3)
ellint_D(0.4,0.7,0.3)
ellint_RC(0.5,0.6)
ellint_RD(0.5,0.6,0.7)
ellint_RF(0.5,0.6,0.7)
ellint_RJ(0.5,0.6,0.7,0.1)


x <- seq(from=0,to=0.5,by=0.01)
col1 <- ellint_Kcomp(sqrt(x))
col2 <- ellint_Kcomp(sqrt(1-x))
col3 <- exp(-pi*col2/col1)
cbind(x,col1,col2,col3)         #table 17.1, p608

x <- 0:45
col1 <- ellint_Kcomp(sin(pi/180*x))
col2 <- ellint_Kcomp(sin(pi/2-pi/180*x))
col3 <- exp(-pi*col2/col1)
cbind(x,col1,col2,col3)       #table 17.2, p610

x <- seq(from=0,to=90,by=2)
f <- function(a){ellint_F(phi=a*pi/180,sin(x*pi/180))}
g <- function(a){ellint_E(phi=a*pi/180,sin(x*pi/180))}
h <- function(a,n){ellint_P(phi=a*pi/180,sin( a*15*pi/180),n)}
i <- function(x){ellint_P(phi=x*pi/180, k=sin((0:6)*15*pi/180),  n= -0.6)}


cbind(x,f(5),f(10),f(15),f(20),f(25),f(30))          #table 17.5, p613
cbind(x,g(5),g(10),g(15),g(20),g(25),g(30))          #table 17.6, p616



cbind(i(15),i(30),i(45),i(60),i(75),i(90))           #table 17.9,
                                                     #(BOTTOM OF p625)


}
\keyword{array}
