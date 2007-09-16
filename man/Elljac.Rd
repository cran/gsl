\name{Elljac}
\alias{Elljac}
\alias{elljac}
\alias{sn}
\alias{cn}
\alias{dn}
\alias{ns}
\alias{nc}
\alias{nd}
\alias{sc}
\alias{sd}
\alias{cs}
\alias{cd}
\alias{ds}
\alias{dc}
\title{Elliptic functions}
\description{
  Elljac functions as per the Gnu Scientific Library, reference manual
  section 7.14 and AMS-55, chapter 16.  These functions are
  declared in header file \code{gsl_sf_elljac.h}
}
\usage{
elljac(u, m, give=FALSE, strict=TRUE)
sn(z,m)
cn(z,m)
dn(z,m)
ns(z,m)
nc(z,m)
nd(z,m)
sc(z,m)
sd(z,m)
cs(z,m)
cd(z,m)
ds(z,m)
dc(z,m)
}
\arguments{
  \item{u,m}{input: real values}
  \item{z}{input: complex values}
  \item{give}{Boolean with \code{TRUE} meaning to return a list of three
    items: the value, an estimate of the error, and a status number}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if status is an error}
}
\details{
  A straightforward wrapper for the \code{gsl_sf_elljac_e}
  function of the GSL library, except for \code{sn()}, \code{cn()}, and
  \code{dn()}, which implement 16.21.1 to 16.21.4  (thus taking complex
  arguments); and \code{ns()} et
  seq which are the minor elliptic functions.
}
\references{\url{http://www.gnu.org/software/gsl}}
\author{Robin K. S. Hankin}
\examples{

K <- ellint_F(phi=pi/2,k=sqrt(1/2))  #note the sqrt: m=k^2
u <- seq(from=0,to=4*K,by=K/24)
jj <- elljac(u,1/2)
plot(u,jj$sn,type="l",xaxt="n",yaxt="n",bty="n",ylab="",xlab="",main="Fig 16.1, p570")
lines(u,jj$cn,lty=2)
lines(u,jj$dn,lty=3)
axis(1,pos=0,at=c(K,2*K,3*K,4*K),labels=c("K","2K","3K","4K"))
abline(0,0)
axis(2,pos=0,at=c(-1,1))
text(1.8*K,0.6,"sn u")
text(1.6*K,-0.5,"cn u")
text(2.6*K,0.9,"dn u")



 a <- seq(from=-5,to=5,len=100)
jj <- outer(a,a,function(a,b){a})
z <- jj+1i*t(jj)
e <- Re(cd(z,m=0.2))
e[abs(e)>10] <- NA
contour(a,a,e,nlev=55)

}

\keyword{array}
