\name{Bessel}
\alias{Bessel}
\alias{bessel_J0}
\alias{bessel_J1}
\alias{bessel_Jn}
\alias{bessel_Jn_array}
\alias{bessel_Y0}
\alias{bessel_Y1}
\alias{bessel_Yn}
\alias{bessel_Yn_array}
\alias{bessel_I0}
\alias{bessel_I1}
\alias{bessel_In}
\alias{bessel_In_array}
\alias{bessel_I0_scaled}
\alias{bessel_I1_scaled}
\alias{bessel_In_scaled}
\alias{bessel_In_scaled_array}
\alias{bessel_K0}
\alias{bessel_K1}
\alias{bessel_Kn}
\alias{bessel_Kn_array}
\alias{bessel_K0_scaled}
\alias{bessel_K1_scaled}
\alias{bessel_Kn_scaled}
\alias{bessel_Kn_scaled_array}
\alias{bessel_j0}
\alias{bessel_j1}
\alias{bessel_j2}
\alias{bessel_jl}
\alias{bessel_jl_array}
\alias{bessel_jl_steed_array}
\alias{bessel_y0}
\alias{bessel_y1}
\alias{bessel_y2}
\alias{bessel_yl}
\alias{bessel_yl_array}
\alias{bessel_i0_scaled}
\alias{bessel_i1_scaled}
\alias{bessel_i2_scaled}
\alias{bessel_il_scaled}
\alias{bessel_il_scaled_array}
\alias{bessel_k0_scaled}
\alias{bessel_k1_scaled}
\alias{bessel_k2_scaled}
\alias{bessel_kl_scaled}
\alias{bessel_kl_scaled_array}
\alias{bessel_Jnu}
\alias{bessel_sequence_Jnu}
\alias{bessel_Ynu}
\alias{bessel_Inu}
\alias{bessel_Inu_scaled}
\alias{bessel_Knu}
\alias{bessel_lnKnu}
\alias{bessel_Knu_scaled}
\alias{bessel_zero_J0}
\alias{bessel_zero_J1}
\alias{bessel_zero_Jnu}
\title{Bessel functions}
\description{
Bessel functons as per the Gnu Scientific Library
}
\usage{
bessel_J0(x, give=FALSE, strict=TRUE)
bessel_J1(x, give=FALSE, strict=TRUE)
bessel_Jn(n,x, give=FALSE, strict=TRUE)
bessel_Jn_array(nmin,nmax,x, give=FALSE, strict=TRUE)
bessel_Y0(x, give=FALSE, strict=TRUE)
bessel_Y1(x, give=FALSE, strict=TRUE)
bessel_Yn(n,x, give=FALSE, strict=TRUE)
bessel_Yn_array(nmin, nmax, x, give=FALSE, strict=TRUE)
bessel_I0(x, give=FALSE, strict=TRUE)
bessel_I1(x, give=FALSE, strict=TRUE)
bessel_In(n, x, give=FALSE, strict=TRUE)
bessel_In_array(nmin, nmax, x, give=FALSE, strict=TRUE)
bessel_I0_scaled(x, give=FALSE, strict=TRUE)
bessel_I1_scaled(x, give=FALSE, strict=TRUE)
bessel_In_scaled(n, x, give=FALSE, strict=TRUE)
bessel_In_scaled_array(nmin, nmax, x, give=FALSE, strict=TRUE)
bessel_K0(x, give=FALSE, strict=TRUE)
bessel_K1(x, give=FALSE, strict=TRUE)
bessel_Kn(n, x, give=FALSE, strict=TRUE)
bessel_Kn_array(nmin, nmax, x, give=FALSE, strict=TRUE)
bessel_K0_scaled(x, give=FALSE, strict=TRUE)
bessel_K1_scaled(x, give=FALSE, strict=TRUE)
bessel_Kn_scaled(n, x, give=FALSE, strict=TRUE)
bessel_Kn_scaled_array(nmin, nmax, x, give=FALSE, strict=TRUE)
bessel_j0(x, give=FALSE, strict=TRUE)
bessel_j1(x, give=FALSE, strict=TRUE)
bessel_j2(x, give=FALSE, strict=TRUE)
bessel_jl(l,x, give=FALSE, strict=TRUE)
bessel_jl_array(lmax,x, give=FALSE, strict=TRUE)
bessel_jl_steed_array(lmax, x, give=FALSE, strict=TRUE)
bessel_y0(x, give=FALSE, strict=TRUE)
bessel_y1(x, give=FALSE, strict=TRUE)
bessel_y2(x, give=FALSE, strict=TRUE)
bessel_yl(l, x, give=FALSE, strict=TRUE)
bessel_yl_array(lmax, x, give=FALSE, strict=TRUE)
bessel_i0_scaled(x, give=FALSE, strict=TRUE)
bessel_i1_scaled(x, give=FALSE, strict=TRUE)
bessel_i2_scaled(x, give=FALSE, strict=TRUE)
bessel_il_scaled(l, x, give=FALSE, strict=TRUE)
bessel_il_scaled_array(lmax, x, give=FALSE, strict=TRUE)
bessel_k0_scaled(x, give=FALSE, strict=TRUE)
bessel_k1_scaled(x, give=FALSE, strict=TRUE)
bessel_k2_scaled(x, give=FALSE, strict=TRUE)
bessel_kl_scaled(l,x, give=FALSE, strict=TRUE)
bessel_kl_scaled_array(lmax,x, give=FALSE, strict=TRUE)
bessel_Jnu(nu, x, give=FALSE, strict=TRUE)
bessel_sequence_Jnu(nu, v, mode=0, give=FALSE, strict=TRUE)
bessel_Ynu(nu, x, give=FALSE, strict=TRUE)
bessel_Inu(nu, x, give=FALSE, strict=TRUE)
bessel_Inu_scaled(nu, x, give=FALSE, strict=TRUE)
bessel_Knu(nu, x, give=FALSE, strict=TRUE)
bessel_lnKnu(nu, x, give=FALSE, strict=TRUE)
bessel_Knu_scaled(nu, x, give=FALSE, strict=TRUE)
bessel_zero_J0(s, give=FALSE, strict=TRUE)
bessel_zero_J1(s, give=FALSE, strict=TRUE)
bessel_zero_Jnu(nu, s, give=FALSE, strict=TRUE)
}
\arguments{
  \item{x}{input: real valued}
  \item{v}{input: real valued}
  \item{n}{input: integer valued}
  \item{nmin}{input: integer valued}
  \item{nmax}{input: integer valued}
  \item{lmax}{input: integer valued}
  \item{l}{input: integer valued}
  \item{s}{input: integer valued}
  \item{nu}{input: real valued}
  \item{mode}{calc mode}
  \item{give}{give error}
  \item{strict}{strict or not}
}
\details{All as for the GSL library, except for
  \code{process.bessel.args()}, which ensures that the arguments
  \code{n} and \code{x} are suitable for passing to the relevant
  \code{C} routine.
}
\author{Robin K. S. Hankin}
\examples{

x <- seq(from=0,to=15,len=100)
plot(x,bessel_J0(x),xlim=c(0,15),ylim=c(-0.8,1),type="l",xaxt="n",yaxt="n",bty="n",xlab="",ylab="",main="Figure 9.1, p359")
axis(1,pos=0)
axis(2,pos=0)
lines(x,bessel_Y0(x))
lines(x,bessel_J1(x),lty=2)
lines(x,bessel_Y1(x),lty=2)

x <- seq(from=0,to=13,len=100)
 matplot(x,t(bessel_jl_array(3,x)),col="black",type="l",xaxt="n",yaxt="n",bty="n",xlab="",ylab="",main="Figure 10.1, p438")
axis(1,pos=0)
axis(2,pos=0)


x <- seq(from=0 ,to=5,by=0.1)
cbind(x, bessel_J0(x),bessel_J1(x),bessel_Jn(2,x))      #table 9.1, p390
cbind(x, bessel_Y0(x),bessel_Y1(x),bessel_Yn(2,x))      #table 9.2, p391
t(bessel_Jn_array(3,9,x*2))                             #table 9.2, p398

cbind(                       x,                         #table 9.8, p416
        exp(-x)*bessel_I0  (x),
        exp(-x)*bessel_I1  (x),
         x^(-2)*bessel_In(2,x)
)

cbind(                      x,                          #table 9.8, p417
        exp(x)*bessel_K0  (x),
        exp(x)*bessel_K1  (x),
         x^(2)*bessel_Kn(2,x)
)

cbind(x,                                                #table 10.1 , p457
    bessel_j0(x),
    bessel_j1(x),
    bessel_j2(x),
    bessel_y0(x),
    bessel_y1(x),
    bessel_y2(x)
)


bessel_Jnu(0.5,1.123)
bessel_Ynu(0.5,1.123)
bessel_Inu(0.5,1.123)
bessel_Inu_scaled(0.5,1.123)

bessel_Knu(0.5,1.123)
bessel_lnKnu(0.5,1.123)
bessel_Knu_scaled(0.5,1.123)

x <- 1:20
cbind(x,bessel_zero_J0(x), bessel_zero_J1(x), bessel_zero_Jnu(n=3,x))
#Table 9.5, p409, odd columns




\dontrun{
bessel_sequence_Jnu(nu=0.5,v=c(0.4,0.44,0.55))
}
}
\keyword{array}
