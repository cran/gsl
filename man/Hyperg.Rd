\name{Hyperg}
\alias{Hyperg}
\alias{hyperg_0F1}
\alias{hyperg_1F1_int}
\alias{hyperg_1F1}
\alias{hyperg_U_int}
\alias{hyperg_U}
\alias{hyperg_2F1}
\alias{hyperg_2F1_conj}
\alias{hyperg_2F1_renorm}
\alias{hyperg_2F1_conj_renorm}
\alias{hyperg_2F0}
\title{Hypergeometric functions}
\description{
Hypergeometric functons as per the Gnu Scientific Library ref manual
section 7.21 and Abramowitz and Stegun sections 13 and 17
}
\usage{
hyperg_0F1(c, x, give=FALSE, strict=TRUE)
hyperg_1F1_int(m, n, x, give=FALSE, strict=TRUE)
hyperg_1F1(a, b, x, give=FALSE, strict=TRUE)
hyperg_U_int(m, n, x, give=FALSE, strict=TRUE)
hyperg_U(a, b, x, give=FALSE, strict=TRUE)
hyperg_2F1(a, b, c, x, give=FALSE, strict=TRUE)
hyperg_2F1_conj(aR, aI, c, x, give=FALSE, strict=TRUE)
hyperg_2F1_renorm(a, b, c, x, give=FALSE, strict=TRUE)
hyperg_2F1_conj_renorm(aR, aI, c, x, give=FALSE, strict=TRUE)
hyperg_2F0(a, b, x, give=FALSE, strict=TRUE)
}
\arguments{
  \item{x}{input: real values}
  \item{c}{input: real values}
  \item{m}{input: integer values}
  \item{n}{input: integer values}
  \item{a}{input: real values}
  \item{b}{input: real values}
  \item{aR}{input: real values}
  \item{aI}{input: real values}
  \item{give}{Boolean with \code{TRUE} meaning to return a list of three
    items: the value, an estimate of the error, and a status number.}
  \item{strict}{Boolean, with \code{TRUE} meaning to return \code{NaN}
    if status is an error.}
}
\references{\url{http://www.gnu.org/software/gsl}}
\author{Robin K. S. Hankin}
\examples{

hyperg_0F1(0.1,0.55)

hyperg_1F1_int(2,3,0.555)
hyperg_1F1(2.12312,3.12313,0.555)
hyperg_U_int(2, 3, 0.555)
hyperg_U(2.234, 3.234, 0.555)
}

\keyword{array}
