\name{Misc}
\alias{Misc}
\alias{process.2.args}
\alias{process.3.args}
\alias{process.4.args}
\alias{strictify}
\title{Argument processing and general info}
\description{
Massages arguments into a form suitable for passing to C functions; also
general info
}
\usage{
process.2.args(a1,a2)
process.3.args(a1,a2,a3)
process.4.args(a1,a2,a3,a4)
strictify(val,status)
}
\arguments{
  \item{a1}{First argument}
  \item{a2}{Second argument}
  \item{a3}{Third argument}
  \item{a4}{Fourth argument}
  \item{val}{Value component of \code{&result}}
  \item{status}{status integer}
}
\details{
  Functions that retain the \dQuote{\code{gsl_sf_}} prefix:
  sin, cos, gamma, choose, beta

  }
\author{Robin K. S. Hankin}
\keyword{array}
