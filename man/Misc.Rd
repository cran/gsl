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
  \code{gsl_sf_sin()}, \code{gsl_sf_cos()}, \code{gsl_sf_gamma()},
  \code{gsl_sf_ choose()}, \code{beta()}

  Functions \code{process.2.args()} et seq are called, if possible, to
  massage the arguments into a form suitable for passing to \code{.C()}.
  For example, in function \code{hyperg_0F1(c,x)}, one wants
  \code{hyperg_0F1(0.1, c(0.3,0.4))} and \code{hyperg_0F1(c(0.1,0.2),
  0.3)} and \code{hyperg_0F1(c(0.1,0.2),c(0.3,0.4)} to behave sensibly.
  The process functions do this by returning a list whose elements are
  vectors of the same length.

  Function \code{strictify()} uses the \code{status} value returned by
  the \dQuote{error} form of the GSL special functions to make values
  returned with a nonzero \code{error} a \code{NaN}.  This function is
  called if argument \code{strict} takes its default value of
  \code{TRUE}.  Setting it to \code{FALSE} sometimes returns a numerical
  value as per the GSL reference manual.

  If argument \code{give} takes its default value of \code{FALSE}, a
  numerical value only is returned.  If \code{TRUE}, error information
  and the status (see preceding paragraph) is also returned.
  
}
\author{Robin K. S. Hankin}
\keyword{array}
