\name{Misc}
\alias{Misc}
\alias{misc}
\alias{process.args}
\alias{strictify}
\title{Argument processing and general info}
\description{
Various widely used functions in the package
}
\usage{
process.args(...)
strictify(val,status)
}
\arguments{
  \item{...}{Argument list to be coerced to the same length}
  \item{val}{Value component of \code{&result}}
  \item{status}{status integer}
}
\details{
  Function \code{process.args()} is an internal function used to
  massage the arguments into a form suitable for passing to \code{.C()}.
  For example, in function \code{hyperg_0F1(c,x)}, one wants each of 
  \code{hyperg_0F1(0.1, c(0.3,0.4))} and \code{hyperg_0F1(c(0.1,0.2),
  0.3)} and \code{hyperg_0F1(c(0.1,0.2),c(0.3,0.4)} to behave sensibly.

  Function \code{process.arg()} is used widely in the package, taking an
  arbitrary number of arguments and returning a list whose elements are
  vectors of the same length.  Most of the special functions use
  \code{process.args()} to ensure that the returned value takes the
  attributes of the input argument with most elements where possible.
  
  Function \code{strictify()} uses the \code{status} value returned by
  the \dQuote{error} form of the GSL special functions to make values
  returned with a nonzero \code{error} a \code{NaN}.  In most of the
  special functions, \code{strictify()} is called if argument
  \code{strict} takes its default value of \code{TRUE}.  Setting it to
  \code{FALSE} sometimes returns a numerical value as per the GSL
  reference manual.

  In most of the special functions, if argument \code{give} takes its
  default value of \code{FALSE}, only a numerical value is returned.
  If \code{TRUE}, error information and the status (see preceding
  paragraph) is also returned.
}
\references{\url{http://www.gnu.org/software/gsl}}
\author{Robin K. S. Hankin}
\keyword{array}
