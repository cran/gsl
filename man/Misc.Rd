\name{Misc}
\alias{Misc}
\alias{process.args}
\alias{strictify}
\title{Argument processing and general info}
\description{
Massages arguments into a form suitable for passing to C functions; also
general package info.
}
\usage{
process.args(...)
strictify(val,status)
}
\arguments{
  \item{...}{Argument list to be coerced to the same length.}
  \item{val}{Value component of \code{&result}}
  \item{status}{status integer}
}
\details{
  Function \code{process.args()} et seq is called, if possible, to
  massage the arguments into a form suitable for passing to \code{.C()}.
  For example, in function \code{hyperg_0F1(c,x)}, one wants
  \code{hyperg_0F1(0.1, c(0.3,0.4))} and \code{hyperg_0F1(c(0.1,0.2),
  0.3)} and \code{hyperg_0F1(c(0.1,0.2),c(0.3,0.4)} to behave sensibly.
  Function \code{process.arg()} does this by returning a list whose
  elements are vectors of the same length.  Most of the special
  functions use \code{process.args()} to ensure that the returned value
  takes the attributes of the input argument with most elements where
  possible.
  
  Function \code{strictify()} uses the \code{status} value returned by
  the \dQuote{error} form of the GSL special functions to make values
  returned with a nonzero \code{error} a \code{NaN}.  In most of the
  special functions, \code{strictify()} is called if argument
  \code{strict} takes its default value of
  \code{TRUE}.  Setting it to \code{FALSE} sometimes returns a numerical
  value as per the GSL reference manual.

  In most of the special functions, if argument \code{give} takes its
  default value of \code{FALSE}, only a numerical value is returned.
  If \code{TRUE}, error information and the status (see preceding
  paragraph) is also returned.

  The function naming scheme directly copies the GSL manual except that
  leading \code{gsl_sf_} and, if present, the trailing \code{_e} is
  stripped: thus \code{gsl_sf_Airy_Ai_e} goes to \R\ function
  \code{airy_Ai()}; however, some functions retain the prefix to avoid
  conflicts (viz \code{gsl_sf_sin()}, \code{gsl_sf_cos()},
  \code{gsl_sf_gamma()}, \code{gsl_sf_ choose()}, \code{gsl_sf_beta()}).

  \R\ function arguments have the same names as in the GSL reference 
  manual, except for the quasirandom functions documented in the
  \code{Qrng} manpage.

  The package is organized into units corresponding to GSL header files;
  the \code{.c}, \code{.R}, and \code{.Rd} filenames match the GSL header
  filenames, except that the \code{.Rd} files are capitalized.  Functions
  appear in all files in the same order as the GSL reference manual, which
  precludes the use of the tidying method given in section 3.1 of R-exts.
  Error forms of GSL functions (\code{_e} versions) are used if available.

  In general, documentation is limited to: (a), a pointer to the GSL
  reference book, which would in any case dominate any docs here; and (b),
  re-productions of some tables and figures in Abramowitz and Stegun
  (June 1964).
}
\references{\url{http://www.gnu.org/software/gsl}}
\author{Robin K. S. Hankin}
\keyword{array}
