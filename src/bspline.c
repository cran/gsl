#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_bspline.h>
#include <R.h>

/* Code to replicate bs() in splines package. Note that feeding in
   x_min and x_max is necessary if you want to replicate predict.bs()
   - use x_min/x_max for your training data and let x be the
   evaluation data. */

int gsl_bspline(double *x,
                int *n,
                int *degree,
                int *ncoeffs,
                double *x_min,
                double *x_max,
                double *Bx)
{

  int k = *degree + 1; /* k in gsl */
  const size_t nbreak = *ncoeffs + 2 - k;
  int i, j;

  gsl_bspline_workspace *bw = gsl_bspline_alloc(k, nbreak);
  gsl_vector *B = gsl_vector_alloc(*ncoeffs);
  
  gsl_bspline_knots_uniform(*x_min, *x_max, bw);

  for (i = 0; i < *n; ++i)
    {

      /* compute B_j(xi) for all j */
      gsl_bspline_eval(x[i], B, bw);
      
      /* fill in row i of Bx */
      for (j = 0; j < *ncoeffs; ++j)
        {
          double Bj = gsl_vector_get(B, j);
          Bx[i*(*ncoeffs)+j] = Bj;
        }
    }
  
  gsl_bspline_free(bw);
  gsl_vector_free(B);

  return(0);

} /* main() */

/* Provide missing functionality derivative bs() function in package
   splines */

int gsl_bspline_deriv(double *x,
                      int *n,
                      int *degree,
                      int *ncoeffs,
                      int *order,
                      double *x_min,
                      double *x_max,
                      double *Bx)
{

  int k = *degree + 1; /* k in gsl */
  const size_t nbreak = *ncoeffs + 2 - k;
  size_t i, j;

  gsl_bspline_workspace *bw = gsl_bspline_alloc(k, nbreak);
  gsl_matrix *dB = gsl_matrix_alloc(*ncoeffs, 1+*order);
  gsl_vector *dBorder = gsl_vector_alloc(*ncoeffs);
  gsl_bspline_deriv_workspace *derivWS = gsl_bspline_deriv_alloc(*ncoeffs);
  
  gsl_bspline_knots_uniform(*x_min, *x_max, bw);

  for (i = 0; i < *n; ++i)
    {

      /* compute B_j(xi) for all j */
      gsl_bspline_deriv_eval(x[i], *order, dB, bw, derivWS);

      /* fill in row i of Bx */
      gsl_matrix_get_col(dBorder, dB, *order);
      
      for (j = 0; j < *ncoeffs; ++j)
        {
          double Bj = gsl_vector_get(dBorder, j);
          Bx[i*(*ncoeffs)+j] = Bj;
        }
    }
  
  gsl_bspline_free(bw);
  gsl_matrix_free(dB);
  gsl_vector_free(dBorder);
  gsl_bspline_deriv_free(derivWS);

  return(0);

} /* main() */
