#include <gsl/gsl_sf_laguerre.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name and the _e from the end.  Thus gsl_sf_laguerre_1_e goes
   to laguerre_1. */

void laguerre_1(double *a, double *x, int *nx, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();  

  for(i = 0; i< *nx ; i++){
    status[i]  = gsl_sf_laguerre_1_e(a[i], x[i], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void laguerre_2(double *a, double *x, int *nx, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();  

  for(i = 0; i< *nx ; i++){
    status[i]  = gsl_sf_laguerre_2_e(a[i], x[i], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void laguerre_3(double *a, double *x, int *nx, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();  

  for(i = 0; i< *nx ; i++){
    status[i]  = gsl_sf_laguerre_3_e(a[i], x[i], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  



void laguerre_n(int *n, double *a, double *x, int *nx, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();  

  for(i = 0; i< *nx ; i++){
    status[i]  = gsl_sf_laguerre_n_e(*n, a[i], x[i], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  
