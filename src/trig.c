#include <gsl/gsl_sf_trig.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name.  Thus gsl_sf_sin_e  goes to sin_e. */


void sin_e(double *x, int *nx, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nx ; i++){
    status[i]  = gsl_sf_sin_e(x[i] , &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void cos_e(double *x, int *nx, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nx ; i++){
    status[i]  = gsl_sf_cos_e(x[i] , &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  
