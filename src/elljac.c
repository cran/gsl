#include <gsl/gsl_sf_elljac.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name.  Thus gsl_sf_dawson  goes to dawson. */


void elljac_e(double *u, double *m, int *nu, double *sn, double *cn, double *dn, int *status)
{
  int i;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nu ; i++){
    status[i]  = gsl_sf_elljac_e(u[i] , m[i] , sn+i , cn+i , dn+i);
  }
}  
