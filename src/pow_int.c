#include <gsl/gsl_sf_pow_int.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name, and stripping the _e.  Thus gsl_sf_laguerre_1_e goes
   to laguerre_1. */

void pow_int(double *x, int *n, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();  

	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_pow_int_e(x[i], n[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  
