#include <gsl/gsl_sf_trig.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name.  Thus gsl_sf_sin_e  goes to sin_e. */


void sin_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_sin_e(x[i] , &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void cos_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_cos_e(x[i] , &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void hypot_e(double *x, double *y, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_hypot_e(x[i], y[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void sinc_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_sinc_e(x[i] , &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}

void complex_sin_e(double *zr, double *zi, int *len, double *val_lnr, double *val_arg, double *err_lnr, double *err_arg, int *status)
{
	int i;
	gsl_sf_result lnr;
	gsl_sf_result arg;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_complex_sin_e(zr[i], zi[i], &lnr, &arg) ;

		val_lnr[i] = lnr.val;
		val_arg[i] = arg.val;

		err_lnr[i] = lnr.err;
		err_arg[i] = arg.err;
	}
}


void complex_cos_e(double *zr, double *zi, int *len, double *val_lnr, double *val_arg, double *err_lnr, double *err_arg, int *status)
{
	int i;
	gsl_sf_result lnr;
	gsl_sf_result arg;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_complex_cos_e(zr[i], zi[i], &lnr, &arg) ;

		val_lnr[i] = lnr.val;
		val_arg[i] = arg.val;

		err_lnr[i] = lnr.err;
		err_arg[i] = arg.err;
	}
}

void complex_logsin_e(double *zr, double *zi, int *len, double *val_lnr, double *val_arg, double *err_lnr, double *err_arg, int *status)
{
	int i;
	gsl_sf_result lnr;
	gsl_sf_result arg;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_complex_logsin_e(zr[i], zi[i], &lnr, &arg) ;

		val_lnr[i] = lnr.val;
		val_arg[i] = arg.val;

		err_lnr[i] = lnr.err;
		err_arg[i] = arg.err;
	}
}

void lnsinh_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_lnsinh_e(x[i] , &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
} 

void lncosh_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_lncosh_e(x[i] , &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
} 
