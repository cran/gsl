#include <gsl/gsl_sf_legendre.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name.  Thus gsl_sf_legendre_P1 goes to legendre_P1. */

void legendre_P1(double *x, int *len,  double *out)
{
	int i;
	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_P1(x[i]);
	}
}
void legendre_P1_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_P1_e(x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_P2(double *x, int *len, double *out)
{
	int i;
	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_P2(x[i]);
	}
}

void legendre_P2_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_P2_e(x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_P3(double *x, int *len, double *out)
{
	int i;
	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_P3(x[i]);
	}
}

void legendre_P3_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_P3_e(x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_Pl(int *l, double *x, int *len, double *out)
{
	int i;
	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_Pl(*l, x[i]);
	}
}

void legendre_Pl_e(int *l, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_Pl_e(*l, x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_Pl_array(int *lmax, double* x, int* len, double* out, int* status)
{
	int i;
	gsl_set_error_handler_off();

	for(i=0 ; i< *len ; i++) {
		status[i] = gsl_sf_legendre_Pl_array(*lmax, x[i], out+i*(*lmax+1));
	}
}

void legendre_Q0(double *x, int *len, double *out)
{
	int i;
	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_Q0(x[i]);
	}
}

void legendre_Q0_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_Q0_e(x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_Q1(double *x, int *len,  double *out)
{
	int i;
	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_Q1(x[i]);
	}
}

void legendre_Q1_e(double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_Q1_e(x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_Ql(int *l, double *x, int *len,  double *out)
{
	int i;
	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_Ql(*l, x[i]);
	}
}

void legendre_Ql_e(int *l, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_Ql_e(*l, x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_Plm(int *l, int *m, double *x, int *len, double *out)
{
	int i;

	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_Plm(*l, *m, x[i]);
	}
}

void legendre_Plm_e(int *l, int *m, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_Plm_e(*l, *m, x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_Plm_array(int *lmax, int *m, double* x, int* len, double* out, int* status)
{
	int i;
	gsl_set_error_handler_off();

	for(i=0 ; i< *len ; i++) {
		status[i] = gsl_sf_legendre_Plm_array(*lmax, *m, x[i], out+i*(*lmax- *m +1));
	}
}

void legendre_sphPlm(int *l, int *m, double *x, int *len, double *out)
{
	int i;

	for(i = 0; i< *len ; i++){
		out[i] = gsl_sf_legendre_sphPlm(*l, *m, x[i]);
	}
}

void legendre_sphPlm_e(int *l, int *m, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
 
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_sphPlm_e(*l, *m, x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_sphPlm_array(int *lmax, int *m, double* x, int* len, double* out, int* status)
{
	int i;
	gsl_set_error_handler_off();

	for(i=0 ; i< *len ; i++) {
		status[i] = gsl_sf_legendre_Plm_array(*lmax, *m, x[i], out+i*(*lmax- *m +1));
	}
}

void conicalP_half_e(double *lambda, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_conicalP_half_e(lambda[i], x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void conicalP_mhalf_e(double *lambda, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_conicalP_mhalf_e(lambda[i], x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void conicalP_0_e(double *lambda, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_conicalP_0_e(lambda[i], x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void conicalP_1_e(double *lambda, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_conicalP_1_e(lambda[i], x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void conicalP_sph_reg_e(int *l, double *lambda, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();

	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_conicalP_sph_reg_e(l[i], lambda[i], x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void conicalP_cyl_reg_e(int *m, double *lambda, double *x, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();

	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_conicalP_sph_reg_e(m[i], lambda[i], x[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_H3d_0_e(double *lambda, double *eta, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();

	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_H3d_0_e(lambda[i], eta[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_H3d_1_e(double *lambda, double *eta, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();

	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_H3d_1_e(lambda[i], eta[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_H3d_e(int *l, double *lambda, double *eta, int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();

	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_legendre_H3d_e(l[i], lambda[i], eta[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void legendre_H3d_array(int *lmax, double *lambda, double *eta, int *len, double* out, int* status)
{
	int i;
	gsl_set_error_handler_off();

	for(i=0 ; i< *len ; i++) {
		status[i] = gsl_sf_legendre_H3d_array(*lmax, lambda[i], eta[i], out+i*(*lmax+1));
	}
}
