#include <gsl/gsl_sf_ellint.h>
#include <gsl/gsl_errno.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name.  Thus gsl_sf_airy_Ai goes to airy_Ai. See section 7.13
   of the GSL manual for documentation */

const  gsl_mode_t sf_mode[] = { GSL_PREC_DOUBLE, GSL_PREC_SINGLE, GSL_PREC_APPROX } ;

void ellint_Kcomp_e(const double *k, const int *len, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *len ; i++){
    status[i] = gsl_sf_ellint_Kcomp_e(k[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void ellint_Ecomp_e(const double *k, const int *nk, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nk ; i++){
    status[i] = gsl_sf_ellint_Ecomp_e(k[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void ellint_F_e(double *phi, const double *k, const int *nk, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nk ; i++){
    status[i] = gsl_sf_ellint_F_e(phi[i], k[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void ellint_E_e(double *phi, const double *k, const int *nk, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nk ; i++){
    status[i] = gsl_sf_ellint_E_e(phi[i], k[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void ellint_P_e(double *phi, const double *k, const double *n, const int *nk, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nk ; i++){
    status[i] = gsl_sf_ellint_P_e(phi[i], k[i], n[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void ellint_D_e(const double *phi, const double *k, const int *nk, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nk ; i++){
    status[i] = gsl_sf_ellint_D_e(phi[i], k[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}
  
void ellint_RC_e(const double *x, const double *y, const int *nx, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nx ; i++){
    status[i] = gsl_sf_ellint_RC_e(x[i], y[i],  sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
} 
 
void ellint_RD_e(const double *x, const double *y, double *z, int *nx, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nx ; i++){
    status[i] = gsl_sf_ellint_RD_e(x[i], y[i], z[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  

void ellint_RF_e(const double *x, const double *y, const double *z, const int *nx, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nx ; i++){
    status[i] = gsl_sf_ellint_RF_e(x[i], y[i], z[i], sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
} 
 
void ellint_RJ_e(const double *x, const double *y, const double *z, const double *p, const int *nx, const int *mode, double *val, double *err, int *status)
{
  int i;
  gsl_sf_result result;
  gsl_set_error_handler_off();
  
  for(i = 0; i< *nx ; i++){
    status[i] = gsl_sf_ellint_RJ_e(x[i], y[i], z[i], *p, sf_mode[*mode], &result) ;
    val[i] = result.val;
    err[i] = result.err;
  }
}  
