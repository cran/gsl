




AC_INIT([src/airy.c])

AC_CHECK_LIB(gsl, gsl_sf_airy_Ai_e, , 
             [AC_MSG_ERROR([Cannot find Gnu Scientific Library.])],
             [-lgslcblas])
AC_CHECK_HEADER([gsl/gsl_sf_airy.h], ,
             [AC_MSG_ERROR([Cannot find gsl headers.])])
AC_OUTPUT()


