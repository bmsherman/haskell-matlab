/*
 * MATLAB Compiler: 4.18.1 (R2013a)
 * Date: Mon Aug  4 14:07:22 2014
 * Arguments: "-B" "macro_default" "-W" "lib:libhsmatlab" "-T" "link:lib"
 * "libhsmatlab.c" 
 */

#ifndef __libhsmatlab_h
#define __libhsmatlab_h 1

#if defined(__cplusplus) && !defined(mclmcrrt_h) && defined(__linux__)
#  pragma implementation "mclmcrrt.h"
#endif
#include "mclmcrrt.h"
#ifdef __cplusplus
extern "C" {
#endif

#if defined(__SUNPRO_CC)
/* Solaris shared libraries use __global, rather than mapfiles
 * to define the API exported from a shared library. __global is
 * only necessary when building the library -- files including
 * this header file to use the library do not need the __global
 * declaration; hence the EXPORTING_<library> logic.
 */

#ifdef EXPORTING_libhsmatlab
#define PUBLIC_libhsmatlab_C_API __global
#else
#define PUBLIC_libhsmatlab_C_API /* No import statement needed. */
#endif

#define LIB_libhsmatlab_C_API PUBLIC_libhsmatlab_C_API

#elif defined(_HPUX_SOURCE)

#ifdef EXPORTING_libhsmatlab
#define PUBLIC_libhsmatlab_C_API __declspec(dllexport)
#else
#define PUBLIC_libhsmatlab_C_API __declspec(dllimport)
#endif

#define LIB_libhsmatlab_C_API PUBLIC_libhsmatlab_C_API


#else

#define LIB_libhsmatlab_C_API

#endif

/* This symbol is defined in shared libraries. Define it here
 * (to nothing) in case this isn't a shared library. 
 */
#ifndef LIB_libhsmatlab_C_API 
#define LIB_libhsmatlab_C_API /* No special import/export declaration */
#endif

extern LIB_libhsmatlab_C_API 
bool MW_CALL_CONV libhsmatlabInitializeWithHandlers(
       mclOutputHandlerFcn error_handler, 
       mclOutputHandlerFcn print_handler);

extern LIB_libhsmatlab_C_API 
bool MW_CALL_CONV libhsmatlabInitialize(void);

extern LIB_libhsmatlab_C_API 
void MW_CALL_CONV libhsmatlabTerminate(void);



extern LIB_libhsmatlab_C_API 
void MW_CALL_CONV libhsmatlabPrintStackTrace(void);



#ifdef __cplusplus
}
#endif
#endif
