/*
 * MATLAB Compiler: 4.18.1 (R2013a)
 * Date: Mon Aug  4 14:07:22 2014
 * Arguments: "-B" "macro_default" "-W" "lib:libhsmatlab" "-T" "link:lib"
 * "libhsmatlab.c" 
 */

#include <stdio.h>
#define EXPORTING_libhsmatlab 1
#include "libhsmatlab.h"

static HMCRINSTANCE _mcr_inst = NULL;


#ifdef __cplusplus
extern "C" {
#endif

static int mclDefaultPrintHandler(const char *s)
{
  return mclWrite(1 /* stdout */, s, sizeof(char)*strlen(s));
}

#ifdef __cplusplus
} /* End extern "C" block */
#endif

#ifdef __cplusplus
extern "C" {
#endif

static int mclDefaultErrorHandler(const char *s)
{
  int written = 0;
  size_t len = 0;
  len = strlen(s);
  written = mclWrite(2 /* stderr */, s, sizeof(char)*len);
  if (len > 0 && s[ len-1 ] != '\n')
    written += mclWrite(2 /* stderr */, "\n", sizeof(char));
  return written;
}

#ifdef __cplusplus
} /* End extern "C" block */
#endif

/* This symbol is defined in shared libraries. Define it here
 * (to nothing) in case this isn't a shared library. 
 */
#ifndef LIB_libhsmatlab_C_API
#define LIB_libhsmatlab_C_API /* No special import/export declaration */
#endif

LIB_libhsmatlab_C_API 
bool MW_CALL_CONV libhsmatlabInitializeWithHandlers(
    mclOutputHandlerFcn error_handler,
    mclOutputHandlerFcn print_handler)
{
    int bResult = 0;
  if (_mcr_inst != NULL)
    return true;
  if (!mclmcrInitialize())
    return false;
    {
        mclCtfStream ctfStream = 
            mclGetEmbeddedCtfStream((void *)(libhsmatlabInitializeWithHandlers));
        if (ctfStream) {
            bResult = mclInitializeComponentInstanceEmbedded(   &_mcr_inst,
                                                                error_handler, 
                                                                print_handler,
                                                                ctfStream);
            mclDestroyStream(ctfStream);
        } else {
            bResult = 0;
        }
    }  
    if (!bResult)
    return false;
  return true;
}

LIB_libhsmatlab_C_API 
bool MW_CALL_CONV libhsmatlabInitialize(void)
{
  return libhsmatlabInitializeWithHandlers(mclDefaultErrorHandler, 
                                           mclDefaultPrintHandler);
}

LIB_libhsmatlab_C_API 
void MW_CALL_CONV libhsmatlabTerminate(void)
{
  if (_mcr_inst != NULL)
    mclTerminateInstance(&_mcr_inst);
}

LIB_libhsmatlab_C_API 
void MW_CALL_CONV libhsmatlabPrintStackTrace(void) 
{
  char** stackTrace;
  int stackDepth = mclGetStackTrace(&stackTrace);
  int i;
  for(i=0; i<stackDepth; i++)
  {
    mclWrite(2 /* stderr */, stackTrace[i], sizeof(char)*strlen(stackTrace[i]));
    mclWrite(2 /* stderr */, "\n", sizeof(char)*strlen("\n"));
  }
  mclFreeStackTrace(&stackTrace, stackDepth);
}


