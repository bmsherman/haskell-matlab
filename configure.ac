AC_INIT([Haskell Matlab interface], [0.1], [dylan@dylex.net])
AC_CONFIG_SRCDIR([matlab.cabal])
AC_ARG_WITH(compiler,
	    [AC_HELP_STRING([--with-compiler=PATH], [])],
	    [],
	    [])
AC_ARG_WITH(mcr,
	    [AC_HELP_STRING([--with-mcr=PATH], [use Matlab Component Runtime installed in PATH @<:@default=no@:>@])],
	    [],
	    [with_mcr=])
AC_ARG_WITH(matlab,
	    [AC_HELP_STRING([--with-matlab=MATLAB], [use specified matlab @<:@default=check@:>@])],
	    [],
	    [with_matlab=yes])
AC_ARG_ENABLE(runtime,
	      [AC_HELP_STRING([--disable-runtime], [enable Matlab.Runtime interface @<:@default=yes@:>@])],
	      [use_runtime=$enableval],
	      [use_runtime=yes])


AS_IF([test "$with_matlab" == yes],
      [AC_PATH_PROG([MATLAB_BIN], [matlab])],
      [test "$with_matlab" != no],
      [MATLAB_BIN="$with_matlab"])






AS_IF([test -n "$with_mcr"], [

AC_MSG_CHECKING([Matlab Component Runtime])
AS_IF([test \! -d "$with_mcr/runtime"], [
       AC_MSG_RESULT(not found)
       AC_MSG_FAILURE([Matlab Component Runtime not found at $with_mcr/runtime])])
AC_MSG_RESULT([$with_mcr])
MATLAB_DIR="$with_mcr"
MATLAB_ARCH=`ls $MATLAB_DIR/runtime 2>/dev/null`
MATLAB_LIB_PATH="$MATLAB_DIR/bin/$MATLAB_ARCH"

], [

AS_IF([test -z "$MATLAB_BIN"], [AC_MSG_ERROR([Matlab is required])])

AC_MSG_CHECKING([matlab environment])

matlab_env=./matlab.env
env -i "$MATLAB_BIN" -e > $matlab_env 2>&AS_MESSAGE_LOG_FD
matlab_envres=$?
cat $matlab_env >&AS_MESSAGE_LOG_FD

AC_DEFUN([MATLAB_GETENV], [`. $matlab_env && echo "${$1}"`])

unset MATLAB
AS_IF([test $matlab_envres -gt 0], 
      [AC_MSG_RESULT([failed])
      AC_MSG_FAILURE([$MATLAB_BIN -e failed])],
      [! (. $matlab_env > /dev/null 2>&1 && test -n "$MATLAB")],
      [AC_MSG_RESULT([invalid])
      AC_MSG_FAILURE([$MATLAB_BIN -e output invalid])],
      [AC_MSG_RESULT([MATLAB_GETENV([MATLAB])])])

MATLAB_DIR=MATLAB_GETENV([MATLAB])
MATLAB_ARCH=MATLAB_GETENV([ARCH])
MATLAB_LIB_PATH=MATLAB_GETENV([LD_LIBRARY_PATH//:/ ])

])

AC_MSG_CHECKING([matlab arch])
AS_CASE(["$MATLAB_ARCH"],
	[sol64|sola64|glnx86|glnxi64|glnxa64|mac|maci], AC_MSG_RESULT([$MATLAB_ARCH]),
	[AC_MSG_RESULT([unknown])
	MATLAB_ARCH=])
AS_IF([test -z "$MATLAB_ARCH"], AC_MSG_FAILURE([matlab architecture unknown]))

MATLAB_LIB_DIR="$MATLAB_DIR/bin/$MATLAB_ARCH"

LDFLAGS="$LDFLAGS -Wl,-rpath,$MATLAB_LIB_DIR"
save_ldflags="$LDFLAGS"
LDFLAGS="$LDFLAGS -L${MATLAB_LIB_PATH// / -L}"
AC_CHECK_LIB(mx, mxDestroyArray, , [AC_MSG_ERROR([Matlab array library not found])])
AC_CHECK_LIB(eng, engEvalString, , [AC_MSG_ERROR([Matlab engine library not found])])
LDFLAGS="$save_ldflags"


AS_IF([test "$use_runtime" == yes],
      [AC_PATH_PROG([MATLAB_MCC], [mcc], , [$MATLAB_DIR/bin:$PATH])
      AS_IF([test \! -x "$MATLAB_MCC" -a -z "$with_mcr"], [AC_MSG_ERROR([Matlab compiler not found])])])

AC_SUBST([MATLAB_DIR])
AC_SUBST([MATLAB_BIN])
AC_SUBST([MATLAB_MCC])
AC_SUBST([MATLAB_ARCH])
AC_SUBST([MCCFLAGS])
AC_SUBST([MATLAB_LIB_PATH])
AC_SUBST([MATLAB_LIB_DIR])

AC_CONFIG_FILES([matlab.buildinfo src/Makefile test/Makefile])
AC_OUTPUT

