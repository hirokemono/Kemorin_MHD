
#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# AC_PREREQ([2.72])
AC_INIT([FULL-PACKAGE-NAME],[VERSION],[BUG-REPORT-ADDRESS])

AM_INIT_AUTOMAKE
AM_CONDITIONAL([am__fastdepOBJC], false)
AM_CONDITIONAL([am__fastdepCXX], false)
AC_CANONICAL_HOST
AM_PROG_AS
AC_SUBST(CCAS)

dnl Include OpenMP related definitions
builtin(include,MHD/confdb/ax_pthread.m4)
builtin(include,MHD/confdb/ax_openmp.m4)

dnl Include MPI related definitions
builtin(include,MHD/confdb/ax_mpi.m4)

dnl Include gmake Check related definitions
builtin(include,MHD/confdb/check_gnu_make.m4)

dnl Include BLAS library check
builtin(include,MHD/confdb/ax_blas.m4)
builtin(include,MHD/confdb/ax_blas_f77_func.m4)

dnl Include x86 features Check
builtin(include,MHD/confdb/ax_gcc_x86_cpu_supports.m4)
builtin(include,MHD/confdb/ax_check_x86_features.m4)


#
dnl Include zlib Check related definitions
builtin(include,MHD/confdb/ax_check_zlib.m4)
dnl Include pgplot Check related definitions
builtin(include,MHD/confdb/ax_check_pgplot.m4)
#
dnl Include hdf5 Check
builtin(include,MHD/confdb/ax_lib_hdf5.m4)
#
dnl Include Aquaterm Check
builtin(include,MHD/confdb/ax_check_aquaterm.m4)
#
dnl Include pkg-config Check
builtin(include,MHD/confdb/pkg.m4)
#

AC_CONFIG_SRCDIR([MHD/programs/SPH_MHD/main_sph_MHD.f90])
# AC_CONFIG_HEADERS([config.h])

AC_ARG_ENABLE(cocoa,
	[  --enable-cocoa          Use Cocoa framework],[USE_Cocoa='yes'],[USE_Cocoa='no'])

AC_ARG_ENABLE(gl,
	[  --enable-gl             Use OpenGL for viewer],[USE_gl='yes'],[USE_gl='no'])


AC_ARG_WITH(fftw3,
    [  --with-fftw3            Use fftw3 library],[USE_fftw3='yes'],[USE_fftw3='no'])

AC_ARG_WITH(libpng,
	[  --with-libpng           Use libpng library],[USE_libpng='yes'],[USE_libpng='no'])

AC_ARG_WITH(gtk,
	[  --with-gtk              Use gtk+ GUI framework],[USE_gtk='yes'],[USE_gtk='no'])

AC_ARG_WITH(glfw,
	[  --with-glfw             Use GLFW GUI framework],[USE_glfw='yes'],[USE_glfw='no'])


AC_MSG_RESULT(enable-cocoa... "${USE_Cocoa}")
AC_MSG_RESULT(enable-OpenGL.. "${USE_gl}")

AC_MSG_RESULT(with-fftw3...   "${USE_fftw3}")
AC_MSG_RESULT(enable-libpng..."${USE_libpng}")
AC_MSG_RESULT(enable-gtk...   "${USE_gtk}")
AC_MSG_RESULT(enable-glfw...  "${USE_glfw}")

AC_MSG_RESULT(with-blas...     "${with_blas}")
AC_MSG_RESULT(with-zlib...     "${with_zlib}")
AC_MSG_RESULT(with-aquaterm... "${with_aquaterm}")
AC_MSG_RESULT(with-pgplot...   "${with_pgplot}")

# Checks for programs.
AC_PROG_CC
AC_PROG_FC

if test -z ${FC}
then
	FC=${F77}
else
	F77=${FC}
fi

SSE_FLAG=
SSE_CPU="cpufort"
SSE4="fort"
SSE8="fort512"
AC_MSG_RESULT(CPU Check...."${host}")
case $host in
  x86_64-*-*)
    AC_MSG_RESULT(Build for x86_64)
# Check x86 features
# AC_MSG_RESULT(CFLAGS in:    "${CFLAGS}")
# AX_CHECK_X86_FEATURES
# AC_MSG_RESULT(CFLAGS out:    "${CFLAGS}")
    AX_GCC_X86_CPU_SUPPORTS(avx512f, have_avx512="yes")

    AX_GCC_X86_CPU_SUPPORTS(fma4, have_fma="yes")
    AX_GCC_X86_CPU_SUPPORTS(fma, have_fma="yes")

    AX_GCC_X86_CPU_SUPPORTS(avx2, have_avx="yes")
    AX_GCC_X86_CPU_SUPPORTS(avx, have_avx="yes")

    AX_GCC_X86_CPU_SUPPORTS(sse3, have_sse3="yes")

    if test -n "${have_avx}"; then
#      AC_MSG_RESULT(AVX INSTRUCTIONS)
      SSE_FLAG="avx"
      SSE_CPU="cpuavx"
      SSE4="avx"
      SSE8="fort512"
    fi
    if test -n "${have_fma}"; then
#      AC_MSG_RESULT(FMA INSTRUCTIONS)
      SSE_FLAG="fma"
      SSE_CPU="cpufma"
      SSE4="fma"
      SSE8="fort512"
    fi
    if test -n "${have_avx512}"; then
      AC_MSG_RESULT(AVX512 INSTRUCTIONS)
      SSE_FLAG="avx512f"
      SSE_CPU="cpuavx512"
      SSE4="fma"
      SSE8="avx512"
    fi
    ;;
  arm-*-*)
    AC_MSG_RESULT(Build for arm64)
    ;;
  powerpc64-*-*)
    AC_MSG_RESULT(Build for Powerpc64)
    ;;
  *-*-*)
    AC_MSG_RESULT(Build for something else)
    ;;
esac
AC_SUBST(SSE_FLAG)
AC_SUBST(SSE_CPU)
AC_SUBST(SSE4)
AC_SUBST(SSE8)

if test ${host} = "powerpc64-bgq-linux"
then
  XL_FORTRAN="__bgq__"
fi
AC_SUBST(XL_FORTRAN)



if test -z ${AR}
then
	ar_bin="ar"
else
	ar_bin="${AR}"
fi
AC_SUBST(ar_bin)

AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_MSG_RESULT(RANLIB... "${RANLIB}")

#GNU make check
CHECK_GNU_MAKE()
GMAKE=$_cv_gnu_make_command
AC_SUBST(GMAKE)
AC_MSG_RESULT(GNU MAKE... "${GMAKE}")

# libtool check
PKG_PROG_PKG_CONFIG

# Checks using C compiler
AC_LANG_SAVE
AC_LANG(C)
# Check OpenMP
AX_OPENMP
AC_SUBST([OPENMP_CFLAGS])
# Check MPI
AX_MPI
AC_LANG_RESTORE
# Checks using Fortran Compiler
AC_LANG_SAVE
AC_LANG(Fortran)
# Check OpenMP
AX_OPENMP
AC_SUBST(OPENMP_FCFLAGS)
# Check MPI
AX_MPI
AC_LANG_RESTORE
AC_SUBST(MPIDIR)
AC_SUBST(MPILIBS)
AC_SUBST(MPIINCDIR)

AC_ARG_VAR(MPIRUN, Command and options to invoke tests using MPI (Default: mpirun))

if test -n "${MPIRUN}"; then
    AC_MSG_RESULT(Defined MPIRUN..."${MPIRUN}")
else
	AC_MSG_RESULT(MPIRUN is not defined)
	MPIRUN="mpirun"
    AC_SUBST(MPIRUN)
fi

# AC_MSG_RESULT(MPIDIR..."${MPIDIR}")
# AC_MSG_RESULT(MPIINCDIR..."${MPIINCDIR}")
# AC_MSG_RESULT(MPILIBS..."${MPILIBS}")
AC_MSG_RESULT(MPIRUN..."${MPIRUN}")

# hdf5 check
AX_LIB_HDF5(parallel)
AC_MSG_RESULT(HDF5_LDFLAGS... "${HDF5_LDFLAGS}")
AC_MSG_RESULT(HDF5_PFLIBS... "${HDF5_PFLIBS}")

# Check BLAS
if test -n "${with_blas}"; then
    AX_BLAS
#   AX_BLAS_F77_FUNC
fi
AC_SUBST(BLAS_LIBS)

# Checks for header files.
m4_warn([obsolete],
[The preprocessor macro `STDC_HEADERS' is obsolete.
  Except in unusual embedded environments, you can safely include all
  ISO C90 headers unconditionally.])dnl
# Autoupdate added the next two lines to ensure that your configure
# script's behavior did not change.  They are probably safe to remove.
AC_CHECK_INCLUDES_DEFAULT
AC_PROG_EGREP

AC_CHECK_HEADERS([stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
	AC_HEADER_STDBOOL
	AC_C_CONST

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([floor memset sqrt strrchr strtol])

# FFTW3 check
if test ${USE_fftw3} = "yes"
then
	PKG_CHECK_MODULES([FFTW3], fftw3, have_fftw3=yes, AC_MSG_ERROR(Cannot find FFTW 3.x))
	echo "have_fftw3=$have_fftw3"
fi
if test ${have_fftw3} = "yes"
then
    PKGFLAGS_FFTW3="fftw3"
    AC_SUBST(PKGFLAGS_FFTW3)
    AC_SUBST(FFTW3_CFLAGS)
    AC_SUBST(FFTW3_LIBS)

    if test -n "${OPENMP_CFLAGS}"; then
		CFLAGS_TMP="${CFLAGS}"
		LDLAGS_TMP="${LDFLAGS}"
		CFLAGS="${OPENMP_CFLAGS} ${CFLAGS}"
		LDFLAGS="${FFTW3_LIBS}"
		AC_MSG_RESULT(CFLAGS_TMP... "${CFLAGS_TMP}")
		AC_MSG_RESULT(CFLAGS... "${CFLAGS}")
		AC_MSG_RESULT(LDFLAGS... "${LDFLAGS}")
		
		AC_SUBST(CFLAGS)
		AC_SUBST(LDFLAGS)
		AC_CHECK_LIB(fftw3_omp, fftw_init_threads, have_fftw3_omp=yes, AC_MSG_RESULT(FFTW 3.x does not support OopenMP))
		LDFLAGS="${LDLAGS_TMP}"
		CFLAGS_TMP="${CFLAGS_TMP}"
		AC_SUBST(CFLAGS)
		AC_SUBST(LDFLAGS)
    fi
fi
if test ${have_fftw3_omp} = "yes"
then
	FFTW3_OMP_LIBS="${FFTW3_LIBS}_omp"
	AC_SUBST(FFTW3_OMP_LIBS)
fi

# Check zlib
if test ${with_zlib} != "no"
then
#	PKG_CHECK_MODULES([ZLIB], zlib, have_zlib=yes, AC_MSG_ERROR(Cannot find zlib))
	echo "have_zlib=$have_zlib"
	if test ${have_zlib} = "yes"
	then
		PKGFLAGS_ZLIB="z"
	else
		AX_CHECK_ZLIB
		if test -n "${LIBS}"
		then
			ZLIB_CFLAGS="${CPPFLAGS}"
			ZLIB_LIBS="${LDFLAGS} ${LIBS}"
		fi
	fi
	AC_SUBST(PKGFLAGS_ZLIB)
	AC_SUBST(ZLIB_CFLAGS)
	AC_SUBST(ZLIB_LIBS)
fi

# Check libpng
if test ${USE_libpng} = "yes"
then
	PKG_CHECK_MODULES([PNG], libpng, have_libpng=yes, AC_MSG_ERROR(Cannot find libpng))
	echo "have_libpng=$have_libpng"
fi
if test ${have_libpng} = "yes"
then
	PKGFLAGS_PNG="png"
else
	PKGFLAGS_PNG=""
	PNG_CFLAGS=""
	PNG_LIBS=""
fi
AC_SUBST(PKGFLAGS_PNG)
AC_SUBST(PNG_CFLAGS)
AC_SUBST(PNG_LIBS)

# Checks for Xwindow
	AC_PATH_X
	AC_PATH_XTRA
	X_LIBS="${X_LIBS} -lX11"
	AC_SUBST(X_LIBS)
	AC_SUBST(X_CFLAGS)

# Checks for Cocoa framework
if test ${USE_Cocoa} = "yes"
then
	USE_gl = "yes"
	GL_LIBS="-framework OpenGL  -framework CoreVideo  -framework IOKit -framework Cocoa"
	GL_INC="${X_CFLAGS}"
	COCOA_FLAG="-D__APPLE__"
# Check Aquaterm
	LIBS=""
	AX_CHECK_AQUATERM
	AC_SUBST(HAVE_LIBAQUATERM)
	AQUATERM_LIBS="-F${AQUATERM_HOME} ${LIBS} -Wl,-framework -Wl,Foundation"
	AC_MSG_RESULT(AQUATERM_LIBS: ${AQUATERM_LIBS})
else
	COCOA_FLAG=""
# Checks for OpenGL
	if test ${USE_gl} = "yes"
	then
		PKG_CHECK_MODULES([GL], gl, have_gl=yes, AC_MSG_ERROR(Cannot find OpenGL))
		echo "have_gl=$have_gl"
	fi
	
	if test ${have_gl} = "yes"
	then
		PKGFLAGS_GL="OpenGL"
	else
		PKGFLAGS_GL=""
	fi
	
	AQUATERM_LIBS=""
fi
AC_SUBST(COCOA_FLAG)

AC_SUBST(PKGFLAGS_GL)
AC_SUBST(GL_LIBS)
AC_SUBST(GL_INC)
AC_SUBST(AQUATERM_LIBS)

if test -n "${GL_LIBS}"
then
# Check GTK+3
	if test ${USE_gtk} = "yes"
	then
		PKG_CHECK_MODULES([GTK3], gtk+-3.0, have_gtk=yes, AC_MSG_ERROR(Cannot find GTK+ 3.0))
		echo "have_gtk=$have_gtk"
	fi
	
	if test ${have_gtk} = "yes"
	then
		PKGFLAGS_GTK="gtk+-3.0"
	else
		PKGFLAGS_GTK=""
	fi
else
	CXX=""
	
	PKGFLAGS_GTK=""
	GTK3_CFLAGS=""
	GTK3_LIBS=""
fi

AC_SUBST(CXX)
AC_SUBST(CXXFLAGS)

AC_SUBST(PKGFLAGS_GTK)
AC_SUBST(GTK3_CFLAGS)
AC_SUBST(GTK3_LIBS)

if test -n "${GTK3_LIBS}"
then
# Check GLFW3
	if test ${USE_glfw} = "yes"
	then
		PKG_CHECK_MODULES([GLFW], glfw3, have_glfw=yes, AC_MSG_ERROR(Cannot find GLFW))
	fi
	
	if test ${have_glfw} = "yes"
	then
		PKGFLAGS_GLFW="glfw-3"
	else
		PKGFLAGS_GLFW=""
	fi
else
	PKGFLAGS_GLFW=""
	GLFW_CFLAGS=""
	GLFW_LIBS=""
fi


AC_SUBST(PKGFLAGS_GTK)
AC_SUBST(GTK3_CFLAGS)
AC_SUBST(GTK3_LIBS)

AC_SUBST(PKGFLAGS_GLFW)
AC_SUBST(GLFW_CFLAGS)
AC_SUBST(GLFW_LIBS)

# Check pgplot for Fortran compiler
LIBS="${ZLIB_LIBS} ${PNG_LIBS} ${X_LIBS} ${AQUATERM_LIBS}"
LDFLAGS=""
AX_CHECK_PGPLOT
AC_SUBST(HAVE_LIBPGPLOT)
AC_MSG_RESULT(LIBS... "${LIBS}")
if test -n "${LIBS}"
	then
	PGPLOT_LIBS="${LDFLAGS} ${LIBS}"
fi
AC_SUBST(PGPLOT_LIBS)

AC_CANONICAL_HOST

AC_MSG_RESULT()
AC_MSG_RESULT(-----   Configuration summary   -------------------------------)
AC_MSG_RESULT()
AC_MSG_RESULT(  host:          "${host}")
AC_MSG_RESULT(XL_FORTRAN:      "${XL_FORTRAN}")
AC_MSG_RESULT(x86 instruction: "${SSE_FLAG}")
AC_MSG_RESULT(mpirun command and options: "${MPIRUN}")
AC_MSG_RESULT()
if test -n "${OPENMP_FCFLAGS}"; then
        AC_MSG_RESULT(    Use OpenMP ...        yes)
else
        AC_MSG_RESULT(    Use OpenMP ...         no)
fi
if test -n "${COCOA_FLAG}"; then
        AC_MSG_RESULT(    Use Cocoa...          yes)
else
        AC_MSG_RESULT(    Use Cocoa...           no)
fi
if test -n "${X_LIBS}"; then
        AC_MSG_RESULT(    Use X Window...       yes)
else
        AC_MSG_RESULT(    Use X Window...        no)
fi
AC_MSG_RESULT()
if test -n "${BLAS_LIBS}"; then
        AC_MSG_RESULT(    Use BLAS ...          yes)
else
        AC_MSG_RESULT(    Use BLAS ...           no)
fi
AC_MSG_RESULT()
if test -n "${FFTW3_LIBS}"; then
        AC_MSG_RESULT(    Use FFTW3 ...         yes)
	if test -n "${FFTW3_OMP_LIBS}"; then
        AC_MSG_RESULT(    Use FFTW3 with OpenMP ... yes)
	else
        AC_MSG_RESULT(    Use FFTW3 with OpenMP ... no)
	fi
else
        AC_MSG_RESULT(    Use FFTW3 ...          no)
fi
if test -n "${HDF5_PFLIBS}"; then
        AC_MSG_RESULT(    Use parallel HDF5 ... yes)
else
        AC_MSG_RESULT(    Use parallel HDF5 ...  no)
fi
AC_MSG_RESULT()
if test -n "${ZLIB_LIBS}"; then
        AC_MSG_RESULT(    Use zlib ...          yes)
else
        AC_MSG_RESULT(    Use zlib ...           no)
fi
if test -n "${PNG_LIBS}"; then
        AC_MSG_RESULT(    Use libpng output...     yes)
else
        AC_MSG_RESULT(    Use libpng output...      no)
fi
AC_MSG_RESULT()
if test -n "${GTK3_LIBS}"; then
        AC_MSG_RESULT(    Use GTK3+...          yes)
else
        AC_MSG_RESULT(    Use GTK3+...           no)
fi
if test -n "${GLFW_LIBS}"; then
        AC_MSG_RESULT(    Use GLFW...           yes)
else
        AC_MSG_RESULT(    Use GLFW...            no)
fi
AC_MSG_RESULT()
if test -n "${PGPLOT_LIBS}"; then
        AC_MSG_RESULT(    Use PGPLOT...         yes)
else
        AC_MSG_RESULT(    Use PGPLOT...          no)
fi
if test -n "${AQUATERM_LIBS}"; then
        AC_MSG_RESULT(    Use Aquaterm...       yes)
else
        AC_MSG_RESULT(    Use Aquaterm...        no)
fi
AC_MSG_RESULT()
AC_MSG_RESULT(---------------------------------------------------------------)
AC_MSG_RESULT()



AC_CONFIG_FILES([Makefile])
AC_OUTPUT
