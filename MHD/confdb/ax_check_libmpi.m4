# SYNOPSIS
#
#   AX_CHECK_LIBMPI()
#
# DESCRIPTION
#
#   This macro searches for an installed mpi library. If nothing was
#   specified when calling configure, it searches first in /usr/local and
#   then in /usr, /opt/local and /sw. If the --with-mpi=DIR is specified,
#   it will try to find it in DIR/include/mpi.h and DIR/include/mpif.h
#   and DIR/lib/libz.a. If
#   --without-mpi is specified, the library is not searched at all.
#
#   If either the header file (mpi.h and mpif.h) or the library (libz) is not found,
#   the configuration exits on error, asking for a valid mpi installation
#   directory or --without-mpi.
#
#   The macro defines the symbol HAVE_LIBZ if the library is found. You
#   should use autoheader to include a definition for this symbol in a
#   config.h file. Sample usage in a C/C++ source is as follows:
#
#     #ifdef HAVE_LIBZ
#     #include <mpi.h>
#     #endif /* HAVE_LIBZ */
#
# LICENSE
#
#   Copyright (c) 2008 Loic Dachary <loic@senga.org>
#   Copyright (c) 2010 Bastien Chevreux <bach@chevreux.org>
#
#   This program is free software; you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation; either version 2 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.

#serial 8

AU_ALIAS([CHECK_LIBMPI], [AX_CHECK_LIBMPI])
AC_DEFUN([AX_CHECK_LIBMPI],
#
# Handle user hints
#
[AC_MSG_CHECKING(if mpi is wanted)
AC_ARG_WITH(mpi,
[  --with-mpi=DIR root directory path of mpi installation [defaults to
                    /usr/local or /usr if not found in /usr/local]
  --without-mpi to disable mpi usage completely],
[if test "$withval" != no ; then
  mpi_places="/usr/local /usr /opt/local /sw"
  AC_MSG_RESULT(yes)
  if test -d "$withval"
  then
    mpi_places="$withval $mpi_places"
  else
    AC_MSG_WARN([Sorry, $withval does not exist, checking usual places])
  fi
else
  AC_MSG_RESULT(no)
fi],
[AC_MSG_WARN([Checking mpi in usual places])
 mpi_places="/usr/local /usr /opt/local /sw"
 AC_MSG_RESULT(yes)])

#
# Locate mpi, if wanted
#

if test -n "${mpi_places}"
then
	# check the user supplied or any other more or less 'standard' place:
	#   Most UNIX systems      : /usr/local and /usr
	#   MacPorts / Fink on OSX : /opt/local respectively /sw
	for MPI_HOME in ${mpi_places} ; do
	  if test -f "${MPI_HOME}/include/mpif.h"
	  then 
		MPIINCDIR="${MPI_HOME}/include"
		break
	  elif test -f "${MPI_HOME}/include/openmpi/mpif.h"
	  then 
		MPIINCDIR="${MPI_HOME}/include/openmpi"
		break
	  fi
	  MPI_HOME=""
	done

	# if mpif.h was nowhere to be found, give a notice and bail out
	if test ! -n "${MPI_HOME}"; then
          AC_MSG_ERROR(No mpif.h in any include directory of ${mpi_places}: either specify a valid mpi installation with --with-mpi=DIR or disable mpi usage with --without-mpi)
	fi

		LIBS_OLD=${LIBS}
        MPI_OLD_LDFLAGS=$LDFLAGS
        MPI_OLD_CPPFLAGS=$CPPFLAGS
        LDFLAGS="${LDFLAGS} -L${MPI_HOME}/lib"
		CPPFLAGS="${CPPFLAGS} -I${MPIINCDIR}"
        AC_LANG_SAVE
        AC_LANG_C
        AC_CHECK_LIB(mpi, MPI_Init, [mpi_cv_mpi=yes], [mpi_cv_mpi=no])
        AC_LANG_RESTORE

        AC_CHECK_HEADER(mpi.h,  [mpi_cv_mpi_h=yes], [mpi_cv_mpi_h=no])

		if test "$mpi_cv_mpi" = "yes" -a "$mpi_cv_mpi_h" = "yes"
        then
                #
                # If both library and header were found, use them
                #
                AC_LANG_SAVE
                AC_LANG_C
                AC_CHECK_LIB(mpi,  MPI_Init)
                AC_LANG_RESTORE
                AC_MSG_CHECKING(mpi in ${MPI_HOME})
                AC_MSG_RESULT(ok)
				MPIDIR=${MPI_HOME}
				MPILIBS=${LIBS}
				LIBS=${LIBS_OLD}
        else
                #
                # If either header or library was not found, revert and bomb
                #
                AC_MSG_CHECKING(mpi in ${MPI_HOME})
                LDFLAGS="$MPI_OLD_LDFLAGS"
                CPPFLAGS="$MPI_OLD_CPPFLAGS"
                AC_MSG_RESULT(failed)
                AC_MSG_ERROR(either specify a valid mpi installation with --with-mpi=DIR or disable mpi usage with --without-mpi)
        fi
fi

])