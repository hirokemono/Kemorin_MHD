# SYNOPSIS
#
#   AX_CHECK_MPIF()
#
# DESCRIPTION
#
#   This macro searches for an installed mpif.h. If nothing was
#   specified when calling configure, it searches first in /usr/local and
#   then in /usr, /opt/local and /sw. If the --with-mpif=DIR is specified,
#   it will try to find it in DIR/mpif.h and DIR/lib/libz.a. If
#   --without-mpif is specified, the library is not searched at all.
#
#   If either the header file (mpif.h) or the library (libz) is not found,
#   the configuration exits on error, asking for a valid mpif.h installation
#   directory or --without-mpif.
#
#   The macro defines the symbol HAVE_LIBZ if the library is found. You
#   should use autoheader to include a definition for this symbol in a
#   config.h file. Sample usage in a C/C++ source is as follows:
#
#     #ifdef HAVE_LIBZ
#     #include <mpif.h>
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

AU_ALIAS([CHECK_MPIF], [AX_CHECK_MPIF])
AC_DEFUN([AX_CHECK_MPIF],
#
# Handle user hints
#
[AC_MSG_CHECKING(if mpif.h is wanted)
AC_ARG_WITH(mpif.h,
[  --with-mpif=DIR root directory path of mpif.h installation [defaults to
                    /usr/local or /usr if not found in /usr/local]
  --without-mpif to disable mpif.h usage completely],
[if test "$withval" != no ; then
  mpif_places="/usr/local /usr /opt/local /sw"
  AC_MSG_RESULT(yes)
  if test -d "$withval"
  then
    mpif_places="$withval $withval/include $mpif_places"
  else
    AC_MSG_WARN([Sorry, $withval does not exist, checking usual places])
  fi
else
  AC_MSG_RESULT(no)
fi],
[AC_MSG_WARN([Checking mpif.h in usual places])
 mpif_places="/usr/local /usr /opt/local /sw"
 AC_MSG_RESULT(yes)])

#
# Locate mpif.h, if wanted
#
AC_MSG_RESULT(mpif_places..."${mpif_places}")	  
if test -n "${mpif_places}"
then
	# check the user supplied or any other more or less 'standard' place:
	#   Most UNIX systems      : /usr/local and /usr
	#   MacPorts / Fink on OSX : /opt/local respectively /sw
	for MPIINCDIR in ${mpif_places} ; do
	  if test -f "${MPIINCDIR}/mpif.h"; then break; fi
	  MPIINCDIR=""
	done

	# if mpif.h was nowhere to be found, give a notice and bail out
	if test ! -n "${MPIINCDIR}"; then
          AC_MSG_ERROR(No mpif.h in any include directory of ${mpif_places}: either specify a valid mpif.h installation with --with-mpif=DIR or disable mpif.h usage with --without-mpif)
	fi

        CPPFLAGS="$CPPFLAGS -I${MPIINCDIR}"
        AC_CHECK_HEADER(mpif.h, [mpif_cv_mpif_h=yes], [mpif_cv_mpif_h=no])
        if test "$mpif_cv_mpif_h" = "yes"
        then
                #
                # If both library and header were found, use them
                #
                AC_MSG_RESULT(mpif.h in ${MPIINCDIR} ok)
        else
                #
                # If either header or library was not found, revert and bomb
                #
                AC_MSG_CHECKING(mpif.h in ${MPIINCDIR})
                AC_MSG_RESULT(failed)
                AC_MSG_ERROR(either specify a valid mpif.h installation with --with-mpif=DIR or disable mpif usage with --without-mpif)
        fi
fi

])
