# SYNOPSIS
#
#   AX_CHECK_LIBPNG()
#
# DESCRIPTION
#
#   This macro searches for an installed libpng library. If nothing was
#   specified when calling configure, it searches first in /usr/local and
#   then in /usr, /opt/local and /sw. If the --with-libpng=DIR is specified,
#   it will try to find it in DIR/include/png.h and DIR/lib/libpng.a. If
#   --without-libpng is specified, the library is not searched at all.
#
#   If either the header file (png.h) or the library (libpng) is not found,
#   the configuration exits on error, asking for a valid libpng installation
#   directory or --without-libpng.
#
#   The macro defines the symbol HAVE_LIBPNG if the library is found. You
#   should use autoheader to include a definition for this symbol in a
#   config.h file. Sample usage in a C/C++ source is as follows:
#
#     #ifdef HAVE_LIBPNG
#     #include <png.h>
#     #endif /* HAVE_LIBPNG */
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

AU_ALIAS([CHECK_LIBPNG], [AX_CHECK_LIBPNG])
AC_DEFUN([AX_CHECK_LIBPNG],
[AC_REQUIRE([AX_CHECK_ZLIB]) dnl Check zlib
#
# Handle user hints
#
AC_MSG_CHECKING(if libpng is wanted)
AC_ARG_WITH(libpng,
[  --with-libpng=DIR root directory path of libpng installation [defaults to
                    /usr/local or /usr if not found in /usr/local]
  --without-libpng to disable libpng usage completely],
[if test "$withval" != no ; then
  libpng_places="/usr/local /usr /opt/local /sw"
  AC_MSG_RESULT(yes)
  if test -d "$withval"
  then
    libpng_places="$withval $libpng_places"
  else
    AC_MSG_WARN([Sorry, $withval does not exist, checking usual places])
  fi
else
  AC_MSG_RESULT(no)
fi],
[AC_MSG_WARN([Checking libpng in usual places])
 libpng_places="/usr/local /usr /opt/local /sw"
 AC_MSG_RESULT(yes)])

#
# Locate libpng, if wanted
#
AC_MSG_RESULT(${libpng_places})
if test -n "${libpng_places}"
then
	# check the user supplied or any other more or less 'standard' place:
	#   Most UNIX systems      : /usr/local and /usr
	#   MacPorts / Fink on OSX : /opt/local respectively /sw
	for LIBPNG_HOME in ${libpng_places} ; do
	  if test -f "${LIBPNG_HOME}/include/png.h"; then break; fi
	  LIBPNG_HOME=""
	done

	# if png.h was nowhere to be found, give a notice and bail out
	if test ! -n "${LIBPNG_HOME}"; then
          AC_MSG_ERROR(No png.h in any include directory of ${libpng_places}: either specify a valid libpng installation with --with-libpng=DIR or disable libpng usage with --without-libpng)
	fi

        LIBPNG_OLD_LDFLAGS=$LDFLAGS
        LIBPNG_OLD_CPPFLAGS=$LDFLAGS
        LDFLAGS="$LDFLAGS -L${LIBPNG_HOME}/lib"
        CPPFLAGS="$CPPFLAGS -I${LIBPNG_HOME}/include"
        AC_LANG_SAVE
        AC_LANG_C
        AC_CHECK_LIB(png, png_write_end, [libpng_cv_libpng=yes], [libpng_cv_libpng=no])
        AC_CHECK_HEADER(png.h, [libpng_cv_libpng_h=yes], [libpng_cv_libpng_h=no])
        AC_LANG_RESTORE
        if test "$libpng_cv_libpng" = "yes" -a "$libpng_cv_libpng_h" = "yes"
        then
                #
                # If both library and header were found, use them
                #
                AC_CHECK_LIB(png, png_write_end)
                AC_MSG_CHECKING(libpng in ${LIBPNG_HOME})
                AC_MSG_RESULT(ok)
        else
                #
                # If either header or library was not found, revert and bomb
                #
                AC_MSG_CHECKING(libpng in ${LIBPNG_HOME})
                LDFLAGS="$LIBPNG_OLD_LDFLAGS"
                CPPFLAGS="$LIBPNG_OLD_CPPFLAGS"
                AC_MSG_RESULT(failed)
                AC_MSG_ERROR(either specify a valid libpng installation with --with-libpng=DIR or disable libpng usage with --without-libpng)
        fi
fi

])
