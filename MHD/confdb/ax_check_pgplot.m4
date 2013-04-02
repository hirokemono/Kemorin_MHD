# SYNOPSIS
#
#   AX_CHECK_PGPLOT()
#
# DESCRIPTION
#
#   This macro searches for an installed pgplot library. If nothing was
#   specified when calling configure, it searches first in /usr/local and
#   then in /usr, /opt/local and /sw. If the --with-pgplot=DIR is specified,
#   it will try to find DIR/lib/pgplot.a.
#   If --without-pgplot is specified, the library is not searched at all.
#
#   If the library (pgplot) is not found,
#   the configuration exits on error, asking for a valid pgplot installation
#   directory or --without-pgplot.
#
#   The macro defines the symbol HAVE_LIBPGPLOT if the library is found. You
#   should use autoheader to include a definition for this symbol in a
#   config.h file. Sample usage in a C/C++ source is as follows:
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

AU_ALIAS([CHECK_PGPLOT], [AX_CHECK_PGPLOT])
AC_DEFUN([AX_CHECK_PGPLOT],
[AC_REQUIRE([PKG_CHECK_MODULES]) dnl Check libpng
#
# Handle user hints
#
AC_MSG_CHECKING(if pgplot is wanted)
AC_ARG_WITH(pgplot,
[  --with-pgplot=DIR root directory path of pgplot installation [defaults to
                    /usr/local or /usr if not found in /usr/local]
  --without-pgplot to disable pgplot usage completely],
[if test "$withval" != no ; then
  pgplot_places="/usr/local /usr /opt/local /sw"
  AC_MSG_RESULT(yes)
  if test -d "$withval"
  then
    pgplot_places="$withval $pgplot_places"
  else
    AC_MSG_WARN([Sorry, $withval does not exist, checking usual places])
  fi
else
  LIBS=""
  AC_MSG_RESULT(no)
fi],
[AC_MSG_WARN([Checking pgplot in usual places])
 pgplot_places="/usr/local /usr /opt/local /sw"
 AC_MSG_RESULT(yes)])

#
# Locate pgplot, if wanted
#
if test -n "${pgplot_places}"
then
	# check the user supplied or any other more or less 'standard' place:
	#   Most UNIX systems      : /usr/local and /usr
	#   MacPorts / Fink on OSX : /opt/local respectively /sw
	for PGPLOT_HOME in ${pgplot_places} ; do
	  if test -f "${PGPLOT_HOME}/lib/libpgplot.a"; then break; fi
	  PGPLOT_HOME=""
	done

	# if libpgplot.a was nowhere to be found, give a notice and bail out
	if test ! -n "${PGPLOT_HOME}"; then
          AC_MSG_ERROR(No libpgplot.a in any include directory of ${pgplot_places}: either specify a valid pgplot installation with --with-pgplot=DIR or disable pgplot usage with --without-pgplot)
	fi

        PLPLOT_OLD_LDFLAGS=$LDFLAGS
        LDFLAGS="$LDFLAGS -L${PGPLOT_HOME}/lib"
        AC_LANG_SAVE
        AC_LANG(Fortran)
        AC_CHECK_LIB(pgplot, pgopen, [pgplot_cv_pgplot=yes], [pgplot_cv_pgplot=no],
         [$LDFLAGS])
        if test "$pgplot_cv_pgplot" = "yes"
        then
                #
                # If library was found, use them
                #
                AC_CHECK_LIB(pgplot, pgopen)
                AC_MSG_CHECKING(pgplot in ${PGPLOT_HOME})
                AC_MSG_RESULT(ok)
        else
                #
                # If library was not found, revert and bomb
                #
                AC_MSG_CHECKING(pgplot in ${PGPLOT_HOME})
                LDFLAGS="$PLPLOT_OLD_LDFLAGS"
                AC_MSG_RESULT(failed)
                AC_MSG_ERROR(either specify a valid pgplot installation with --with-pgplot=DIR or disable pgplot usage with --without-pgplot)
        fi
        AC_LANG_RESTORE
fi

])
