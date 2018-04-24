# SYNOPSIS
#
#   AX_CHECK_AQUATERM()
#
# DESCRIPTION
#
#   This macro searches for an installed aquaterm. If nothing was
#   specified when calling configure, it searches first in /Library/Frameworks and
#   then in /System/Library/Frameworks, /opt/local/Frameworks and /sw/Frameworks. If the --with-aquaterm=DIR is specified,
#   it will try to find it in DIR/AquaTerm.framework and DIR/lib/libz.a. If
#   --without-aquaterm is specified, the library is not searched at all.
#
#   If the execute file (AquaTerm.framework/AquaTerm) is not found,
#   the configuration exits on error, asking for a valid aquaterm installation
#   directory or --without-aquaterm.
#
#   The macro defines the symbol HAVE_AQUATERM if the library is found. You
#   should use autoheader to include a definition for this symbol in a
#   config.h file. Sample usage in a C/C++ source is as follows:
#
#     #ifdef HAVE_AQUATERM
#     #endif /* HAVE_AQUATERM */
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

AU_ALIAS([CHECK_AQUATERM], [AX_CHECK_AQUATERM])
AC_DEFUN([AX_CHECK_AQUATERM],
#
# Handle user hints
#
[AC_MSG_CHECKING(if aquaterm is wanted)
AC_ARG_WITH(aquaterm,
[  --with-aquaterm=DIR root directory path of aquaterm installation [defaults to
                    /Library/Frameworks or /System/Library/Frameworks if not found in /Library/Frameworks]
  --without-aquaterm to disable aquaterm usage completely],
[if test "$withval" != no ; then
  aquaterm_places="/Library/Frameworks /System/Library/Frameworks /opt/local/Frameworks /sw/Frameworks"
  AC_MSG_RESULT(yes)
  if test -d "$withval"
  then
    aquaterm_places="$withval $aquaterm_places"
  else
    AC_MSG_WARN([Sorry, $withval does not exist, checking usual places])
  fi
else
  AC_MSG_RESULT(no)
fi],
[AC_MSG_WARN([Checking aquaterm in usual places])
 aquaterm_places="/Library/Frameworks /System/Library/Frameworks /opt/local/Frameworks /sw/Frameworks"
 AC_MSG_RESULT(yes)])

#
# Locate aquaterm, if wanted
#
if test -n "${aquaterm_places}"
then
	# check the user supplied or any other more or less 'standard' place:
	#   Most UNIX systems      : /Library/Frameworks and /System/Library/Frameworks
	#   MacPorts / Fink on OSX : /opt/local/Frameworks respectively /sw/Frameworks
	for AQUATERM_HOME in ${aquaterm_places} ; do
	  if test -f "${AQUATERM_HOME}/AquaTerm.framework/AquaTerm"; then break; fi
	  AQUATERM_HOME=""
	done

	# if AquaTerm.framework/AquaTerm was nowhere to be found, give a notice and bail out
	if test ! -n "${AQUATERM_HOME}"; then
          AC_MSG_ERROR(No AquaTerm.framework/AquaTerm in any directory of ${aquaterm_places}: either specify a valid aquaterm installation with --with-aquaterm=DIR or disable aquaterm usage with --without-aquaterm)
	fi

	AQUATERM_OLD_LDFLAGS=$LDFLAGS
	AQUATERM_OLD_CPPFLAGS=$LDFLAGS
	LDFLAGS="$LDFLAGS -L${AQUATERM_HOME}"
	LIBS="$LIBS -framework AquaTerm"

	AC_MSG_CHECKING(aquaterm in ${AQUATERM_HOME})
	AC_MSG_RESULT(ok)
fi

])