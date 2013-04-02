************************************************************************
* ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
* Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
************************************************************************
************************************************************************
*     pseudorandom number generator                           2010/02/26
************************************************************************
      SUBROUTINE APRAND(ISEED,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(IA=1664525,IC=1013904223,AM=2D0**32)

      ISEED=IA*ISEED+IC
      R=(IBCLR(ISEED,31)+0.5D0)/AM

      IF(BTEST(ISEED,31)) THEN
        R=R+0.5D0
      END IF

      END
