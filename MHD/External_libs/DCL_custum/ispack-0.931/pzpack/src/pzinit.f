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
*     INITIALIZATION OF PZPACK                                2009/05/22
************************************************************************
      SUBROUTINE PZINIT(JM,IM,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ITJ(2,2),TJ(JM*3,2),ITI(2,2),TI(IM*4,2)

      CALL FJRINI(JM,1,1,ITJ(1,1),TJ(1,1))
      CALL FJRINI(JM,1,2,ITJ(1,2),TJ(1,2))
      CALL FJCINI(IM,1,1,ITI(1,1),TI(1,1))
      CALL FJCINI(IM,1,2,ITI(1,2),TI(1,2))

      END
