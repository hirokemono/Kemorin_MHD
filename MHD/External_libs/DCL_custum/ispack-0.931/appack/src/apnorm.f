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
*     pseudorandom number generator (normal)                  2010/02/26
************************************************************************
      SUBROUTINE APNORM(ISEED,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DATA IFLAG/0/
      SAVE

      IF(IFLAG.EQ.0) THEN
        CALL APRAND(ISEED,X1)
        CALL APRAND(ISEED,X2)
        Y1=SQRT(-2*LOG(X1))*COS(2*PI*X2)
        Y2=SQRT(-2*LOG(X1))*SIN(2*PI*X2)
        R=Y1
        IFLAG=1
      ELSE
        R=Y2
        IFLAG=0
      END IF

      END
