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
*     OBTAIN GRID POINTS (IN RADIAN)                            95/10/05
************************************************************************
      SUBROUTINE STOGRD(JM,IM,Y,X,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION Y(JM),X(0:IM-1)
      DIMENSION Q(JM/2,3)

      CALL LTOGRD(JM,Y,Q)

      DO I=0,IM-1
        X(I)=2*PI*I/IM
      END DO

      END
************************************************************************
      SUBROUTINE LTOGRD(JM,Y,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM)
      DIMENSION Q(JM/2,3)

      JH=JM/2

      DO J=1,JH
        Y(JH+J  )= ASIN(Q(J,3))
        Y(JH-J+1)=-ASIN(Q(J,3))
      END DO

      END
