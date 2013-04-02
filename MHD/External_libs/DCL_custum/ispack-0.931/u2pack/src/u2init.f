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
*     INITIALIZATION OF U2PACK                                2004/08/26
************************************************************************
      SUBROUTINE U2INIT(JM,IM,ITJ,TJ,ITI,TI,Y,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)      
      DIMENSION ITJ(5),TJ(0:JM/2-1,6),ITI(5),TI(IM*2),Y(0:JM-1)

      CALL FTTRUI(JM,ITJ,TJ)
      CALL FTTRUI(IM,ITI,TI)

      DO J=0,JM/2-1
        TJ(J,5)=COS(PI*J/JM)
        TJ(J,6)=SIN(PI*J/JM)
      END DO

      DO J=0,JM-1
        THETA=2*PI*(J+0.5D0-JM/2)/JM
        Y(J)=R*2*TAN(0.5D0*THETA)
      END DO
      

      END
