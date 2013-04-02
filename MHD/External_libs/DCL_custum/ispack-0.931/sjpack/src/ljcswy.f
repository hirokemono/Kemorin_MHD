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
*     REPACKING SPECTRA TRANSFORM for y grad (wave component)
*                                                            2009 /08/17
*-----------------------------------------------------------------------      
      SUBROUTINE LJCSWY(MM,M,Z,ZG,C)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      DIMENSION Z(M:MM),ZG(M:MM+1)
      DIMENSION C((MM+1)*(MM+1))

      NS=(2*MM+1+2*MM+1-2*(M-1))*M/2

      N=M

      IF(N.NE.MM) THEN
        ZG(N)=C(NS+MM-M+1+1)*Z(N+1)
      END IF
      DO N=M+1,MM-1
        ZG(N)=C(NS+N-M)*Z(N-1)+C(NS+MM-M+1+1+N-M)*Z(N+1)
      END DO
      IF(M.NE.MM) THEN
        ZG(MM)=C(NS+MM-M)*Z(MM-1)
      ELSE
        ZG(MM)=0
      END IF
      ZG(MM+1)=C(NS+MM+1-M)*Z(MM)

      END
