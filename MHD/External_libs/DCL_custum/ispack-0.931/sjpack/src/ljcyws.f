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
*     REPACKING SPECTRA TRANSFORM for y div (wave component)  2009/08/17
************************************************************************
      SUBROUTINE LJCYWS(MM,M,ZG,Z,C)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      DIMENSION Z(M:MM),ZG(M:MM+1)
      DIMENSION C((MM+1)*(MM+1))

      NS=(2*MM+1+2*MM+1-2*(M-1))*M/2+1

      N=M
      Z(N)=-C(NS+N-M)*ZG(N+1)
      DO N=M+1,MM
        Z(N)=-C(NS+N-M)*ZG(N+1)-C(NS+MM-M+N-M)*ZG(N-1)
      END DO

      END
