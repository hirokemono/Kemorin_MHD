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
*     REPACKING SPECTRA FOR x-grad                            2009/08/19
************************************************************************
      SUBROUTINE SJCS2X(MM,S,SX)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)),SX((MM+1)*(MM+1))

      M=0
      DO N=0,MM
        SX(N+1)=0
      END DO
      DO M=1,MM
        NS=MM+1+(MM+(MM-(M-2)))*(M-1)+1
        DO N=M,MM
          L=NS+2*(N-M)
          SX(L)=-M*S(L+1)
          SX(L+1)=M*S(L)
        END DO
      END DO

      END
