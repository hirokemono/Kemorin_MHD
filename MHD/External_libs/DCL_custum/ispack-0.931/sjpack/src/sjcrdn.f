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
*     REPACKING SPECTRA from larger truncation number NN      2009/08/20
************************************************************************
      SUBROUTINE SJCRDN(MM,NN,SR,S)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)),SR((2*NN+1-MM)*MM+NN+1)

      DO N=0,MM
        S(N+1)=SR(N+1)
      END DO
      DO M=1,MM
        NS=MM+2+(M-1)*(2*MM+2-M)
        NSR=NN+2+(M-1)*(2*NN+2-M)
        DO L=0,2*(MM-M+1)-1
          S(NS+L)=SR(NSR+L)
        END DO
      END DO

      END
