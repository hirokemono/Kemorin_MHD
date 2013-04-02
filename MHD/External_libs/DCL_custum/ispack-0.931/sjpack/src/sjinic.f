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
*     INITIALIZE C for y-grad                                 2009/08/20
*-----------------------------------------------------------------------
      SUBROUTINE SJINIC(MM,C)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION C((MM+1)*(MM+1))

      IND=0
      DO M=0,MM
        DO N=M,MM
          IND=IND+1
          C(IND)=-N*SQRT((1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1))
        END DO
        DO N=M+1,MM        
          IND=IND+1
          C(IND)=(N+1)*SQRT((1D0*N*N-M*M)/(4D0*N*N-1))
        END DO
      END DO

      END
