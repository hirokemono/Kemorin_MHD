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
*     INITIALIZE D for Laplacian                              2009/08/19
*-----------------------------------------------------------------------
      SUBROUTINE SJINID(MM,D)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D((MM+1)*(MM+1),2)

      IND=0
      M=0
      IND=IND+1
      D(1,1)=0
      D(1,2)=1
      DO N=1,MM
        IND=IND+1
        D(IND,1)=-N*(N+1)
        D(IND,2)=1D0/D(IND,1)
      END DO
      DO M=1,MM
        DO N=M,MM
          IND=IND+1
          D(IND,1)=-N*(N+1)
          D(IND,2)=1D0/D(IND,1)          
          IND=IND+1
          D(IND,1)=D(IND-1,1)
          D(IND,2)=D(IND-1,2)
        END DO
      END DO

      END
