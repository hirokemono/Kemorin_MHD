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
*     SUBROUTINES FOR CONVERTING (LAPLACIAN)                    98/02/16
************************************************************************
      SUBROUTINE SMCLAP(MM,KM,A,B,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1),KM)
      DIMENSION B((MM+1)*(MM+1),KM)
      DIMENSION R((MM+1)*(MM+1))

      LM=(MM+1)*(MM+1)

      DO K=1,KM
        DO L=1,LM
          B(L,K)=R(L)*A(L,K)
        END DO
      END DO

      END
