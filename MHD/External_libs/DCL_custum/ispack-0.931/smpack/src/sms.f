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
*     LOWER ROUTINES FOR SMPACK (REARRANGEMENT IN SPECTRA)      98/02/16
************************************************************************
      SUBROUTINE SMSSSB(MM,KM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1),KM),B(KM,(MM+1)*(MM+1))

      DO K=1,KM
        DO L=1,(MM+1)*(MM+1)
          B(K,L)=A(L,K)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SMSSSF(MM,KM,B,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION B(KM,(MM+1)*(MM+1)),A((MM+1)*(MM+1),KM)

      DO K=1,KM
        DO L=1,(MM+1)*(MM+1)
          A(L,K)=B(K,L)
        END DO
      END DO

      END
