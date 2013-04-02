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
*     LOWER ROUTINES FOR SMPACK (FOURIER TRANSFORM)             99/03/11
************************************************************************
      SUBROUTINE SMFRUB(M,N,MM,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M,-MM:N-MM-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      L=N/2

      DO I=1,M
        Y(I,0,1)=X(I,0)
        Y(I,0,2)=X(I,0)
      END DO

      DO J=1,L-MM-1
        DO I=1,M
          Y(I,J,1)=X(I, J)-T(J,3)*X(I,-J)-T(J,4)*X(I, J)
          Y(I,J,2)=X(I,-J)+T(J,3)*X(I, J)-T(J,4)*X(I,-J)
        END DO
      END DO

      DO J=L-MM,MM
        DO I=1,M
          Y(I,J,1)=        (X(I,L-J)+X(I, J))
     &             -T(J,3)*(X(I,J-L)+X(I,-J))
     &             +T(J,4)*(X(I,L-J)-X(I, J))
          Y(I,J,2)=       -(X(I,J-L)-X(I,-J))
     &             -T(J,3)*(X(I,L-J)-X(I, J))
     &             -T(J,4)*(X(I,J-L)+X(I,-J))
        END DO
      END DO

      DO J=MAX(L-MM,MM+1),L-1
        DO I=1,M
          Y(I,J,1)= X(I,L-J)-T(J,3)*X(I,J-L)+T(J,4)*X(I,L-J)
          Y(I,J,2)=-X(I,J-L)-T(J,3)*X(I,L-J)-T(J,4)*X(I,J-L)
        END DO
      END DO

      DO J=MM+1,L-MM-1
        DO I=1,M
          Y(I,J,1)=0
          Y(I,J,2)=0
        END DO
      END DO

      CALL FTTZLM(M,L,Y,X,IT,T)

      DO J=0,L-1
        DO I=1,M
          X(I,-MM  +2*J)=Y(I,J,1)
          X(I,-MM+1+2*J)=Y(I,J,2)
        END DO
      END DO
          
      END
************************************************************************
      SUBROUTINE SMFRUF(M,N,MM,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M,-MM:N-MM-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      L=N/2

      DO J=0,L-1
        DO I=1,M
          Y(I,J,1)=X(I,-MM  +2*J)
          Y(I,J,2)=X(I,-MM+1+2*J)
        END DO
      END DO

      CALL FTTZLM(M,L,Y,X,IT,T)

      R=1D0/N
      S=R/2

      DO I=1,M
        X(I,0)=R*(Y(I,0,1)+Y(I,0,2))
      END DO

      DO J=1,MM
        DO I=1,M
          X(I, J)=S*(        (Y(I,L-J,1)+Y(I,J,1))
     &               +T(J,3)*(Y(I,L-J,2)+Y(I,J,2))
     &               -T(J,4)*(Y(I,L-J,1)-Y(I,J,1)))
          X(I,-J)=S*(        (Y(I,L-J,2)-Y(I,J,2))
     &               -T(J,3)*(Y(I,L-J,1)-Y(I,J,1))
     &               -T(J,4)*(Y(I,L-J,2)+Y(I,J,2)))
        END DO
      END DO

      END
