!************************************************************************
!* ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
!* Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
!*
!* This library is free software; you can redistribute it and/or
!* modify it under the terms of the GNU Lesser General Public
!* License as published by the Free Software Foundation; either
!* version 2.1 of the License, or (at your option) any later version.
!*
!* This library is distributed in the hope that it will be useful,
!* but WITHOUT ANY WARRANTY; without even the implied warranty of
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!* Lesser General Public License for more details.
!* 
!* You should have received a copy of the GNU Lesser General Public
!* License along with this library; if not, write to the Free Software
!* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!* 02110-1301 USA.
!************************************************************************
!*************************************************************************
!*     SINE TRANSFORM (TRAPEZOIDAL)                   2000/09/19 K.Ishioka      
!*************************************************************************
!************************************************************************
      SUBROUTINE FTTSTI(N,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)      
      DIMENSION T(0:N/2-1,5),IT(5)

      CALL FTTRUI(N,IT,T)
 
      N2=N*2
 
      DO I=0,N/2-1
        T(I,5)=SIN(2*PI*(2*I+1)/N2)
      END DO
 
      END
!************************************************************************
      SUBROUTINE FTTSTF(M,N,X,Y,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,5)
      DIMENSION X(M,N),Y(M,0:N-1)

      DO J=1,N/2-1
        J1=2*J
        J2=N-J1
        J3=2*J+1
        J4=N-J3
        DO I=1,M
          Y(I,J1)=X(I,J2)-X(I,J1)+2*T(J,4)*(X(I,J2)+X(I,J1))
          Y(I,J3)=X(I,J4)-X(I,J3)+2*T(J,5)*(X(I,J4)+X(I,J3))
        END DO
      END DO
 
      J=0
      J3=2*J+1
      J4=N-J3
      DO I=1,M
        Y(I,0)=0
        Y(I,1)=X(I,J4)-X(I,J3)+2*T(J,5)*(X(I,J4)+X(I,J3))
      END DO
 
      CALL FTTRUF(M,N,Y,X,IT,T) 
 
      DO I=1,M
        X(I,N)=0
        X(I,1)=0.5D0*Y(I,0)
!*        X(I,N-1)=-0.5D0*Y(I,1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          X(I,2*J)=Y(I,2*J+1)
          X(I,2*J+1)=X(I,2*J-1)+Y(I,2*J)
        END DO
      END DO
 
      END
!************************************************************************
      SUBROUTINE FTTSTB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,5)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)

      CALL FTTSTF(M,N,X,Y,IT,T)

      DO J=0,N-1
        DO I=1,M
          X(I,J)=(N/2D0)*X(I,J)
        END DO
      END DO

      END
