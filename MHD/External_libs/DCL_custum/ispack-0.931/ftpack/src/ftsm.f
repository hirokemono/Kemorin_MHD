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
*************************************************************************
*     SINE TRANSFORM (MID-POINT)                     2000/09/19 K.Ishioka      
*************************************************************************
************************************************************************
      SUBROUTINE FTTSMI(N,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)      
      DIMENSION T(0:N/2-1,12),IT(5)

      CALL FTTRUI(N,IT,T)
 
      N2=N*2

      DO I=0,N/2-1
        T(I,5)=SIN(2*PI*(2*I+1)/N2)        
        T(I,6)=COS(2*PI*(2*I+1)/N2)
        T(I,7)=COS(PI*I/N)
        T(I,8)=SIN(PI*I/N)
        T(I,9)=SIN(PI*(4*I+1)/N2)
        T(I,10)=SIN(PI*(4*I+3)/N2)
        T(I,11)=1/T(I,9)
        T(I,12)=1/T(I,10)
      END DO
 
      END
************************************************************************
      SUBROUTINE FTTSMF(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)
 
      DO J=0,N/2-1
        J1=2*J
        J2=N-1-J1
        J3=2*J+1
        J4=N-1-J3
        DO I=1,M
          Y(I,2*J)=-(X(I,J1)-X(I,J2))+2*T(J,9 )*(X(I,J1)+X(I,J2))
          Y(I,2*J+1)=-(X(I,J3)-X(I,J4))+2*T(J,10)*(X(I,J3)+X(I,J4))
        END DO
      END DO

      CALL FTTRUF(M,N,Y,X,IT,T)
 
      DO I=1,M
        X(I,0)=0.5D0*Y(I,0)
        X(I,N-1)=-Y(I,1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          X(I,2*J-1)=T(J,7)*Y(I,2*J+1)-T(J,8)*Y(I,2*J)
          X(I,2*J)=X(I,2*J-2)+T(J,7)*Y(I,2*J)+T(J,8)*Y(I,2*J+1)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE FTTSMB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)

      DO I=1,M
        Y(I,0)=2*X(I,0)
        Y(I,1)=-X(I,N-1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          Y(I,2*J)=(X(I,2*J)-X(I,2*J-2))*T(J,7)-T(J,8)*X(I,2*J-1)
          Y(I,2*J+1)=(X(I,2*J)-X(I,2*J-2))*T(J,8)+T(J,7)*X(I,2*J-1)
        END DO
      END DO
      
      CALL FTTRUB(M,N,Y,X,IT,T)

      DO J=0,N/2-1
        DO I=1,M
          X(I,2*J  )=-0.25D0*(Y(I,2*J)-Y(I,2*(N/2-1-J)+1))
     &      +0.125D0*(Y(I,2*J)+Y(I,2*(N/2-1-J)+1))*T(J,11)
          X(I,2*J+1)=-0.25D0*(Y(I,2*J+1)-Y(I,2*(N/2-1-J)))
     &      +0.125D0*(Y(I,2*J+1)+Y(I,2*(N/2-1-J)))*T(J,12)
        END DO
      END DO
      
      END
