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
!************************************************************************
      SUBROUTINE FTTRUI(N,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION T(0:N/2-1,4),IT(5)

      IF(MOD(N,2).NE.0) THEN
        CALL FTDMSG('E','FTTRUI','N MUST BE EVEN.')
      END IF

      L=N/2

      CALL FTTZLI(L,IT,T)

      DO 10 I=0,L-1
        T(I,3)=COS(2*PI*I/N)
        T(I,4)=SIN(2*PI*I/N)
   10 CONTINUE

      END
!************************************************************************
      SUBROUTINE FTTRUF(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M,2,0:N/2-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      L=N/2

      DO 20 J=0,L-1
        DO 10 I=1,M
          Y(I,J,1)=X(I,1,J)
          Y(I,J,2)=X(I,2,J)
   10   CONTINUE
   20 CONTINUE

      CALL FTTZLM(M,L,Y,X,IT,T)

      R=1D0/N
      S=R/2

      DO 30 I=1,M
        X(I,1,0)=R*(Y(I,0,1)+Y(I,0,2))
        X(I,2,0)=R*(Y(I,0,1)-Y(I,0,2))
   30 CONTINUE

      DO 50 J=1,L-1
        DO 40 I=1,M
          X(I,1,J)=S*(        (Y(I,L-J,1)+Y(I,J,1))                     &
     &                +T(J,3)*(Y(I,L-J,2)+Y(I,J,2))                     &
     &                -T(J,4)*(Y(I,L-J,1)-Y(I,J,1)))
          X(I,2,J)=S*(        (Y(I,L-J,2)-Y(I,J,2))                     &
     &                -T(J,3)*(Y(I,L-J,1)-Y(I,J,1))                     &
     &                -T(J,4)*(Y(I,L-J,2)+Y(I,J,2)))
   40   CONTINUE
   50 CONTINUE

      END
!************************************************************************
      SUBROUTINE FTTRUB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M,2,0:N/2-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      L=N/2

      DO 10 I=1,M
        Y(I,0,1)=X(I,1,0)+X(I,2,0)
        Y(I,0,2)=X(I,1,0)-X(I,2,0)
   10 CONTINUE

      DO 30 J=1,L-1
        DO 20 I=1,M
          Y(I,J,1)=        (X(I,1,L-J)+X(I,1,J))                        &
     &             -T(J,3)*(X(I,2,L-J)+X(I,2,J))                        &
     &             +T(J,4)*(X(I,1,L-J)-X(I,1,J))
          Y(I,J,2)=       -(X(I,2,L-J)-X(I,2,J))                        &
     &             -T(J,3)*(X(I,1,L-J)-X(I,1,J))                        &
     &             -T(J,4)*(X(I,2,L-J)+X(I,2,J))
   20   CONTINUE
   30 CONTINUE

      CALL FTTZLM(M,L,Y,X,IT,T)

      DO 50 J=0,L-1
        DO 40 I=1,M
          X(I,1,J)=Y(I,J,1)
          X(I,2,J)=Y(I,J,2)
   40   CONTINUE
   50 CONTINUE

      END
