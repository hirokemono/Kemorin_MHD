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
      SUBROUTINE FTTZUI(N,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION T(0:N-1,2),IT(5)

      CALL FTTZLI(N,IT,T)

      END
!************************************************************************
      SUBROUTINE FTTZUF(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      F=1D0/N

      DO I=1,M*N
        X(I,2)=-X(I,2)
      END DO
      CALL FTTZLM(M,N,X,Y,IT,T)
      DO I=1,M*N
        X(I,1)= X(I,1)*F
        X(I,2)=-X(I,2)*F
      END DO

      END
!************************************************************************
      SUBROUTINE FTTZUB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      CALL FTTZLM(M,N,X,Y,IT,T)

      END
!************************************************************************
      SUBROUTINE FTTZLI(N,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION T(0:N-1,2),IT(5)

      IF(N.LE.0) THEN
        CALL FTDMSG('E','FTTZLI','N MUST BE .GT. 0')
      END IF

      J=N
      DO 20 I=5,2,-1
        IT(I)=0
   10   CONTINUE
        K=MOD(J,I)
        IF(K.EQ.0) THEN
          IT(I)=IT(I)+1
          J=J/I
          GO TO 10
        END IF
   20 CONTINUE

      IF(J.NE.1) THEN
        CALL FTDMSG('E','FTTZLI','N.NE.(2**P)*(3**Q)*(5**R)')
      END IF

      IT(1)=MOD(IT(2)+IT(3)+IT(4)+IT(5),2)

      IF(IT(1).EQ.1.AND.IT(4).GE.1) THEN
        IT(2)=IT(2)+2
        IT(4)=IT(4)-1
        IT(1)=0
      END IF

      DO 30 I=0,N-1
        T(I,1)=COS(2*PI*I/N)
        T(I,2)=SIN(2*PI*I/N)
   30 CONTINUE

      END
!************************************************************************
      SUBROUTINE FTTZLM(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      K=N
      L=1
      IP=1

      DO 10 I=1,IT(1)
        CALL FTTZL1(M,N,X,Y)
        IP=-IP
   10 CONTINUE

      DO 20 I=1,IT(2)
        IF(IP.EQ. 1) CALL FTTZL2(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL2(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   20 CONTINUE

      DO 30 I=1,IT(3)
        IF(IP.EQ. 1) CALL FTTZL3(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL3(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   30 CONTINUE

      DO 40 I=1,IT(4)
        IF(IP.EQ. 1) CALL FTTZL4(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL4(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   40 CONTINUE

      DO 50 I=1,IT(5)
        IF(IP.EQ. 1) CALL FTTZL5(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL5(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   50 CONTINUE

      END
!************************************************************************
!*     FT BY FACTOR 1
!*-----------------------------------------------------------------------
      SUBROUTINE FTTZL1(M,N,X,Y)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N*2),Y(M*N*2)

      DO 10 I=1,M*N*2
        Y(I)=X(I)
   10 CONTINUE

      END
!************************************************************************
!*     FT BY FACTOR 2
!*-----------------------------------------------------------------------
      SUBROUTINE FTTZL2(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M*L,0:K/2-1,0:1),B(M*L,0:K/2-1,0:1)
      DIMENSION C(M*L,0:1,0:K/2-1),D(M*L,0:1,0:K/2-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/2-1
        DO 10 I=1,M*L
          C(I,0,J)=A(I,J,0)+A(I,J,1)
          D(I,0,J)=B(I,J,0)+B(I,J,1)
          C(I,1,J)= T(0,1*J,1)*(A(I,J,0)-A(I,J,1))                      &
     &             -T(0,1*J,2)*(B(I,J,0)-B(I,J,1))
          D(I,1,J)= T(0,1*J,1)*(B(I,J,0)-B(I,J,1))                      &
     &             +T(0,1*J,2)*(A(I,J,0)-A(I,J,1))
   10   CONTINUE
   20 CONTINUE

      K=K/2
      L=L*2

      END
!************************************************************************
!*     FT BY FACTOR 3
!*-----------------------------------------------------------------------
!*     C1 = COS(PI/3), S1 = SIN(PI/3)
!*-----------------------------------------------------------------------
      SUBROUTINE FTTZL3(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(C1=0.5D0,S1=0.86602540378443864676D0)
      DIMENSION A(M*L,0:K/3-1,0:2),B(M*L,0:K/3-1,0:2)
      DIMENSION C(M*L,0:2,0:K/3-1),D(M*L,0:2,0:K/3-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/3-1
        DO 10 I=1,M*L
          C(I,0,J)=A(I,J,0)+(A(I,J,1)+A(I,J,2))
          D(I,0,J)=B(I,J,0)+(B(I,J,1)+B(I,J,2))
          C(I,1,J)=                                                     &
     &      T(0,1*J,1)*(                                                &
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))-S1*(B(I,J,1)-B(I,J,2))) &
     &     -T(0,1*J,2)*(                                                &
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))+S1*(A(I,J,1)-A(I,J,2)))
          D(I,1,J)=                                                     &
     &      T(0,1*J,1)*(                                                &
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))+S1*(A(I,J,1)-A(I,J,2))) &
     &     +T(0,1*J,2)*(                                                &
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))-S1*(B(I,J,1)-B(I,J,2)))
          C(I,2,J)=                                                     &
     &      T(0,2*J,1)*(                                                &
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))+S1*(B(I,J,1)-B(I,J,2))) &
     &     -T(0,2*J,2)*(                                                &
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))-S1*(A(I,J,1)-A(I,J,2)))
          D(I,2,J)=                                                     &
     &      T(0,2*J,1)*(                                                &
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))-S1*(A(I,J,1)-A(I,J,2))) &
     &     +T(0,2*J,2)*(                                                &
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))+S1*(B(I,J,1)-B(I,J,2)))
   10   CONTINUE
   20 CONTINUE

      K=K/3
      L=L*3

      END
!************************************************************************
!*     FT BY FACTOR 4
!*-----------------------------------------------------------------------
      SUBROUTINE FTTZL4(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M*L,0:K/4-1,0:3),B(M*L,0:K/4-1,0:3)
      DIMENSION C(M*L,0:3,0:K/4-1),D(M*L,0:3,0:K/4-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/4-1
        DO 10 I=1,M*L
          C(I,0,J)=(A(I,J,0)+A(I,J,2))+(A(I,J,1)+A(I,J,3))
          D(I,0,J)=(B(I,J,0)+B(I,J,2))+(B(I,J,1)+B(I,J,3))
          C(I,2,J)=                                                     &
     &        T(0,2*J,1)*((A(I,J,0)+A(I,J,2))-(A(I,J,1)+A(I,J,3)))      &
     &       -T(0,2*J,2)*((B(I,J,0)+B(I,J,2))-(B(I,J,1)+B(I,J,3)))
          D(I,2,J)=                                                     &
     &        T(0,2*J,1)*((B(I,J,0)+B(I,J,2))-(B(I,J,1)+B(I,J,3)))      &
     &       +T(0,2*J,2)*((A(I,J,0)+A(I,J,2))-(A(I,J,1)+A(I,J,3)))
          C(I,1,J)=                                                     &
     &        T(0,1*J,1)*((A(I,J,0)-A(I,J,2))-(B(I,J,1)-B(I,J,3)))      &
     &       -T(0,1*J,2)*((B(I,J,0)-B(I,J,2))+(A(I,J,1)-A(I,J,3)))
          D(I,1,J)=                                                     &
     &        T(0,1*J,1)*((B(I,J,0)-B(I,J,2))+(A(I,J,1)-A(I,J,3)))      &
     &       +T(0,1*J,2)*((A(I,J,0)-A(I,J,2))-(B(I,J,1)-B(I,J,3)))
          C(I,3,J)=                                                     &
     &        T(0,3*J,1)*((A(I,J,0)-A(I,J,2))+(B(I,J,1)-B(I,J,3)))      &
     &       -T(0,3*J,2)*((B(I,J,0)-B(I,J,2))-(A(I,J,1)-A(I,J,3)))
          D(I,3,J)=                                                     &
     &        T(0,3*J,1)*((B(I,J,0)-B(I,J,2))-(A(I,J,1)-A(I,J,3)))      &
     &       +T(0,3*J,2)*((A(I,J,0)-A(I,J,2))+(B(I,J,1)-B(I,J,3)))
   10   CONTINUE
   20 CONTINUE

      K=K/4
      L=L*4

      END
!************************************************************************
!*     FT BY FACTOR 5
!*-----------------------------------------------------------------------
!*     C1 = COS(PI/5), S1 = SIN(PI/5), C2 = COS(2*PI/5), S2 = SIN(2*PI/5)
!*-----------------------------------------------------------------------
      SUBROUTINE FTTZL5(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(C1=0.80901699437494742410D0,S1=0.58778525229247312917D0)
      PARAMETER(C2=0.30901699437494742410D0,S2=0.95105651629515357212D0)
      DIMENSION A(M*L,0:K/5-1,0:4),B(M*L,0:K/5-1,0:4)
      DIMENSION C(M*L,0:4,0:K/5-1),D(M*L,0:4,0:K/5-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/5-1
        DO 10 I=1,M*L
          C(I,0,J)=A(I,J,0)+(A(I,J,1)+A(I,J,4))+(A(I,J,2)+A(I,J,3))
          D(I,0,J)=B(I,J,0)+(B(I,J,1)+B(I,J,4))+(B(I,J,2)+B(I,J,3))
          C(I,1,J)=                                                     &
     &      T(0,1*J,1)*(                                                &
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))  &
     &                -(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))&
     &     -T(0,1*J,2)*(                                                &
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))  &
     &                +(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))
          D(I,1,J)=                                                     &
     &      T(0,1*J,1)*(                                                &
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))  &
     &                +(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))&
     &     +T(0,1*J,2)*(                                                &
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))  &
     &                -(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))
          C(I,4,J)=                                                     &
     &      T(0,4*J,1)*(                                                &
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))  &
     &                +(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))&
     &     -T(0,4*J,2)*(                                                &
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))  &
     &                -(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))
          D(I,4,J)=                                                     &
     &      T(0,4*J,1)*(                                                &
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))  &
     &                -(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))&
     &     +T(0,4*J,2)*(                                                &
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))  &
     &                +(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))
          C(I,2,J)=                                                     &
     &      T(0,2*J,1)*(                                                &
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))  &
     &                -(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))&
     &     -T(0,2*J,2)*(                                                &
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))  &
     &                +(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))
          D(I,2,J)=                                                     &
     &      T(0,2*J,1)*(                                                &
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))  &
     &                +(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))&
     &     +T(0,2*J,2)*(                                                &
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))  &
     &                -(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))
          C(I,3,J)=                                                     &
     &      T(0,3*J,1)*(                                                &
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))  &
     &                +(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))&
     &     -T(0,3*J,2)*(                                                &
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))  &
     &                -(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))
          D(I,3,J)=                                                     &
     &      T(0,3*J,1)*(                                                &
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))  &
     &                -(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))&
     &     +T(0,3*J,2)*(                                                &
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))  &
     &                +(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))
   10   CONTINUE
   20 CONTINUE

      K=K/5
      L=L*5

      END
