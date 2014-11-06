!
      module ispack_lag
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: IP(:,:)
      real(kind = kreal), allocatable :: P_ispack(:,:,:,:)
      real(kind = kreal), allocatable :: X_ispack(:),Y_ispack(:,:),W(:)
      real(kind = kreal), allocatable :: QSG(:),QGS(:)
      real(kind = kreal), allocatable :: QSV(:),QVS(:)
      real(kind = kreal), allocatable :: QSU(:),QUS(:)
      real(kind = kreal), allocatable :: R(:,:,:,:)
      integer(kind = kint) , allocatable :: ML(:,:),NL(:,:)
      real(kind = kreal), allocatable :: RF(:,:),RB(:,:)
!
!*-----------------------------------------------------------------------
!
      contains
!
!*-----------------------------------------------------------------------
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
!*     INITIALIZATION OF SMPACK                                  98/02/13
!************************************************************************
!*     X_ispack(I): \lambda_i
!*     Y_ispack(J): \phi_j
!*     IP: parity coefficients
!*     P_ispack(K,L1,L2,J): P^m_n(\mu_j); (L2=n,L1=MM-n+m)
!*     P_ispack(K,L1,MM+1,J): DP^m_n(\mu_j); (n=MM,L1=m)
!*     P_ispack(K,L1,MM+2,J): DP^m_n(\mu_j); (n=MM,L1=MM-m)
!*     QSG(J): 1
!*     QGS(J): w_j/2
!*     QSV(J): 1/cos(\phi_j)
!*     QVS(J): (w_j/2)*(1/cos(\phi_j))
!*     QSU(J): 1
!*     QUS(J): (w_j/2)*(1/cos(\phi_j))*(1/cos(\phi_j))
!*     R: coefficients for recurrnce formula
!*     RF(L1,L2): -n*(n+1); (L2=n,L1=MM-n-m)
!*     RB(L1,L2): -1/(n*(n+1)); (L2=n,L1=MM-n-m)
!*     ML(L1,L2): m; (L2=n,L1=MM-n-m)
!*     NL(L1,L2): n; (L2=n,L1=MM-n-m)
!************************************************************************
!
!*-----------------------------------------------------------------------
!
      SUBROUTINE SMFIN
!
      deallocate( IP )
      deallocate( P_ispack, X_ispack, Y_ispack, W )
      deallocate( QSG, QGS, QSV, QVS, QSU, QUS )
      deallocate( R, ML, NL, RF, RB )
!
      end SUBROUTINE SMFIN
!
!*-----------------------------------------------------------------------
!
      SUBROUTINE SMINIT(MM,IM,JM,KM)
!
      integer(kind = kint), intent(in) :: MM, IM, JM, KM
!
      integer(kind = kint) :: JH, K, J, M, N, L, I
      real(kind = kreal) :: RD, A, B
      real(kind = kreal), parameter :: PI = 3.1415926535897932385D0
!
!*/ CHECK JM /*

      IF(MOD(JM,2).NE.0) THEN
        CALL BSDMSG('E','SKINIT','JM MUST BE EVEN.')
      END IF

      JH=JM/2

      allocate( IP(KM,-MM:MM) )
      allocate( P_ispack(KM,0:MM,0:MM+2,JM/2) )
      allocate( X_ispack(0:IM-1),Y_ispack(JM/2,2),W(JM) )
      allocate( QSG(JM/2),QGS(JM/2) )
      allocate( QSV(JM/2),QVS(JM/2) )
      allocate( QSU(JM/2),QUS(JM/2) )
      allocate( R(KM,MM,MM-1,2) )
      allocate( ML(0:MM,0:MM),NL(0:MM,0:MM) )
      allocate( RF(0:MM,0:MM),RB(0:MM,0:MM) )
!
!*/ INITIALIZATION OF SPHERICAL HARMONICS /*

!*/ CALCULATION OF GAUSSIAN LATITUDE AND GAUSSIAN WEIGHT /*

      CALL SMGAUS(JM,Y_ispack(1,1),QGS)

      DO J=1,JH
        Y_ispack(J,2)=SQRT(1-Y_ispack(J,1)*Y_ispack(J,1))
        QSG(J)=1
        QSV(J)=1/Y_ispack(J,2)
        QSU(J)=1
        QVS(J)=QGS(J)*QSV(J)
        QUS(J)=QVS(J)*QSV(J)
      END DO

!*/ CALCULATION OF ASSOCIATED LEGENDRE FUNCTIONS /*

      K=1

      DO J=1,JH
        P_ispack(K,MM,0,J)=1
      END DO

      DO M=1,MM
        A=SQRT(1D0*(2*M+1)/(2*M))
        DO J=1,JH
          P_ispack(K,MM,M,J)=A*P_ispack(K,MM,M-1,J)*Y_ispack(J,2)
        END DO
      END DO

      DO M=0,MM-1
        A=SQRT(1D0*(2*M+3))
        DO J=1,JH
          P_ispack(K,MM-1,M+1,J)=A*P_ispack(K,MM,M,J)*Y_ispack(J,1)
        END DO
      END DO

      DO N=2,MM
        DO M=0,N-2
          A=SQRT((4D0*N*N-1)/(1D0*N*N-1D0*M*M))
          B=SQRT((1D0*(N-1)*(N-1)-1D0*M*M)/(4D0*(N-1)*(N-1)-1))
          L=MM-N+M
          DO J=1,JH
            P_ispack(K,L,N,J)=A*(Y_ispack(J,1)*P_ispack(K,L+1,N-1,J)    &
     &                       -B*P_ispack(K,L+2,N-2,J))
          END DO
        END DO
      END DO

      N=MM
      DO M=0,MM-1
        A=SQRT(1D0*(2*N+1)*(N+M)*(N-M)/(2*N-1))
        DO J=1,JH
          P_ispack(K,M,MM+1,J)=A*P_ispack(K,M+1,N-1,J)                  &
     &                        -N*Y_ispack(J,1)*P_ispack(K,M,N,J)
        END DO
      END DO

      DO J=1,JH
        P_ispack(K,MM,MM+1,J)=-N*Y_ispack(J,1)*P_ispack(K,MM,N,J)
      END DO

      DO M=1,MM
        DO J=1,JH
          P_ispack(K,MM-M,MM+2,J)=P_ispack(K,M,MM+1,J)
        END DO
      END DO

      DO J=1,JH
        P_ispack(K,MM,MM+2,J)=0
      END DO

      DO N=1,MM
        DO M=1,N
          DO J=1,JH
            P_ispack(K,N-M,MM-N,J)=P_ispack(K,MM-N+M,N,J)
          END DO
        END DO
      END DO

      DO J=1,JH
        DO N=0,MM+2
          DO M=0,MM
            DO K=2,KM
              P_ispack(K,M,N,J)=P_ispack(1,M,N,J)
            END DO
          END DO
        END DO
      END DO

!*/ CALCULATION OF GRID POINTS /*

      DO I=0,IM-1
        X_ispack(I)=2*PI*I/IM
      END DO

      DO J=1,JH
        Y_ispack(J,2)= -ASIN(Y_ispack(J,1))

      END DO

      DO J=1,JH
        Y_ispack(J,1)=-Y_ispack(JH-J+1,2)
      END DO

      DO J=1,JH
        W(J)=QGS(JH-J+1)
        W(JH+J)=QGS(J)
      END DO

!*/ CALCULATION OF SOME COEFFICIENTS /*

      DO K=1,KM
        DO M=-MM,MM
          IP(K,M)=1
        END DO
        DO M=1,MM,2
          IP(K,M)=-1
        END DO
        DO M=-MM+1,-1,2
          IP(K,M)=-1
        END DO
      END DO

      RF(MM,0)=0
      RB(MM,0)=1

      DO N=1,MM
        DO M=0,N
          L=MM-N+M
          RF(L,N)=-1D0*N*(N+1)
          RB(L,N)=-1/(1D0*N*(N+1))
        END DO
      END DO

      DO N=1,MM
        DO M=1,N
          RF(N-M,MM-N)=RF(MM-N+M,N)
          RB(N-M,MM-N)=RB(MM-N+M,N)
        END DO
      END DO

      DO N=0,MM
        DO M=0,N
          L=MM-N+M
          ML(L,N)=M
        END DO
        DO M=-MM+N,-1
          L=MM-N+M
          ML(L,N)=M
        END DO
      END DO

      DO N=0,MM
        DO M=0,N
          L=MM-N+M
          NL(L,N)=N
        END DO
        DO M=-MM+N,-1
          L=MM-N+M
          NL(L,N)=MM-N
        END DO
      END DO

      DO J=1,MM-1
        N=J
        DO M=0,N
          L=MM-N+M
          RD=-N*SQRT((1D0*(N+1)*(N+1)-1D0*M*M)/(4D0*(N+1)*(N+1)-1))
          DO K=1,KM
            R(K,L,J,1)=RD
          END DO
        END DO
        N=MM-J
        DO M=-N+1,-1
          L=N+M
          RD=(N+1)*SQRT((1D0*N*N-1D0*M*M)/(4D0*N*N-1))
          DO K=1,KM
            R(K,L,J,1)=RD
          END DO
        END DO
      END DO

      DO J=1,MM-1
        N=J
        DO M=0,N-1
          L=MM-N+M+1
          RD=(N+1)*SQRT((1D0*N*N-1D0*M*M)/(4D0*N*N-1))
          DO K=1,KM
            R(K,L,J,2)=RD
          END DO
        END DO
        N=MM-J
        DO M=-N,-1
          L=N+M+1
          RD=-N*SQRT((1D0*(N+1)*(N+1)-1D0*M*M)/(4D0*(N+1)*(N+1)-1))
          DO K=1,KM
            R(K,L,J,2)=RD
          END DO
        END DO
      END DO

      END SUBROUTINE SMINIT
!*
!*
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
!***********************************************************************
!*     CALCULATE GAUSSIAN LATITUDES AND WEIGHTS                 98/02/13
!***********************************************************************
!*     X_gauss(J): sin(\phi_j)
!*     W_gauss(J): w_j/2
!***********************************************************************
      SUBROUTINE SMGAUS(JM,X_gauss,W_gauss)

      integer(kind = kint), intent(in) :: JM
      real(kind = kreal), intent(inout) :: X_gauss(JM/2),W_gauss(JM/2)
!
      integer(kind = kint), parameter :: NB = 64
      real(kind = kreal) :: E(NB)
      real(kind = kreal), parameter :: PI = 3.1415926535897932385D0
!
      integer(kind = kint) :: JH, I, J, N, IFLAG
      real(kind = kreal) :: P0, P1, EPS, Z, DZ, DP
!
!
      JH=JM/2

      EPS=1
      DO I=1,NB
        EPS=EPS/2
        E(I)=EPS+1
      END DO

      I=0
      EPS=1
   10 CONTINUE
        I=I+1
        EPS=EPS/2
      IF(E(I).GT.1) GOTO 10

      EPS=EPS*16
      
      DO J=1,JH
        Z=SIN(PI*(2*J-1)/(2*JM+1))
        IFLAG=0
   20   CONTINUE
          P0=0
          P1=1
          DO N=1,JM-1,2
            P0=(dble(2*N-1)*Z*P1-(N-1)*P0)/N
            P1=(dble(2*N+1)*Z*P0-N*P1)/(N+1)
          END DO
          DP=JM*(P0-Z*P1)/(1-Z*Z)
          DZ=P1/DP
          Z=Z-DZ
        IF(IFLAG.EQ.0) THEN
          IF(ABS(DZ/Z).LE.EPS) THEN
            IFLAG=1
            X_gauss(J)=Z
          END IF
          GOTO 20
        END IF
        W_gauss(J)=1/(DP*DP)/(1-X_gauss(J)*X_gauss(J))
      END DO

      END SUBROUTINE SMGAUS
!*
!*-----------------------------------------------------------------------
!
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
!*     DUMP MESSAGES
!*-----------------------------------------------------------------------
      SUBROUTINE BSDMSG(CL,CS,CM)
!
      CHARACTER CL*1,CS*(*),CM*(*)
!
      CHARACTER CSD*6,CMD*53
      integer(kind = kint), SAVE ::  MMSG = 20,  IMSG =0
!
      CSD=CS
      CMD=CM
!
      IF(CL.EQ.'E') THEN
        WRITE(6,'(A)') '***** ERROR ('//CSD//') ***  '//CMD
        STOP
      END IF
!
      IF(IMSG.LT.MMSG) THEN
        IF(CL.EQ.'W') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** WARNING ('//CSD//') ***  '//CMD
        ELSE IF(CL.EQ.'M') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** MESSAGE ('//CSD//') ***  '//CMD
        END IF
        IF(IMSG.EQ.MMSG) THEN
          WRITE(*,*) '+++ THE FOLLOWING MESSAGES ARE SUPRRESSED.'
        END IF
      END IF
!
      END SUBROUTINE BSDMSG
!
!*-----------------------------------------------------------------------
!
      end module ispack_lag
