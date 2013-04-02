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
*     INITIALIZATION OF SMPACK                                  98/02/13
************************************************************************
*     X(I): \lambda_i
*     Y(J): \phi_j
*     IP: parity coefficients
*     P(K,L1,L2,J): P^m_n(\mu_j); (L2=n,L1=MM-n+m)
*     P(K,L1,MM+1,J): DP^m_n(\mu_j); (n=MM,L1=m)
*     P(K,L1,MM+2,J): DP^m_n(\mu_j); (n=MM,L1=MM-m)
*     QSG(J): 1
*     QGS(J): w_j/2
*     QSV(J): 1/cos(\phi_j)
*     QVS(J): (w_j/2)*(1/cos(\phi_j))
*     QSU(J): 1
*     QUS(J): (w_j/2)*(1/cos(\phi_j))*(1/cos(\phi_j))
*     R: coefficients for recurrnce formula
*     RF(L1,L2): -n*(n+1); (L2=n,L1=MM-n-m)
*     RB(L1,L2): -1/(n*(n+1)); (L2=n,L1=MM-n-m)
*     ML(L1,L2): m; (L2=n,L1=MM-n-m)
*     NL(L1,L2): n; (L2=n,L1=MM-n-m)
************************************************************************
      SUBROUTINE SMINIT(MM,IM,JM,KM,X,Y,W,IT,T,IP,P,
     &                  QSG,QGS,QSV,QVS,QSU,QUS,R,ML,NL,RF,RB)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM,-MM:MM)
      DIMENSION P(KM,0:MM,0:MM+2,JM/2)
      DIMENSION X(0:IM-1),Y(JM/2,2),W(JM)
      DIMENSION QSG(JM/2),QGS(JM/2)
      DIMENSION QSV(JM/2),QVS(JM/2)
      DIMENSION QSU(JM/2),QUS(JM/2)
      DIMENSION R(KM,MM,MM-1,2)
      DIMENSION ML(0:MM,0:MM),NL(0:MM,0:MM)
      DIMENSION RF(0:MM,0:MM),RB(0:MM,0:MM)

*/ CHECK JM /*

      IF(MOD(JM,2).NE.0) THEN
        CALL BSDMSG('E','SKINIT','JM MUST BE EVEN.')
      END IF

      JH=JM/2

*/ INITIALIZATION OF FTPACK /*

      CALL FTTRUI(IM,IT,T)

*/ INITIALIZATION OF SPHERICAL HARMONICS /*

*/ CALCULATION OF GAUSSIAN LATITUDE AND GAUSSIAN WEIGHT /*

      CALL SMGAUS(JM,Y(1,1),QGS)

      DO J=1,JH
        Y(J,2)=SQRT(1-Y(J,1)*Y(J,1))
        QSG(J)=1
        QSV(J)=1/Y(J,2)
        QSU(J)=1
        QVS(J)=QGS(J)*QSV(J)
        QUS(J)=QVS(J)*QSV(J)
      END DO

*/ CALCULATION OF ASSOCIATED LEGENDRE FUNCTIONS /*

      K=1

      DO J=1,JH
        P(K,MM,0,J)=1
      END DO

      DO M=1,MM
        A=SQRT(1D0*(2*M+1)/(2*M))
        DO J=1,JH
          P(K,MM,M,J)=A*P(K,MM,M-1,J)*Y(J,2)
        END DO
      END DO

      DO M=0,MM-1
        A=SQRT(1D0*(2*M+3))
        DO J=1,JH
          P(K,MM-1,M+1,J)=A*P(K,MM,M,J)*Y(J,1)
        END DO
      END DO

      DO N=2,MM
        DO M=0,N-2
          A=SQRT((4D0*N*N-1)/(1D0*N*N-1D0*M*M))
          B=SQRT((1D0*(N-1)*(N-1)-1D0*M*M)/(4D0*(N-1)*(N-1)-1))
          L=MM-N+M
          DO J=1,JH
            P(K,L,N,J)=A*(Y(J,1)*P(K,L+1,N-1,J)-B*P(K,L+2,N-2,J))
          END DO
        END DO
      END DO

      N=MM
      DO M=0,MM-1
        A=SQRT(1D0*(2*N+1)*(N+M)*(N-M)/(2*N-1))
        DO J=1,JH
          P(K,M,MM+1,J)=A*P(K,M+1,N-1,J)-N*Y(J,1)*P(K,M,N,J)
        END DO
      END DO

      DO J=1,JH
        P(K,MM,MM+1,J)=-N*Y(J,1)*P(K,MM,N,J)
      END DO

      DO M=1,MM
        DO J=1,JH
          P(K,MM-M,MM+2,J)=P(K,M,MM+1,J)
        END DO
      END DO

      DO J=1,JH
        P(K,MM,MM+2,J)=0
      END DO

      DO N=1,MM
        DO M=1,N
          DO J=1,JH
            P(K,N-M,MM-N,J)=P(K,MM-N+M,N,J)
          END DO
        END DO
      END DO

      DO J=1,JH
        DO N=0,MM+2
          DO M=0,MM
            DO K=2,KM
              P(K,M,N,J)=P(1,M,N,J)
            END DO
          END DO
        END DO
      END DO

*/ CALCULATION OF GRID POINTS /*

      DO I=0,IM-1
        X(I)=2*PI*I/IM
      END DO

      DO J=1,JH
        Y(J,2)= -ASIN(Y(J,1))

      END DO

      DO J=1,JH
        Y(J,1)=-Y(JH-J+1,2)
      END DO

      DO J=1,JH
        W(J)=QGS(JH-J+1)
        W(JH+J)=QGS(J)
      END DO

*/ CALCULATION OF SOME COEFFICIENTS /*

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

      END
