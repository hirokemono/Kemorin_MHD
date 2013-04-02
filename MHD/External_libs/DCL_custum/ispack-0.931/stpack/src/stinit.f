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
*     INITIALIZATION OF STPACK                                  95/10/03
************************************************************************
      SUBROUTINE STINIT(MM,JM,IM,Q,R,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Q(JM*(MM+1)),R((MM+1)*(MM+1)),IT(5),T(IM*2)

      CALL FTTRUI(IM,IT,T)
      CALL LTINIT(MM,JM,Q,R)

      END
************************************************************************
*     INITIALIZATION OF LTPACK
*-----------------------------------------------------------------------
*     Q(J,1,0): w_j/2
*     Q(J,2,0): 1/sqrt(1-{\mu_j}^2)
*     Q(J,1,M): P^m_m(\mu_j) (M=1,...,MM)
*     Q(J,2,M): m\mu_j       (M=1,...,MM)
*     R: COEFFICIENTS FOR RECURRENCE FORMULA
************************************************************************
      SUBROUTINE LTINIT(MM,JM,Q,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0,EPS=1D-16)
      DIMENSION Q(JM/2,2,0:MM),R(0:MM,0:MM)

      IF(MOD(JM,2).NE.0) THEN
        CALL BSDMSG('E','LTGAUS','JM MUST BE EVEN.')
      END IF

      JH=JM/2

*/ CALCULATION OF GAUSSIAN LATITUDE AND GAUSSIAN WEIGHT /*

      DO J=1,JH
        X=SIN(PI*(2*J-1)/(2*JM+1))
   10   CONTINUE
          P0=0
          P1=1
          DO N=1,JM-1,2
            P0=((2*N-1)*X*P1-(N-1)*P0)/N
            P1=((2*N+1)*X*P0-N*P1)/(N+1)
          END DO
          DP=JM*(P0-X*P1)/(1-X*X)
          DX=P1/DP
          X=X-DX
        IF(ABS(DX).GT.EPS) GOTO 10
        Q(J,1,0)=1/((JM*P0)*(JM*P0))*(1-X*X)
        Q(J,2,0)=1/SQRT(1-X*X)
        Q(J,1,1)=X
        Q(J,2,1)=SQRT(1.5D0)*SQRT(1-X*X)
      END DO

*/ CALCULATION OF COEFFICIENTS FOR RECURRNCE FORMULA /*

      DO M=0,MM
        R(M,M)=-M*(M+1)
        DO N=M+1,MM
          R(N,M)=SQRT((2*N+1)/(1D0*(2*N-1)*(N+M)*(N-M)))
          R(M,N)=SQRT(1D0*(2*N+1)*(N+M)*(N-M)/(2*N-1))
        END DO
      END DO

*/ CALCULATION OF FUNCTION FOR RECURRENCE FORMULA /*

      DO M=2,MM
        A=SQRT(1D0*(2*M+1)/(2*M))/SQRT(1.5D0)
        DO J=1,JH
          Q(J,1,M)=M*Q(J,1,1)
          Q(J,2,M)=A*Q(J,2,M-1)*Q(J,2,1)
        END DO
      END DO

      END
