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
***********************************************************************
      SUBROUTINE DKGAUS(JM,X,W)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(NB=64)
      DIMENSION X(JM),W(JM),E(NB)

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
      
      DO J=1,JM/2
        Z=COS(PI/2*(4*J-1)/(2*JM+1))        
        IFLAG=0
   20   CONTINUE
          P0=0
          P1=1
          DO N=1,JM
            P=P1
            P1=((2*N-1)*Z*P1-(N-1)*P0)/N
            P0=P
          END DO
          DP=JM*(P0-Z*P1)/(1-Z*Z)
          DZ=P1/DP
          Z=Z-DZ
        IF(IFLAG.EQ.0) THEN
          IF(ABS(DZ/Z).LE.EPS) THEN
            IFLAG=1
            X(J)=Z
          END IF
          GOTO 20
        END IF
        W(J)=1/(DP*DP)/(1-X(J)*X(J))
      END DO

      IF(MOD(JM,2).EQ.1) THEN
        J=(JM+1)/2
        P=1
        DO N=2,JM-1,2
          P=-P*(N-1)/N
        END DO
        DP=JM*P
        X(J)=0
        W(J)=1/(DP*DP)
      END IF

      DO J=(JM+1)/2+1,JM
        X(J)=-X(JM-J+1)
        W(J)=W(JM-J+1)
      END DO

      DO J=1,JM
        X(J)=(1-X(J))/2
      END DO

      END
