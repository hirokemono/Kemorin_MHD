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
*     INITIALIZATION OF SNPACK-MPI                            2002/05/16
************************************************************************
      SUBROUTINE SNMINI(MM,IM,JM,JC,IT,T,Y,IP,P,R,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(*)
      DIMENSION IP(-(MM+1)/2:MM,2)
      DIMENSION P(-(MM+1)/2:MM,2,*)
      DIMENSION R(-(MM+1)/2:(MM+1)/2+2,0:MM/2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      IF(MOD(JM,2).NE.0) THEN
        CALL BSDMSG('E','SNMINI','JM MUST BE EVEN.')
      END IF

      JH=JM/2

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)
      
      JPH=(JH-1)/NP+1
      JS=JPH*IPROC+1
      JE=MIN(JPH*(IPROC+1),JH)
      IF(JE.GE.JS) THEN
        JCH=JE-JS+1
        JC=JCH*2
      ELSE
        JC=0
        JS=1
        JE=1
      END IF

*/ FFTに使う配列の初期化 /*

      CALL SNINI1(IM,IT,T)

*/ ガウス緯度およびウェイトの初期化 および/*
*/ Legendre陪関数計算のための係数および初期値の設定 /*

      IF(JC.GT.0) THEN
        CALL SNMGAU(JM,JC,JS,Y,Y(JCH+1),Y(2*JCH+1),Y(3*JCH+1))
        CALL SNINI3(MM,JC,Y,IP,P,R,IA,A)
      END IF

      END
***********************************************************************
*     CALCULATE GAUSSIAN LATITUDES AND WEIGHTS               2002/05/16
***********************************************************************
*     X(J): sin(\phi_j)
*     W(J): w_j/2
***********************************************************************
      SUBROUTINE SNMGAU(JM,JC,JS,X,W,Y3,Y4)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(NB=64)
      DIMENSION X(JC/2),W(JC/2),Y3(JC/2),Y4(JC/2),E(NB)

      JCH=JC/2

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
      
      DO J=1,JCH
        Z=SIN(PI*(2*(JS+J-1)-1)/(2*JM+1))
        IFLAG=0
   20   CONTINUE
          P0=0
          P1=1
          DO N=1,JM-1,2
            P0=((2*N-1)*Z*P1-(N-1)*P0)/N
            P1=((2*N+1)*Z*P0-N*P1)/(N+1)
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

      DO J=1,JCH
        Y3(J)=SQRT(1-X(J)*X(J))
        Y4(J)=1/Y3(J)
      END DO

      END
