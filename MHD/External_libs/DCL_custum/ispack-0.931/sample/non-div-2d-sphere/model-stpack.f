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
*     MODEL TO STUDY SATURATION OF SHEAR INSTABILITY         1999/03/24
*            LEGENDRE POLYNOMIALS FLOWS AND NEW TYPE DISTURBANCE
*            BY RUNGE-KUTTA METHOD
***********************************************************************
      PROGRAM MODEL

      IMPLICIT REAL*8(A-H,O-Z)

      CALL SAMAIN(3,-0.07D0, 11,'output-st.dat')

      END
************************************************************************
*     TEST MODEL FOR NEW ISPACK
************************************************************************
      SUBROUTINE SAMAIN(IPAT,APSI,IU,CF)

*     IPAT: 基本流をP_nモードとして与える場合のn
*     APSI: P_nモードの振幅
*     IU: 出力ファイルの装置番号
*     CF: 出力ファイル名
 
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CF*(*)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(MM=21)
      PARAMETER(LM=(MM+1)*(MM+1))
      PARAMETER(NB=23476)
      DIMENSION AVT(LM),W(LM*3)
      EXTERNAL SBDAVT,SBDISS

*---- OPEN SUBROUTINE PACKAGE AND INITIALIZE VARIABLES -----------------

      LEV=10                    !高階粘性項のラプラシアンの階数
      ITM=30                    !時間発展するステップ数
      NDV=20                    !Runge-Kuttaでのステップ分割数
      DT=1D0                    !ファイル出力の時間間隔

      DNU=10D0/(1D0*(MM*(MM+1)-2))**LEV

*/ ガウシアン型の擾乱のパラメター /*

      DA=1D-3     !擾乱の振幅
      DB=100D0    !擾乱の幅の逆数のルートに相当するパラメター
      X0=0        !擾乱の中心位置のλ座標
      Y0=PI/4     !擾乱の中心位置のφ座標

      CALL SBOPEN(LEV,DNU,NDV,DT)
      CALL SBINIT(AVT,IPAT,APSI,DA,DB,X0,Y0)

*---- OPEN FHPACK ------------------------------------------------------

      CALL FHUOPN(IU,CF,'W',NB)

*---- TIME EVOLUTION BY RUNGE-KUTTA METHOD -----------------------------

      I=0
      TIM=0
      CALL FEPUTS(IU,LM,AVT)
      CALL SBCHCK(I,AVT)

      DO I=1,ITM
        CALL TDRKNU(LM,NDV,DT,TIM,AVT,W,SBDISS,SBDAVT)
        CALL FEPUTS(IU,LM,AVT)
        CALL SBCHCK(I,AVT)
      END DO

*---- CLOSE FHPACK -----------------------------------------------------

      CALL FHUCLS(IU)

      END
************************************************************************
*     OPEN SUBROUTINE PACKAGE
************************************************************************
      SUBROUTINE SBOPEN(LEV,DNU,NDV,DT)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=21,JM=32,IM=64,LM=(MM+1)*(MM+1))
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(OMG=1)
      DIMENSION Q(JM*(MM+1)),R((MM+1)*(MM+1)),IT(5),T(IM*2)
      DIMENSION PSI(LM),AVT(LM),DAVT(LM)
      DIMENSION X(IM),Y(JM)
      DIMENSION P(JM*IM,4),DIS(MM+1)
      SAVE

      CALL STINIT(MM,JM,IM,Q,R,IT,T)
      CALL STOGRD(JM,IM,Y,X,Q)
      CALL NDDISI(MM,LEV,DNU*DT/(2*NDV),DIS)

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF d(AVT)/dt
*-----------------------------------------------------------------------
      ENTRY SBDAVT(TIM,AVT,DAVT)

      CALL NDCA2P(MM,OMG,AVT,PSI)
      CALL STAJBA(MM,JM,IM,PSI,AVT,DAVT,P,Q,R,IT,T)

      DO L=1,LM
        DAVT(L)=-2*PI*DAVT(L)
      END DO

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF THE EFFECT OF VISCOSITY
*-----------------------------------------------------------------------
      ENTRY SBDISS(TIM,DTIM,AVT)

      CALL NDDISA(MM,AVT,DIS)

      RETURN
*-----------------------------------------------------------------------
*     CHECK ENERGY AND ENSTROPHY CONSERVATION
*-----------------------------------------------------------------------
      ENTRY SBCHCK(I,AVT)

      CALL NDCA2P(MM,OMG,AVT,PSI)

      CALL NDGEEA(MM,PSI,ENE)
      CALL NDGENA(MM,PSI,ENS)
      WRITE(6,'(I5,2F15.10)') I,ENE,ENS

      RETURN
*-----------------------------------------------------------------------
*     INITIALIZATION OF AVT
*-----------------------------------------------------------------------
      ENTRY SBINIT(AVT,IPAT,APSI,DA,DB,X0,Y0)

*     / BASIC FIELD /

      N=IPAT
      CALL BSSET0(LM,PSI)
      PSI(N+1)=-N*(N+1)*APSI
      CALL NDTV2A(OMG,PSI)

*     / DISTURBANCE FIELD /

      CALL SCINID(JM,IM,P(1,2),X,Y,DA,DB,X0,Y0)
      CALL STG2SA(MM,JM,IM,P(1,2),AVT,P,Q,R,IT,T)

*     / BASIC FIELD + DISTURBANCE FIELD /

      DO L=1,MM+1
        AVT(L)=AVT(L)+PSI(L)
      END DO

      END
************************************************************************
*     CALCULATE DISTURBANCE FIELD
************************************************************************
      SUBROUTINE SCINID(JM,IM,G,X,Y,DA,DB,X0,Y0)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION G(JM,IM)
      DIMENSION Y(JM),X(IM)

      CB=COS(PI/2-Y0)
      SB=SIN(PI/2-Y0)

      DO I=1,IM
        CA=COS(X(I)-X0)
        DO J=1,JM
          CC=COS(PI/2-Y(J))
          SC=SIN(PI/2-Y(J))
          CR=CB*CC+SB*SC*CA
          G(J,I)=DA*(EXP(DB*(CR-1))-(1-EXP(-2*DB))/(2*DB))
        END DO
      END DO

      END
************************************************************************
*     SUBROUTINES FOR CONVERTING                                95/10/13
************************************************************************
*     CHANGE PSI TO AVT
************************************************************************
      SUBROUTINE NDCP2A(MM,OMG,PSI,AVT)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION PSI((MM+1)*(MM+1)),AVT((MM+1)*(MM+1))

      CALL STCLFA(MM,PSI,AVT)

      AVT(2)=AVT(2)+2*OMG/SQRT3

      END
************************************************************************
*     CHANGE AVT TO PSI
************************************************************************
      SUBROUTINE NDCA2P(MM,OMG,AVT,PSI)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1)),PSI((MM+1)*(MM+1))

      CALL STCLBA(MM,AVT,PSI)

      PSI(2)=PSI(2)+OMG/SQRT3

      END
************************************************************************
*     SUBROUTINES FOR DISSIPATION TERM                          95/10/27
************************************************************************
      SUBROUTINE NDDISI(MM,LEV,DTDNU,D)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(0:MM)

      D(0)=1
      DO N=1,MM
        D(N)=EXP(-DTDNU*(1D0*N*(N+1)-2)**LEV)
      END DO

      END
************************************************************************
*     LOWER ROUTINES
************************************************************************
      SUBROUTINE NDDISA(MM,A,D)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION D(MM+1)

      DO M=1,MM
        CALL LTLMML(MM,M,L)
        CALL NDDISW(MM,M,A(L),D)
      END DO

      CALL NDDISZ(MM,A,D)

      END
************************************************************************
      SUBROUTINE NDDISW(MM,M,A,D)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M:MM,2)
      DIMENSION D(0:MM)

      DO N=M,MM
        A(N,1)=D(N)*A(N,1)
        A(N,2)=D(N)*A(N,2)
      END DO

      END
************************************************************************
      SUBROUTINE NDDISZ(MM,A,D)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(0:MM)
      DIMENSION D(0:MM)

      DO N=0,MM
        A(N)=D(N)*A(N)
      END DO

      END
************************************************************************
*     CALCULATE ENERGY
************************************************************************
      SUBROUTINE NDGEEA(MM,PSI,ENE)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PSI((MM+1)*(MM+1))

      CALL NDGEEZ(MM,PSI,ENE)
      
      DO M=1,MM
        CALL STNM2L(MM,M,M,L)
        CALL NDGEEW(MM,M,PSI(L),ENEW)
        ENE=ENE+ENEW
      END DO

      END
************************************************************************
      SUBROUTINE NDGEEZ(MM,PSI,ENE)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PSI(0:MM)

      ENE=0
      DO N=0,MM
        ENE=ENE+PSI(N)*PSI(N)*N*(N+1)
      END DO
      ENE=0.5D0*ENE

      END
************************************************************************
      SUBROUTINE NDGEEW(MM,M,PSI,ENE)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PSI(M:MM,2)

      ENE=0
      DO N=M,MM
        ENE=ENE+(PSI(N,1)*PSI(N,1)+PSI(N,2)*PSI(N,2))*N*(N+1)
      END DO

      END
************************************************************************
*     CALCULATE ENSTROPHY
************************************************************************
      SUBROUTINE NDGENA(MM,PSI,ENS)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PSI((MM+1)*(MM+1))

      CALL NDGENZ(MM,PSI,ENS)
      
      DO M=1,MM
        CALL STNM2L(MM,M,M,L)
        CALL NDGENW(MM,M,PSI(L),ENSW)
        ENS=ENS+ENSW
      END DO

      END
************************************************************************
      SUBROUTINE NDGENZ(MM,PSI,ENS)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PSI(0:MM)

      ENS=0
      DO N=0,MM
        ENS=ENS+PSI(N)*PSI(N)*(N*(N+1))*(N*(N+1))
      END DO
      ENS=0.5D0*ENS

      END
************************************************************************
      SUBROUTINE NDGENW(MM,M,PSI,ENS)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PSI(M:MM,2)

      ENS=0
      DO N=M,MM
        ENS=ENS
     &      +(PSI(N,1)*PSI(N,1)+PSI(N,2)*PSI(N,2))*(N*(N+1))*(N*(N+1))
      END DO

      END
************************************************************************
*     SUBROUTINES FOR TRANSLATING                               95/10/31
************************************************************************
*     TRANSLATE VRT TO AVT
************************************************************************
      SUBROUTINE NDTV2A(OMG,VRT)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION VRT(2)

      VRT(2)=VRT(2)+2*OMG/SQRT3

      END
************************************************************************
*     TRANSLATE VRT TO AVT
************************************************************************
      SUBROUTINE NDTA2V(OMG,VRT)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION VRT(2)

      VRT(2)=VRT(2)-2*OMG/SQRT3

      END
************************************************************************
*     SUBROUTINES FOR VISCOSITY TERM                            95/10/23
************************************************************************
*     NORMAL VISCOSITY (1)
************************************************************************
      SUBROUTINE NDVIS1(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1)),B((MM+1)*(MM+1))

      DO N=0,MM
        B(N+1)=-(N*(N+1)-2)
      END DO

      CALL STCLLA(MM,A,B)

      END
************************************************************************
*     LOWER ROUTINES
************************************************************************
      SUBROUTINE NDVISA(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION B((MM+1)*(MM+1))

      DO M=1,MM
        CALL LTLMML(MM,M,L)
        CALL NDVISW(MM,M,A(L),B(L),B)
      END DO

      CALL NDVISZ(MM,A,B)

      END
************************************************************************
      SUBROUTINE NDVISW(MM,M,A,B,W)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M:MM,2)
      DIMENSION B(M:MM,2)
      DIMENSION W(0:MM)

      DO N=M,MM
        B(N,1)=W(N)*A(N,1)
        B(N,2)=W(N)*A(N,2)
      END DO

      END
************************************************************************
      SUBROUTINE NDVISZ(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(0:MM)
      DIMENSION B(0:MM)

      DO N=0,MM
        B(N)=B(N)*A(N)
      END DO

      END
