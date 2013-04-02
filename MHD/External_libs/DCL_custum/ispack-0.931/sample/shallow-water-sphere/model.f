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
*     浅水方程式をsppackで解くためのサンプルプログラム       1999/03/29
***********************************************************************
      PROGRAM MODEL

      IMPLICIT REAL*8(A-H,O-Z)

      CALL SAMAIN(11,'output.dat')

      END
************************************************************************
*     TEST MODEL FOR NEW ISPACK
************************************************************************
      SUBROUTINE SAMAIN(IU,CF)

*     IU: 出力ファイルの装置番号
*     CF: 出力ファイル名
 
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CF*(*)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(MM=21)
      PARAMETER(LM=(MM+1)*(MM+1))
      PARAMETER(NB=23476)
      DIMENSION VAR(LM,3),W(LM*3*3)
      EXTERNAL SBDVAR,SBDISS

*---- OPEN SUBROUTINE PACKAGE AND INITIALIZE VARIABLES -----------------

      LEV=10                    !高階粘性項のラプラシアンの階数
      ITM=10                    !時間発展するステップ数
      NDV=10                     !Runge-Kuttaでのステップ分割数
      DT=1D0                  !ファイル出力の時間間隔

      DNU=10D0/(1D0*MM*(MM+1))**LEV  !高階粘性の係数
*      DNU=0                     

*/ ガウシアン型の擾乱のパラメター(ジオポテンシャルに与える) /*

      DA=1D-1     !擾乱の振幅
      DB=10D0    !擾乱の幅の逆数のルートに相当するパラメター
      X0=PI        !擾乱の中心位置のλ座標
*      Y0=PI/4     !擾乱の中心位置のφ座標
      Y0=0     

      CALL SBOPEN(LEV,DNU,NDV,DT)
      CALL SBINIT(VAR,DA,DB,X0,Y0)

*---- OPEN FHPACK ------------------------------------------------------

      CALL FHUOPN(IU,CF,'W',NB)

*---- TIME EVOLUTION BY RUNGE-KUTTA METHOD -----------------------------

      I=0
      TIM=0
      CALL FEPUTS(IU,LM*3,VAR)
      CALL SBCHCK(I,VAR)

      DO I=1,ITM
        CALL TDRKNU(LM*3,NDV,DT,TIM,VAR,W,SBDISS,SBDVAR)
        CALL FEPUTS(IU,LM*3,VAR)
        CALL SBCHCK(I,VAR)
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
      PARAMETER(ID=IM+1,JD=JM+1)
      PARAMETER(IW=IM+2,JW=JD)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(SQRT3=1.7320508075688772935D0)

*/ OMEGA=Ω, BARPHI=Φの基準値 /*
      
      PARAMETER(OMEGA=2*PI,BARPHI=1D0)

*/ 重力波を分離して解くかどうかのフラグ.
*/      ISWTCH=1なら分離して解く. そうでなければ分離しない.

      PARAMETER(ISWTCH=1)

      DIMENSION VAR(LM,3),DVAR(LM,3)
      DIMENSION RN(LM,2),DRN(LM),DOM(LM)
      DIMENSION CD(LM),SD(LM)
      DIMENSION CP(LM),SP(LM)
      DIMENSION XG(IM),YG(JM)
      DIMENSION G(ID,JD)
      DIMENSION IT(5),T(IM*2),Y(JM/2,4)
      DIMENSION IP(((MM+1)/2+MM+1)*2),P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP4(4*((MM+1)/2+MM+1)*2),P4(4*((MM+1)/2+MM+1)*JM)
      DIMENSION R4(4*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP5(5*((MM+1)/2+MM+1)*2),P5(5*((MM+1)/2+MM+1)*JM)
      DIMENSION R5(5*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(5*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(5*JW*IW),WS(5*JW*IW)
      SAVE

      CALL SNINIT(MM,IM,JM,IT,T,Y,IP,P,R,IA,A)
      CALL SNKINI(MM,JM,4,IP,P,R,IP4,P4,R4)
      CALL SNKINI(MM,JM,5,IP,P,R,IP5,P5,R5)
      CALL SPNINI(MM,RN)

      DO J=1,JM/2
        YG(JM/2+J  )= ASIN(Y(J,1))
        YG(JM/2-J+1)=-ASIN(Y(J,1))
      END DO

      DO J=1,IM
        XG(J)=2*PI*(J-1)/IM
      END DO

      DTDNU=DNU*DT/(2*NDV)

      DO L=1,LM
        DRN(L)=EXP(-DTDNU*ABS(RN(L,1))**LEV)
      END DO

*/ 重力波成分を陽に分離して解く際に必要な配列の準備      

      DO L=1,LM
        DOM(L)=SQRT(-RN(L,1)*BARPHI)
      END DO

      DTINT=DT/(2*NDV)

      CD(1)=1
      SD(1)=0
      CP(1)=1
      SP(1)=0
      DO L=2,LM
        CD(L)=COS(DOM(L)*DTINT)
        SD(L)=SIN(DOM(L)*DTINT)*(-RN(L,1))/DOM(L)
        CP(L)=COS(DOM(L)*DTINT)
        SP(L)=-SIN(DOM(L)*DTINT)*BARPHI/DOM(L)
      ENDDO

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF d(VAR)/dt
*-----------------------------------------------------------------------
      ENTRY SBDVAR(TIM,VAR,DVAR)

      CALL SPSWNL(MM,IM,ID,JM,JD,OMEGA,
     &   VAR(1,1), VAR(1,2), VAR(1,3),
     &  DVAR(1,1),DVAR(1,2),DVAR(1,3),
     &  RN,IT,T,Y,IP4,P4,R4,IP5,P5,R5,IA,A,Q,WS,WW)

      IF(ISWTCH.EQ.1) THEN
        DO L=1,LM
          DVAR(L,2)=DVAR(L,2)+RN(L,1)*VAR(L,3)
          DVAR(L,3)=DVAR(L,3)+BARPHI*VAR(L,2)
        END DO
      END IF

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF THE EFFECT OF VISCOSITY
*-----------------------------------------------------------------------
      ENTRY SBDISS(TIM,DTIM,VAR)

      IF(ISWTCH.EQ.1) THEN
        DO L=1,LM
          VAR(L,1)=DRN(L)*VAR(L,1)
          TMPPHI=VAR(L,2)
          VAR(L,2)=DRN(L)*(CD(L)*VAR(L,2)+SD(L)*VAR(L,3))
          VAR(L,3)=DRN(L)*(CP(L)*VAR(L,3)+SP(L)*TMPPHI)
        END DO
      ELSE
        DO L=1,LM
          VAR(L,1)=DRN(L)*VAR(L,1)
          VAR(L,2)=DRN(L)*VAR(L,2)
          VAR(L,3)=DRN(L)*VAR(L,3)
        END DO
      END IF
      
      RETURN
*-----------------------------------------------------------------------
*     CHECK ENERGY AND ENSTROPHY CONSERVATION
*-----------------------------------------------------------------------
      ENTRY SBCHCK(I,VAR)

      CALL SPSWCK(MM,IM,ID,JM,JD,OMEGA,
     &  VAR(1,1), VAR(1,2), VAR(1,3),
     &  AMOM,AEME,AENS,
     &  RN,IT,T,Y,IP4,P4,R4,IA,A,Q,WS,WW)

      print *,'amom= ',AMOM,' aene=',AEME,' aens=',AENS

      RETURN
*-----------------------------------------------------------------------
*     INITIALIZATION OF VAR
*-----------------------------------------------------------------------
      ENTRY SBINIT(VAR,DA,DB,X0,Y0)

      CALL BSSET0(LM*3,VAR)

*/ 絶対渦度場の設定(プラネタリー渦度のみ)
      VAR(3,1)=2*OMEGA/SQRT3

*/ ポテンシャル場の設定

      CALL SCINID(JM,IM,JD,ID,G,XG,YG,DA,DB,X0,Y0)
      CALL SNTG2S(MM,IM,ID,JM,JD,1,
     &  G,VAR(1,3),IT,T,Y,IP,P,R,IA,A,Q,WS,WW,0,0)

      VAR(1,3)=BARPHI

      END
************************************************************************
*     CALCULATE DISTURBANCE FIELD
************************************************************************
      SUBROUTINE SCINID(JM,IM,JD,ID,G,XG,YG,DA,DB,X0,Y0)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION G(ID,JD)
      DIMENSION YG(JM),XG(IM)

      CB=COS(PI/2-Y0)
      SB=SIN(PI/2-Y0)

      DO I=1,IM
        CA=COS(XG(I)-X0)
        DO J=1,JM
          CC=COS(PI/2-YG(J))
          SC=SIN(PI/2-YG(J))
          CR=CB*CC+SB*SC*CA
          G(I,J)=DA*(EXP(DB*(CR-1))-(1-EXP(-2*DB))/(2*DB))
        END DO
      END DO

      END
