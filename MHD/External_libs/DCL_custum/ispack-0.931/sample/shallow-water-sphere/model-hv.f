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
*     浅水方程式をsppackで解くためのサンプルプログラム       2000/08/16
*     (角運動量を保存する高階粘性項を使う)
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

      ITM=10                    !時間発展するステップ数
*      NDV=10                     !Runge-Kuttaでのステップ分割数
*      DT=1D0                  !ファイル出力の時間間隔
*      NDV=20                     !Runge-Kuttaでのステップ分割数
*      DT=1D0                  !ファイル出力の時間間隔
      NDV=20                     !Runge-Kuttaでのステップ分割数
      DT=1D0                  !ファイル出力の時間間隔

*      LEV=10
      LEV=3
      DNU=10D0/(1D0*MM*(MM+1)-2)**(LEV+1) !高階粘性の係数
*      ALPHA=0
*      ALPHA=2D0/3
      ALPHA=1

*/ ガウシアン型の擾乱のパラメター(ジオポテンシャルに与える) /*

      DA=1D-1     !擾乱の振幅
      DB=10D0    !擾乱の幅の逆数のルートに相当するパラメター
      X0=PI        !擾乱の中心位置のλ座標
*      Y0=PI/4     !擾乱の中心位置のφ座標
      Y0=0     

      CALL SBOPEN(DNU,ALPHA,LEV,NDV,DT)
      CALL SBINIT(VAR,DA,DB,X0,Y0)

*---- OPEN FHPACK ------------------------------------------------------

      CALL FHUOPN(IU,CF,'W',NB)

*---- TIME EVOLUTION BY RUNGE-KUTTA METHOD -----------------------------

      I=0
      TIM=0
      CALL FEPUTS(IU,LM*3,VAR)
      CALL SBCHCK(I,VAR)

      CALL APTIME(TIM0)
      DO I=1,ITM
        CALL TDRKNU(LM*3,NDV,DT,TIM,VAR,W,SBDISS,SBDVAR)
        CALL FEPUTS(IU,LM*3,VAR)
        CALL SBCHCK(I,VAR)
      END DO
      CALL APTIME(TIM1)

      print *,'time=',TIM1-TIM0

*---- CLOSE FHPACK -----------------------------------------------------

      CALL FHUCLS(IU)

      END
************************************************************************
*     OPEN SUBROUTINE PACKAGE
************************************************************************
      SUBROUTINE SBOPEN(DNUD,ALPHAD,LEVD,NDV,DT)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=21,JM=32,IM=64,LM=(MM+1)*(MM+1))
      PARAMETER(ID=IM+1,JD=JM+1)
      PARAMETER(IW=IM+2,JW=JD)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(SQRT3=1.7320508075688772935D0)
*/ OMEGA=Ω, BARPHI=Φの基準値 /*
      PARAMETER(OMEGA=2*PI,BARPHI=1D0)
      DIMENSION VAR(LM,3),DVAR(LM,3)
      DIMENSION RN(LM,2)
      DIMENSION IRM(LM,2)
      DIMENSION CL(LM,5)
      DIMENSION XG(IM),YG(JM)
      DIMENSION G(ID,JD)
      DIMENSION IT(5),T(IM*2),Y(JM/2,4)
      DIMENSION IP(((MM+1)/2+MM+1)*2),P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(JW*IW),WS(11*JW*IW)
*      DIMENSION WW(JW*IW*11),WS(11*JW*IW)
*      DIMENSION WW(JW*IW*10),WS(10*JW*IW)
      SAVE

      DNU=DNUD
      ALPHA=ALPHAD
      LEV=LEVD

      CALL SNINIT(MM,IM,JM,IT,T,Y,IP,P,R,IA,A)
      CALL SPNINI(MM,RN)
      CALL SPMINI(MM,IRM)

      DO J=1,JM/2
        YG(JM/2+J  )= ASIN(Y(J,1))
        YG(JM/2-J+1)=-ASIN(Y(J,1))
      END DO

      DO J=1,IM
        XG(J)=2*PI*(J-1)/IM
      END DO

*/ 重力波成分を陽に分離して解く際に必要な配列の準備

      CALL SPSWHI(MM,BARPHI,DNU,ALPHA,LEV,DT/(2*NDV),CL)

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF d(VAR)/dt
*-----------------------------------------------------------------------
      ENTRY SBDVAR(TIM,VAR,DVAR)

      CALL SPSWHV(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,LEV,
*      CALL SPSWHW(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,LEV,
*      CALL SPSWHX(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,LEV,
     &   VAR(1,1), VAR(1,2), VAR(1,3),
     &  DVAR(1,1),DVAR(1,2),DVAR(1,3),
     &  RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF THE EFFECT OF VISCOSITY
*-----------------------------------------------------------------------
      ENTRY SBDISS(TIM,DTIM,VAR)

      CALL SPSWLV(MM,VAR(1,1),VAR(1,2),VAR(1,3),CL)

      RETURN
*-----------------------------------------------------------------------
*     CHECK ENERGY AND ENSTROPHY CONSERVATION
*-----------------------------------------------------------------------
      ENTRY SBCHCK(I,VAR)

      CALL SPSWCV(MM,IM,ID,JM,JD,OMEGA,
     &  VAR(1,1), VAR(1,2), VAR(1,3),
     &  AMOM,AEME,AENS,
     &  RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      print *,'amom= ',AMOM,' aene=',AEME,' aens=',AENS
*      print *,'var(1,3)=',VAR(1,3)
*      print *,'var(1,2)=',VAR(1,2)
*      print *,'var(1,1)=',VAR(1,1)
*      print *,'var(3,1)=',VAR(3,1)
*      print *,'DNU,ALPHA,BARPHI',DNU,ALPHA,BARPHI

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
