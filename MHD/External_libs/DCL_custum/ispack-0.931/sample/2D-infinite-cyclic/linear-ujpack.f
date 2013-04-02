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
*     線形浅水方程式のサンプルプログラム                    2009/06/15
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER(LM=31,KM=31)
      DIMENSION VAR(3*(2*KM+1)*(2*LM+1)),W(3*(2*KM+1)*(2*LM+1)*3)
      EXTERNAL SBDVAR,SBGDZL

*---- OPEN SUBROUTINE PACKAGE AND INITIALIZE VARIABLES -----------------

      ITM=100               !時間発展するステップ数
      NDV=2                 !Runge-Kuttaでのステップ分割数
      DT=0.1D0              !描画出力の時間間隔
      NV=2                  !高階粘性(もどき)項のラプラシアンの階数
      DNU=1D-7              !高階粘性(もどき)係数
      DELTAT=DT/NDV         !Runge-Kutta内部の時間刻み

      CALL SBOPEN(NV,DNU,DELTAT)
      CALL SBINIT(VAR)
      I=0

*---- TIME EVOLUTION BY RUNGE-KUTTA METHOD -----------------------------

      I=0
      TIM=0
      CALL SBGRID(TIM,VAR)

      DO I=1,ITM
        CALL TDRKNU(3*(2*KM+1)*(2*LM+1),NDV,DT,TIM,VAR,W,SBGDZL,SBDVAR)
        TIM=I*DT
        print *,TIM
        CALL SBGRID(TIM,VAR)
      END DO

*---- CLOSE DCL --------------------------------------------------------

      CALL GRCLS

      END
************************************************************************
*     OPEN SUBROUTINE PACKAGE
************************************************************************
      SUBROUTINE SBOPEN(NV,DNU,DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CSGI*1
      PARAMETER(LM=31,KM=31)
      PARAMETER(IM=64,JM=64) ! 線形なので, IM>2*KM, JM>2*LM+1 でよい
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(R=1) ! y方向スケーリングパラメター
      PARAMETER(F=1) ! コリオリパラメター
      ! 平均水深は 1 に固定している
      DIMENSION VAR(-LM:LM,-KM:KM,3) ! u:(*,1), v(*,2), h:(*,3)
      DIMENSION DVAR(-LM:LM,-KM:KM,3)
      DIMENSION DL(-LM:LM,-KM:KM)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM-1,0:IM-1),G2(0:JM-1,0:IM-1)
      DIMENSION W(IM*JM)
      DIMENSION ITJ(4),TJ(JM*7),ITI(4),TI(IM*6)
      DIMENSION X(0:IM-1),Y(0:JM-1)
      REAL RY(0:JM-1),RX(-IM/2:IM/2),RH(-IM/2:IM/2,0:JM-1)
      REAL RPI
      SAVE

      RPI=PI
      
      CALL UJINIT(JM,IM,ITJ,TJ,ITI,TI,Y,R)

      DO I=0,IM-1
        X(I)=2*PI*I/IM
      END DO

      DO J=0,JM-1
        RY(J)=Y(J)
      END DO
      
      DO I=-IM/2,IM/2
        RX(I)=2*PI*I/IM
      END DO

      DO IV=1,3
        DO K=-KM,KM
          DO L=-LM,LM
            RK=K*K+(L/R)*(L/R)
            DL(L,K)=EXP(-DNU*DELTAT/2*RK**NV)
          END DO
        END DO
      END DO

      CALL SGPSET('LCORNER',.FALSE.)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SWISET('IWIDTH',  400)
      CALL SWISET('IHEIGHT', 400)
      CALL SWISET('IPOSX', 200)
      CALL SWISET('IPOSY', 150)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL SGPSET('LSOFTF',.FALSE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGPSET('LCNTL',.FALSE.)
      CALL SLRAT(1.0,1.0)
      CALL UZPSET('INNER',-1)

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF d(VAR)/dt (線形浅水方程式に基づく)
*-----------------------------------------------------------------------
      ENTRY SBDVAR(TIM,VAR,DVAR)

      CALL UJS2GX(LM,KM,JM,IM,VAR(-LM,-KM,1),G,W,ITJ,TJ,ITI,TI)
      ! G = ∂u/∂x
      CALL UJS2GY(LM,KM,JM,IM,VAR(-LM,-KM,2),G2,W,ITJ,TJ,ITI,TI,Y,R)
      ! G2 = ∂v/∂y
      DO I=0,IM-1
        DO J=0,JM-1
          G(J,I)=-G(J,I)-G2(J,I)
        END DO
      END DO
      CALL UJG2SA(LM,KM,JM,IM,G,DVAR(-LM,-KM,3),W,ITJ,TJ,ITI,TI)

      CALL UJS2GX(LM,KM,JM,IM,VAR(-LM,-KM,3),G,W,ITJ,TJ,ITI,TI)
      ! G = ∂h/∂x
      CALL UJG2SA(LM,KM,JM,IM,G,DVAR(-LM,-KM,1),W,ITJ,TJ,ITI,TI)
      DO K=-KM,KM
        DO L=-LM,LM
          DVAR(L,K,1)=-DVAR(L,K,1)+F*VAR(L,K,2)
        END DO
      END DO

      CALL UJS2GY(LM,KM,JM,IM,VAR(-LM,-KM,3),G,W,ITJ,TJ,ITI,TI,Y,R)
      ! G = ∂h/∂y
      CALL UJG2SA(LM,KM,JM,IM,G,DVAR(-LM,-KM,2),W,ITJ,TJ,ITI,TI)
      DO K=-KM,KM
        DO L=-LM,LM
          DVAR(L,K,2)=-DVAR(L,K,2)-F*VAR(L,K,1)
        END DO
      END DO

      RETURN
*-----------------------------------------------------------------------
*     高階粘性(もどき)の作用
*-----------------------------------------------------------------------            
      ENTRY SBGDZL(TIM,DT,VAR)

      DO IV=1,3
        DO K=-KM,KM
          DO L=-LM,LM
            VAR(L,K,IV)=DL(L,K)*VAR(L,K,IV)
          END DO
        END DO
      END DO
    
      RETURN
*-----------------------------------------------------------------------
*     描画(水深(h)場のみ等高線表示)
*-----------------------------------------------------------------------
      ENTRY SBGRID(TIM,VAR)

      CALL UJS2GA(LM,KM,JM,IM,VAR(-LM,-KM,3),G,W,ITJ,TJ,ITI,TI)

      DO I=0,IM/2
        DO J=0,JM-1
          RH(I,J)=G(J,I)
        END DO
      END DO
      DO I=-IM/2,-1
        DO J=0,JM-1
          RH(I,J)=G(J,IM+I)
        END DO
      END DO

      CALL GRFRM
      CALL SGSVPT(0.1,0.95,0.1,0.95)
      CALL SGSWND(-RPI,RPI,-RPI,RPI)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UXAXDV('B',1.0,2.0)
      CALL UXAXDV('T',1.0,2.0)
      CALL UYAXDV('L',1.0,2.0)
      CALL UYAXDV('R',1.0,2.0)
      DO J=0,JM-1
        CALL SGTXZU(0.0,RY(J),'*',0.02,0,0,1)
      END DO
      DO I=-IM/2,IM/2
        CALL SGTXZU(RX(I),0.0,'*',0.02,0,0,1)
      END DO
      CALL UWSGYA(RY,JM)
      CALL UWSGXA(RX,IM+1)
      CALL UDGCLA(-1.0,1.2,0.05)
      CALL UDCNTR(RH,IM+1,JM,JM)

      RETURN
*-----------------------------------------------------------------------
*     初期値設定(原点を中心とするガウシアンもどきの水深分布を与えている)
*-----------------------------------------------------------------------
      ENTRY SBINIT(VAR)

      DO I=0,IM-1
        DO J=0,JM-1
          G(J,I)=EXP(-((Y(J))**2-(COS(X(I))-1)*2)*4)
        END DO
      END DO

      CALL UJG2SA(LM,KM,JM,IM,G,VAR(-LM,-KM,3),W,ITJ,TJ,ITI,TI)
      CALL BSSET0((2*LM+1)*(2*KM+1)*2,VAR)

      END
