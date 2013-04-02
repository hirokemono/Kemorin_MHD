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
*     円盤領域の非粘性浅水方程式の時間発展(重力波成分を分離して解く)     
*                                              2003/04/10 By K. Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=10)
      PARAMETER(MMS=(MM+1)*(MM+2)/2*3-1)
      DIMENSION VAR(MMS),W(MMS*3),WS(MMS)
      EXTERNAL SBDVAN,SBDVAL      

      ITM=1000              !時間発展するステップ数      
      NDV=10                !Runge-Kuttaでのステップ分割数
      DT=1D0                !ファイル出力の時間間隔
      TIM=0

* サブルーチンパッケージおよび変数の初期化
* 重力波成分を分離して解くため, 与えた基本場が厳密に定常にならないため
* 時間積分において補正をかけていることに注意.

      CALL SBOPEN
      CALL SBINIZ(VAR)
      CALL BSCOPY(MMS,VAR,WS)
      CALL TDRKNU(MMS,1,DT/NDV,TIM,VAR,W,SBDVAL,SBDVAN)
      DO K=1,MMS
        WS(K)=VAR(K)-WS(K)
      END DO
      CALL SBINIT(VAR)
      CALL SBGRPH(TIM,VAR)

* 時間発展および保存量のチェックとグラフ表示

      DO I=1,ITM
        DO J=1,NDV        
          CALL TDRKNU(MMS,1,DT/NDV,TIM,VAR,W,SBDVAL,SBDVAN)
          DO K=1,MMS
            VAR(K)=VAR(K)-WS(K)
          END DO
        END DO          
        TIM=I*DT
        CALL SBCCNS(VAR,AMOM,AENE)
        print *,TIM,VAR(1),AMOM,AENE ! 保存量のチェック
        CALL SBGRPH(TIM,VAR)
      END DO

* グラフィクスの終了処理
      
      CALL SGCLS

      END
************************************************************************
*     OPEN SUBROUTINE PACKAGE
************************************************************************
      SUBROUTINE SBOPEN

      IMPLICIT REAL*8(A-H,O-Z)
*-----------------------------------------------------------------------
*     パラメターの設定(MMの値はメインプログラムの値に合わせること)
*-----------------------------------------------------------------------
      PARAMETER(F=1D0)        ! コリオリパラメターの値
      PARAMETER(MM=10,JM=9,IM=32)
        ! 切断次数 MM および 分点数 JM, IM の設定
        ! IM ≧ 3MM+1, JM≧(MM+MM/2+2)/2 であること
*-----------------------------------------------------------------------
*     以下, 使用する配列の宣言など(特に変更する必要はない)
*-----------------------------------------------------------------------
      PARAMETER(MMP=(MM+8)*MM/4+1)
      PARAMETER(MMS=(MM+1)*(MM+2)/2*3-1)
      DIMENSION P(JM,2*MMP)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)      
      DIMENSION T(IM*2),IT(5)
      DIMENSION S(MMS),DS(MMS)
      DIMENSION G(JM,0:IM-1,10),WORK(JM*IM)
      DIMENSION HB(JM,0:IM-1,3)
      REAL RY(JM),RT(0:IM),RH(JM,0:IM)

      DIMENSION WORKD(JM,13)      
      DIMENSION WL((MM/2+1)*3,6)
      DIMENSION WV((MM/2+1)*(MM/2+1)*9,3)
      DIMENSION WRM(MM*(MM+4)/2)
      DIMENSION VRM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      DIMENSION VLM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      SAVE

      CALL DKAINI(MM,JM,IM,IT,T,P,A)  ! DKPACKの初期化
      HBNDRY=1 ! 境界での水深の値
      CALL DKAEGA(MM,JM,F,HBNDRY,WORKD,P,A,WL,WV,WRM,VRM,VLM)
       ! 重力波成分を分離して解くための行列の準備

*-----------------------------------------------------------------------
*     以下, グラフィクス(DCL)の初期化
*-----------------------------------------------------------------------

      CALL SGPSET('LCORNER',.FALSE.)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SWISET('IWIDTH',  400)
      CALL SWISET('IHEIGHT', 400)
      CALL SWISET('IPOSX', 200)
      CALL SWISET('IPOSY', 150)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL SGOPN(1)
      CALL SGPSET('LSOFTF',.FALSE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGPSET('LCNTL',.FALSE.)
      CALL SLRAT(1.0,1.0)
      CALL UZPSET('INNER',-1)

      DO J=1,JM
        RY(J)=P(J,1)
      END DO
      DO I=0,IM
        RT(I)=360D0*I/IM
      END DO
      
      CALL UWSGXA(RY,JM)
      CALL UWSGYA(RT,IM+1)

      RETURN
*-----------------------------------------------------------------------
*     従属変数の初期化(基本場成分のみ)
*-----------------------------------------------------------------------
      ENTRY SBINIZ(S)

      CALL BSSET0(JM*IM*3,HB)
      CALL BSSET0(JM*IM*3,G)

      DO I=0,IM-1
        DO J=1,JM
          Y=P(J,1)
          X=Y*Y
          G(J,I,1)=(-(1-X)**5/5-F*(1-X)**3/3)/2+1
          G(J,I,2)=Y*(1-X)**2
        END DO
      END DO

      CALL DKAG2S(MM,JM,IM,G,S,G(1,0,10),IT,T,P,A)      

      RETURN
*-----------------------------------------------------------------------
*     従属変数の初期化. ここでは,例として, 不安定な軸対称場に微小擾乱を
*     加えたものを与えている(擾乱として波数1を与えている)
*-----------------------------------------------------------------------
      ENTRY SBINIT(S)

      CALL BSSET0(JM*IM*3,G)

      DO I=0,IM-1
        DO J=1,JM
          Y=P(J,1)
          X=Y*Y
          G(J,I,1)=(-(1-X)**5/5-F*(1-X)**3/3)/2+1
          G(J,I,2)=Y*(1-X)**2
        END DO
      END DO

      PI=4*ATAN(1D0)
      DO I=0,IM-1
        DO J=1,JM
          Y=P(J,1)
          X=Y*Y
          G(J,I,3)=1D-6*COS(2*PI*I/IM)*(1-X)
        END DO
      END DO

      CALL DKAG2S(MM,JM,IM,G,S,G(1,0,10),IT,T,P,A)

      RETURN
*-----------------------------------------------------------------------
*     時間微分項の計算
*-----------------------------------------------------------------------
      ENTRY SBDVAR(TIM,S,DS)

      CALL DKATDV(MM,JM,IM,F,HB(1,0,2),S,DS,G,IT,T,P,A)

      RETURN
*-----------------------------------------------------------------------
*     重力波成分以外の時間微分項の計算
*-----------------------------------------------------------------------
      ENTRY SBDVAN(TIM,S,DS)

      CALL DKATDL(MM,JM,IM,F,HBNDRY,HB(1,0,2),S,DS,G,IT,T,P,A)

      RETURN
*-----------------------------------------------------------------------
*     重力波成分の時間発展
*-----------------------------------------------------------------------
      ENTRY SBDVAL(TIM,DTIM,S)

      CALL DKATDG(MM,S,DTIM,WORK,WRM,VRM,VLM)

      RETURN
*-----------------------------------------------------------------------
*     保存量(全角運動量および全エネルギーの計算)
*-----------------------------------------------------------------------
      ENTRY SBCCNS(S,AMOM,AENE)

      CALL DKACNS(MM,JM,IM,F,HB,S,AMOM,AENE,G,IT,T,P)

      RETURN
*-----------------------------------------------------------------------
*     グラフィクス(ジオポテンシャル場の等高線を描く)
*-----------------------------------------------------------------------
      ENTRY SBGRPH(TIM,S)

      CALL SGFRM
      CALL SGSVPT(0.0,1.0,0.0,1.0)      
      CALL SGSSIM(0.5,0.0,0.0)
      CALL SGSTRN(5)
      CALL SGSTRF

      CALL DKAS2G(MM,JM,IM,S,G,G(1,0,10),IT,T,P)

      DO I=0,IM-1
        DO J=1,JM
          RH(J,I)=HB(J,I,1)+G(J,I,1)
        END DO
      END DO
      DO J=1,JM
        RH(J,IM)=RH(J,0)
      END DO

      DO I=0,3
        CALL SGLNU(0.0,90.0*I,1.0,90.0*I)
      END DO
      DO I=0,IM-1
        CALL SGLNU(1.0,RT(I),1.0,RT(I+1))
      END DO
      CALL UDCNTR(RH,JM,JM,IM+1)

      END
