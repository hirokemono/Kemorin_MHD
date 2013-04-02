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
*     円盤領域の非粘性浅水方程式の時間発展     2003/04/10 By K. Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=10)
      PARAMETER(IU=11,NB=16384) ! データ出力装置番号とバッファ長の指定
      PARAMETER(MMS=(MM+1)*(MM+2)/2*3-1)
      DIMENSION VAR(MMS),W(MMS*3)
      EXTERNAL SBDVAR      

      ITM=1000              !時間発展するステップ数      
      NDV=20                !Runge-Kuttaでのステップ分割数
      DT=1D0                !ファイル出力の時間間隔
      TIM=0

* サブルーチンパッケージおよび変数の初期化      

      CALL SBOPEN
      CALL FHUOPN(IU,'data.dat','W',NB) ! 出力ファイルのオープン
      CALL SBINIT(VAR)
      CALL FEPUTD(IU,MMS,VAR) ! データの出力

* 時間発展および保存量のチェックとファイル出力

      DO I=1,ITM
        CALL TDRK4U(MMS,NDV,DT,TIM,VAR,W,SBDVAR)
        CALL FEPUTD(IU,MMS,VAR) ! データの出力        
        TIM=I*DT
        CALL SBCCNS(VAR,AMOM,AENE)
        print *,TIM,VAR(1),AMOM,AENE ! 保存量のチェック
      END DO

* 終了処理
      
      CALL FHUCLS(IU)      ! 出力ファイルのクローズ

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
      SAVE

      CALL DKAINI(MM,JM,IM,IT,T,P,A)  ! DKPACKの初期化

      RETURN
*-----------------------------------------------------------------------
*     従属変数の初期化. ここでは,例として, 不安定な軸対称場に微小擾乱を
*     加えたものを与えている(擾乱として波数1を与えている)
*-----------------------------------------------------------------------
      ENTRY SBINIT(S)

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
*     保存量(全角運動量および全エネルギーの計算)
*-----------------------------------------------------------------------
      ENTRY SBCCNS(S,AMOM,AENE)

      CALL DKACNS(MM,JM,IM,F,HB,S,AMOM,AENE,G,IT,T,P)

      END
