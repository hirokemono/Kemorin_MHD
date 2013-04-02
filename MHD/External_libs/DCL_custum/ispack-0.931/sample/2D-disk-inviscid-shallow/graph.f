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
*     円盤領域の非粘性浅水方程式の時間発展(グラフ表示のみ)
*                                             2003 /04/10 By K. Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=10)
      PARAMETER(IU=11,NB=16384) ! データ出力装置番号とバッファ長の指定      
      PARAMETER(MMS=(MM+1)*(MM+2)/2*3-1)
      DIMENSION VAR(MMS)

      ITM=1000              !時間発展するステップ数      
      DT=1D0                !ファイル出力の時間間隔
      TIM=0

* サブルーチンパッケージおよび変数の初期化      

      CALL SBOPEN
      CALL FHUOPN(IU,'data.dat','R',NB) ! 入力ファイルのオープン      

* 時間発展および保存量のチェックとグラフ表示

      DO I=0,ITM
        CALL FEGETD(IU,MMS,VAR) ! データの入力
        TIM=I*DT
        print *,TIM
        CALL SBGRPH(TIM,VAR)
      END DO

* 終了処理
      
      CALL FHUCLS(IU)      ! 入力ファイルのクローズ
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
      SAVE

      CALL DKAINI(MM,JM,IM,IT,T,P,A)  ! DKPACKの初期化

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
