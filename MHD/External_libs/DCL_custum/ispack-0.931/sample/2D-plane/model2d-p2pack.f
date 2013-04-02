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
*     model2d-p2pack.f:
*               p2packを使って2次元非発散渦度方程式を解くプログラム
*
*                                                  2001/07/23 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* パラメターの設定 

*      PARAMETER(LM=63,KM=63)    !切断波数の設定(aliasingをとっていない)
      PARAMETER(LM=42,KM=42)    !切断波数の設定(aliasingをとる場合)
      PARAMETER(N=(2*LM+1)*(2*KM+1)) !スペクトル変数の配列の大きさ
      DIMENSION Z(N)            !渦度のスペクトル係数の配列
      DIMENSION W(N,3)          !Runge-Kutta法のための作業領域
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kutta法が呼び出すサブルーチン

      NSTEP=100                 !時間発展するステップ数
      H=1D0                     !グラフィックス表示のための時間間隔
      M=3                       !Runge-Kuttaでのステップ分割数

      NV=5                      !高階粘性項のラプラシアンの階数
      DNU=1D-17                 !高階粘性係数
      DELTAT=H/M                !Runge-Kutta内部の時間刻み

* 初期化

      CALL SBINIT(NV,DNU,DELTAT)
      CALL SBINIZ(Z)

* 時間発展およびグラフィックス表示

      T=0                       !Tは時刻を表す変数
      CALL SBGRPH(T,Z)

      DO ISTEP=1,NSTEP
        CALL TDRKNU(N,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGRPH(T,Z)
      END DO

* 終了処理

      CALL SBCLOS

      END
***********************************************************************
*     時間積分等に必要な種々の処理をまとめたサブルーチンパッケージ
*----------------------------------------------------------------------
*     パッケージの初期化をするサブルーチン
*----------------------------------------------------------------------
      SUBROUTINE SBINIT(NV,DNU,DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CTIME*5
      PARAMETER(JM=128,IM=128)    !格子点数の設定(JM: y方向, IM: x方向)
*      PARAMETER(LM=63,KM=63)    !切断波数の設定(aliasingをとっていない)
      PARAMETER(LM=42,KM=42)    !切断波数の設定(aliasingをとる場合)
      PARAMETER(R=1)    !x方向とy方向のアスペクト比
      DIMENSION Z(-LM:LM,-KM:KM) !渦度のスペクトル係数の配列
      DIMENSION DZ(-LM:LM,-KM:KM) !非線形項のスペクトル係数の配列
      DIMENSION WS(-LM:LM,-KM:KM) !作業領域
      DIMENSION WG(0:JM-1,0:IM-1,3) !作業領域
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2) !P2PACKで使われる配列
      DIMENSION DL(-LM:LM,-KM:KM) !粘性効果のために使われる配列
      REAL RG(0:IM,0:JM)        !グラフィックスに使われる配列
      REAL DLEV,TLEV1,TLEV2     !グラフィックスに使われる変数
      SAVE

* 与えられた引数から, SBGDZNで使われる配列DL(粘性による減衰効果を与える)
* を設定する. なお, SBGDZNはRunge-Kuttaの内部で呼び出されるが, この際,
* 常にDT=DELTAT/2で呼び出されるから, それを考慮に入れると, DLは以下のよ
* うに設定できる.

      DO K=-KM,KM
        DO L=-LM,LM
          DL(L,K)=EXP(-DNU*DELTAT/2*(1D0*(K*K+L*L))**NV)
        END DO
      END DO

* P2PACKの初期化

      CALL P2INIT(JM,IM,ITJ,TJ,ITI,TI)

* グラフィックスの初期化

      CALL SWISET('IWIDTH',  400)
      CALL SWISET('IHEIGHT', 400)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL SLRAT(1.0,1.0)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL SLRAT(1.0,1.0)
      CALL UDGCLA(0.0,1.0,0.1)

      IPAT0=25
      DLEV=0.05
      NLEV=20
      IDLEV=3

      CALL UESTLV(999.0,0.0,IPAT0*1000+999)
      DO I=1,NLEV-1
        IPAT=(IPAT0+I*IDLEV)*1000+999
        TLEV1=(I-1)*DLEV
        TLEV2=I*DLEV
        CALL UESTLV(TLEV1,TLEV2,IPAT)
      END DO
      IPAT=(IPAT0+NLEV*IDLEV)*1000+999
      CALL UESTLV(TLEV2,999.0,IPAT)

      RETURN
*----------------------------------------------------------------------
*     渦度のスペクトル係数(Z)の初期化
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

* ここでは, 2つのガウシアン型の渦から成る渦度分布を格子点上で与え, 
* それをP2G2SAを使ってスペクトル係数に変換して, それをZの初期値と
* している

      X1=0.4D0
      Y1=0.4D0
      X2=0.6D0
      Y2=0.6D0
      SIGMA=0.05D0

      DO I=0,IM-1
        X=1D0*I/IM
        DO J=0,JM-1
          Y=1D0*J/JM
          WG(J,I,1)= EXP(-((X-X1)**2+(Y-Y1)**2)/(2*SIGMA**2))
     &              +EXP(-((X-X2)**2+(Y-Y2)**2)/(2*SIGMA**2))
        END DO
      END DO

      CALL P2G2SA(LM,KM,JM,IM,WG,Z,WG(0,0,3),ITJ,TJ,ITI,TI)

      Z(0,0)=0                  !渦度の平均は0でなければならないから

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および渦度のスペクトル係数(Z)を入力として, 非線形項のスペ
*     クトル係数の配列(DZ)を求めるサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P2AJBS(LM,KM,JM,IM,R,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および時間刻み(DT)を入力として, 線形(粘性)項によるZの
*     T→T+DTにおける発展を解くサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO K=-KM,KM
        DO L=-LM,LM
          Z(L,K)=DL(L,K)*Z(L,K)
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     Zに対応する渦度場をグラフィックス表示する.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,Z)

      WRITE(CTIME,'(F5.1)') T

      CALL P2S2GA(LM,KM,JM,IM,Z,WG,WG(0,0,3),ITJ,TJ,ITI,TI)

      DO I=0,IM-1
        DO J=0,JM-1
          RG(I,J)=WG(J,I,1)
        END DO
      END DO

      DO J=0,JM-1
        RG(IM,J)=RG(0,J)
      END DO
      DO I=0,IM-1
        RG(I,JM)=RG(I,0)
      END DO
      RG(IM,JM)=RG(0,0)

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(0.0,1.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UETONE(RG,IM+1,IM+1,JM+1)
*      CALL UDCNTR(RG,IM+1,IM+1,JM+1)
      CALL USDAXS
      CALL UXSTTL('T','T='//CTIME,1.0)
*      CALL GRFRM

      RETURN
*----------------------------------------------------------------------
*     終了処理
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
