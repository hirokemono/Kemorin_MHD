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
*     baro.f: c2packを使って2次元非発散渦度方程式を解くプログラム
*                                                  2000/10/24 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* パラメターの設定 

      PARAMETER(LM=42,KM=42)    !切断波数の設定
      PARAMETER(N=LM*(2*KM+1))  !スペクトル変数の配列の大きさ
      DIMENSION Z(N)            !渦度のスペクトル係数の配列
      DIMENSION W(N,3)          !Runge-Kutta法のための作業領域
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kutta法が呼び出すサブルーチン

      NSTEP=200                 !時間発展するステップ数
      H=1D0                     !グラフィックス表示のための時間間隔
      M=5                       !Runge-Kuttaでのステップ分割数

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
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(JM=64,IM=128)    !格子点数の設定(JM: y方向, IM: x方向)
      PARAMETER(LM=42,KM=42)     !切断波数の設定
                                 !(メインプログラムに合わせること)
      DIMENSION Z(-KM:KM,LM)     !渦度のスペクトル係数の配列
      DIMENSION DZ(-KM:KM,LM)    !非線形項のスペクトル係数の配列
      DIMENSION WS(-KM:KM,0:LM)  !作業領域
      DIMENSION WG(0:JM,0:IM-1,3) !作業領域
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)
                                !C2PACKで使われる配列
      DIMENSION DL(-KM:KM,LM)   !粘性効果のために使われる配列
                                
      REAL RG(0:IM,0:JM)        !グラフィックスに使われる配列
      REAL DLEV,TLEV1,TLEV2     !グラフィックスに使われる変数
      SAVE

* 与えられた引数から, SBGDZNで使われる配列DL(粘性による減衰効果を与える)
* を設定する. なお, SBGDZNはRunge-Kuttaの内部で呼び出されるが, この際,
* 常にDT=DELTAT/2で呼び出されるから, それを考慮に入れると, DLは以下のよ
* うに設定できる.

      DO L=1,LM
        DO K=-KM,KM
          DL(K,L)=EXP(-DNU*DELTAT/2*(1D0*(K*K+L*L))**NV)
        END DO
      END DO

* C2PACKの初期化

      CALL C2INIT(JM,IM,ITJ,TJ,ITI,TI)

* グラフィックスの初期化

      CALL SWISET('IWIDTH', 500)
      CALL SWISET('IHEIGHT',300)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL SLRAT(1.0,0.6)
      CALL UDGCLA(0.0,1.0,0.1)

      IPAT0=25
      DLEV=0.05
      NLEV=20
      IDLEV=3

      DO I=1,NLEV
        IPAT=(IPAT0+I*IDLEV)*1000+999
        IF(I.EQ.1) THEN
          TLEV1=999.0
        ELSE
          TLEV1=(I-1)*DLEV
        END IF
        IF(I.EQ.NLEV) THEN
          TLEV2=999.0
        ELSE
          TLEV2=I*DLEV
        END IF
        CALL UESTLV(TLEV1,TLEV2,IPAT)
      END DO

      RETURN
*----------------------------------------------------------------------
*     渦度のスペクトル係数(Z)の初期化
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

* ここでは, 不安定な渦層に微小擾乱を加えた場を与え, それをC2G2SAを
* 使ってスペクトル係数に変換して, それをZの初期値としている.

      X1=PI
      Y1=PI/2
      SIGMA=0.1D0
      AMP=1D-2

      DO I=0,IM-1
        X=2*PI*I/IM
        DO J=0,JM
          Y=PI*J/JM
          WG(J,I,1)=SIN(Y)**41D0
     &      + AMP*EXP(-((X-X1)**2+(Y-Y1)**2)/(2*SIGMA**2))
        END DO
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG,Z,WG(0,0,3),ITJ,TJ,ITI,TI,1)

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および渦度のスペクトル係数(Z)を入力として, 非線形項のスペ
*     クトル係数の配列(DZ)を求めるサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL C2AJBS(LM,KM,JM,IM,1D0,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および時間刻み(DT)を入力として, 線形(粘性)項によるZの
*     T→T+DTにおける発展を解くサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO L=1,LM
        DO K=-KM,KM
          Z(K,L)=DL(K,L)*Z(K,L)
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     Zに対応する渦度場をグラフィックス表示する.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,Z)

      WRITE(CTIME,'(F5.1)') T

      CALL C2S2GA(LM,KM,JM,IM,Z,WG,WG(0,0,3),ITJ,TJ,ITI,TI,1)

      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)
        END DO
      END DO

      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.10,0.50)
      CALL SGSWND(0.0,2.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UETONE(RG,IM+1,IM+1,JM+1)
      CALL USDAXS
      CALL UXSTTL('T','T='//CTIME,1.0)

      RETURN
*----------------------------------------------------------------------
*     終了処理
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
