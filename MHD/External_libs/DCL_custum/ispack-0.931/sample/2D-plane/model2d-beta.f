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
*   model2d-beta.f: n2packを使って2次元非発散渦度方程式を解くプログラム
*
*                                                 2000/02/14 K.Ishioka
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
      H=0.1D0                   !グラフィックス表示のための時間間隔
      M=5                       !Runge-Kuttaでのステップ分割数

      NV=5                      !高階粘性項のラプラシアンの階数
      DNU=1D-15                 !高階粘性係数
      DELTAT=H/M                !Runge-Kutta内部の時間刻み

* 初期化

      CALL SBINIT(NV,DNU,DELTAT)
      CALL SBINIZ(Z)

* 時間発展およびグラフィックス表示

      T=0                       !Tは時刻を表す変数
      CALL SBGRPH(T,Z)
*      CALL SBSPEC(T,Z)

      DO ISTEP=1,NSTEP
        CALL TDRKNU(N,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGRPH(T,Z) 
*        CALL SBSPEC(T,Z)
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
*      PARAMETER(LM=63,KM=63)   !切断波数の設定(aliasingをとっていない)
      PARAMETER(LM=42,KM=42)    !切断波数の設定(aliasingをとる場合)
      PARAMETER(K0=7,GAMMA=18) !初期場のエネルギースペクトルを決める定数
      PARAMETER(BETA=200)        ! β効果
      DIMENSION Z(-LM:LM,-KM:KM) !渦度のスペクトル係数の配列
      DIMENSION DZ(-LM:LM,-KM:KM) !非線形項のスペクトル係数の配列
      DIMENSION WS(-LM:LM,-KM:KM) !作業領域
      DIMENSION WG(0:JM-1,0:IM-1,3) !作業領域
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2) !N2PACKで使われる配列
      DIMENSION DL(-LM:LM,-KM:KM) !粘性効果のために使われる配列
      DIMENSION DBS(-LM:LM,KM),DBC(-LM:LM,KM)
                                 !β効果のために使われる配列
      DIMENSION ENE(KM+LM),DIST(KM+LM)      !初期値設定に使われる配列
      REAL RG(0:IM,0:JM)        !グラフィックスに使われる配列
      REAL DLEV,TLEV1,TLEV2     !グラフィックスに使われる変数
      REAL RENE(KM),RK(KM)
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

* 与えられた引数から, SBGDZNで使われる配列
*      DBS, DBC(β効果による位相の西進効果を与える)
* を設定する. なお, SBGDZNはRunge-Kuttaの内部で呼び出されるが, この際,
* 常にDT=DELTAT/2で呼び出されるから, それを考慮に入れると, DBS,DBC
* は以下のように設定できる.

      DO K=1,KM
        DO L=-LM,LM
          ALPHA=BETA*DELTAT/2*K/(K*K+L*L)
          DBC(L,K)=COS(ALPHA)
          DBS(L,K)=SIN(ALPHA)
        END DO
      END DO

* N2PACKの初期化

      CALL N2INIT(JM,IM,ITJ,TJ,ITI,TI)

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

      IPAT0=55
      DLEV=3
      IDLEV=3
      NLEV=10

      IPAT=(IPAT0-NLEV*IDLEV)*1000+999
      TLEV2=(-NLEV+0.5D0)*DLEV
      CALL UESTLV(-999.0,TLEV2,IPAT)
      
      IPAT=(IPAT0+NLEV*IDLEV)*1000+999
      TLEV1=(NLEV+0.5D0)*DLEV
      CALL UESTLV(TLEV1,999.0,IPAT)
      
      DO I=-NLEV,NLEV
        IPAT=(IPAT0+I*IDLEV)*1000+999
        TLEV1=(I-0.5D0)*DLEV
        TLEV2=(I+0.5D0)*DLEV
        CALL UESTLV(TLEV1,TLEV2,IPAT)
      END DO

      RETURN
*----------------------------------------------------------------------
*     渦度のスペクトル係数(Z)の初期化
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

* |K|=k0 を中心とする波数にエネルギーピークを与える.
* 同じ|K|に対する位相は正規乱数を使ってランダムに与える.

      CALL BSSET0(KM+LM,DIST)
      SDIST=0
      DO K=1,KM
        DIST(K)=(1D0*K)**(GAMMA/2)/(1D0*K+K0)**GAMMA
        SDIST=SDIST+DIST(K)
      END DO
      DO K=1,KM
        DIST(K)=DIST(K)/SDIST
      END DO

      ISEED=0 !乱数の種
      DO K=-KM,KM
        DO L=-LM,LM
          CALL ISNORM(ISEED,Z(L,K))
        END DO
      END DO

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+Z(L,K)*Z(L,K)/(K*K+L*L)
          END IF
        END DO
      END DO

      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            Z(L,K)=Z(L,K)/SQRT(ENE(KT)/DIST(KT))
          END IF
        END DO
      END DO

      Z(0,0)=0                  !渦度の平均は0でなければならないから

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および渦度のスペクトル係数(Z)を入力として, 非線形項のスペ
*     クトル係数の配列(DZ)を求めるサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL N2AJBS(LM,KM,JM,IM,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

*     もしβ効果を非線形項の計算と一緒に加える場合には以下のようにする.
*      (その場合は, SBGDZL のβ効果の部分を外すこと)
*      
*      DO K=-KM,KM
*        DO L=-LM,LM
*          IF(K.NE.0.OR.L.NE.0) THEN
*            DZ(L,K)=DZ(L,K)-BETA*Z(-L,-K)*K/(K*K+L*L)
*          END IF
*        END DO
*      END DO

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および時間刻み(DT)を入力として, 線形(粘性)項によるZの
*     T→T+DTにおける発展を解くサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

* β効果の部分      

      DO K=1,KM
        DO L=-LM,LM
          TMP=DBS(L,K)*Z(L,K)+DBC(L,K)*Z(-L,-K)
          Z(L,K)=DBC(L,K)*Z(L,K)-DBS(L,K)*Z(-L,-K)
          Z(-L,-K)=TMP
        END DO
      END DO

* 高階粘性効果の部分      

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

      CALL N2S2GA(LM,KM,JM,IM,Z,WG,WG(0,0,3),ITJ,TJ,ITI,TI)

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
      CALL USDAXS
      CALL UXSTTL('T','T='//CTIME,1.0)

      RETURN
*----------------------------------------------------------------------
*     Zに対応するエネルギースペクトルをグラフィックス表示する.
*----------------------------------------------------------------------
      ENTRY SBSPEC(T,Z)

      WRITE(CTIME,'(F5.1)') T

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+Z(L,K)*Z(L,K)/(K*K+L*L)
          END IF
        END DO
      END DO

      ENET=0
      DO K=1,KM+LM
        ENET=ENET+ENE(K)
      END DO

      DO K=1,KM
        RK(K)=K
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-6,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      
      CALL UXSTTL('T','T='//CTIME,1.0)

      print *,'TIME=',T,' TOTAL ENERGY=',ENET

      RETURN
*----------------------------------------------------------------------
*     終了処理
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
********************************************************************
      SUBROUTINE ISNORM(ISEED,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DATA IFLAG/0/
      SAVE

      IF(IFLAG.EQ.0) THEN
        CALL ISRAND(ISEED,X1)
        CALL ISRAND(ISEED,X2)
        Y1=SQRT(-2*LOG(X1))*COS(2*PI*X2)
        Y2=SQRT(-2*LOG(X1))*SIN(2*PI*X2)
        R=Y1
        IFLAG=1
      ELSE
        R=Y2
        IFLAG=0
      END IF

      END
********************************************************************
      SUBROUTINE ISRAND(ISEED,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(IA=1664525,IC=1013904223,AM=2D0**32)

      ISEED=IA*ISEED+IC
      R=(IBCLR(ISEED,31)+0.5D0)/AM

      IF(BTEST(ISEED,31)) THEN
        R=R+0.5D0
      END IF

      END
