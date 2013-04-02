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
*   shallow2d.f: p2packを使ってf平面浅水方程式を解くプログラム
*
*                                                 2001/07/23 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* パラメターの設定 

      PARAMETER(LM=42,KM=42)    !切断波数の設定
      PARAMETER(N=(2*LM+1)*(2*KM+1)) !スペクトル変数の配列の大きさ
      DIMENSION VAR(N,3)        !変数のスペクトル係数の配列
      DIMENSION W(N*3,3)        !Runge-Kutta法のための作業領域
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kutta法が呼び出すサブルーチン

      NSTEP=200                 !時間発展するステップ数
*      H=1D0                     !グラフィックス表示のための時間間隔      
*      M=3                       !Runge-Kuttaでのステップ分割数      
      H=0.1D0                   !グラフィックス表示のための時間間隔
      M=5                       !Runge-Kuttaでのステップ分割数
      
      NV=5                      !高階粘性項のラプラシアンの階数
      DNU=1D-14                 !高階粘性係数
      DELTAT=H/M                !Runge-Kutta内部の時間刻み

* 初期化

      CALL SBINIT(NV,DNU,DELTAT)
      
*      CALL SBINIA(VAR) ! 2つの孤立渦の初期値(H=1, M=3程度が適当)
*                       ! BARPHI=0.05D0, F=0 などとしておくと面白い.
      CALL SBINIB(VAR) ! 乱流的初期値(H=0.1D0, M=5程度が適当)
                       ! BARPHI=15D0, F=10 などとしておくと面白い.
      
* 時間発展およびグラフィックス表示

      T=0                       !Tは時刻を表す変数
      CALL SBGDCK(T,VAR)      
      CALL SBGRPH(T,VAR)
      CALL SBSPEC(T,VAR)

      CALL APTIME(TIM1)
      DO ISTEP=1,NSTEP
        CALL TDRKNU(N*3,M,H,T,VAR,W,SBGDZL,SBGDZN)
        CALL SBGRPH(T,VAR)        
        CALL SBGDCK(T,VAR)
        CALL SBSPEC(T,VAR)
      END DO
      CALL APTIME(TIM2)
      print *,'TIME=',TIM2-TIM1

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
      PARAMETER(JM=128,IM=128)  !格子点数の設定(JM: y方向, IM: x方向)
      PARAMETER(LM=42,KM=42)    !切断波数の設定
      PARAMETER(K0=7,GAMMA=18)  !初期場のエネルギースペクトルを決める定数
*      PARAMETER(F=0)            ! 系の回転効果(F)                  
*      PARAMETER(BARPHI=0.05D0)  ! 平均水深      
      PARAMETER(F=10)           ! 系の回転効果(F)
      PARAMETER(R=1)    !x方向とy方向のアスペクト比      
      PARAMETER(BARPHI=15D0)    ! 平均水深
      
      PARAMETER(ISPLIT=1)       ! 重力波を分離して扱うかどうかのフラグ
      DIMENSION VAR(-LM:LM,-KM:KM,3)  !渦度のスペクトル係数の配列
      DIMENSION DVAR(-LM:LM,-KM:KM,3) !非線形項のスペクトル係数の配列
      DIMENSION WS(-LM:LM,-KM:KM)   !作業領域
      DIMENSION WG(0:JM-1,0:IM-1,4)   !作業領域
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2) !P2PACKで使われる配列
      DIMENSION DL(-LM:LM,-KM:KM)     !粘性効果のために使われる配列
      DIMENSION DB(-LM:LM,-KM:KM,3,3)
                                      !線形項の発展のために使われる配列
      DIMENSION ENE(KM+LM),DIST(KM+LM)      !初期値設定に使われる配列

      PARAMETER(NBR=(((JM+3)*(IM+3)+15)/16+1)*3) !UDCNTZのための定数
      REAL RG(0:IM,0:JM),IBR(NBR)   !グラフィックスに使われる配列
      REAL TLEV1,TLEV2         !グラフィックスに使われる変数
      REAL RENE(KM),RK(KM)          !グラフィックスに使われる配列
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

* 与えられた引数から, SBGDZNで使われる配列 DB(重力波の位相変化を与える)
* を設定する. なお, SBGDZNはRunge-Kuttaの内部で呼び出されるが, この際,
* 常にDT=DELTAT/2で呼び出されるから, それを考慮に入れると, DB は以下の
* ように設定できる.

      DO K=0,KM
        DO L=0,LM
          K2=K*K+L*L
          SIGMA2=BARPHI*K2+F*F
          IF(SIGMA2.EQ.0) SIGMA2=1
          SIGMA=SQRT(SIGMA2)
          FRAC=1/SIGMA2
          ALPHA=(DELTAT/2)*SIGMA
          CA=COS(ALPHA)
          SA=SIN(ALPHA)
          DB(L,K,1,1)=FRAC*(K2*BARPHI+F*F*CA)
          DB(L,K,1,2)=FRAC*(-SIGMA*F*SA)
          DB(L,K,1,3)=FRAC*(-F*K2+F*K2*CA)
          DB(L,K,2,1)=FRAC*(SIGMA*F*SA)
          DB(L,K,2,2)=FRAC*(SIGMA2*CA)
          DB(L,K,2,3)=FRAC*(SIGMA*K2*SA)
          DB(L,K,3,1)=FRAC*(-F*BARPHI+F*BARPHI*CA)
          DB(L,K,3,2)=FRAC*(-SIGMA*BARPHI*SA)
          DB(L,K,3,3)=FRAC*(F*F+K2*BARPHI*CA)
        END DO
      END DO

      DO IV1=1,3
        DO IV2=1,3
          DB(0,0,IV1,IV2)=0
        END DO
        DB(0,0,IV1,IV1)=1
      END DO

      DO IV1=1,3
        DO IV2=1,3
          DO K=0,KM
            DO L=0,LM
              DB(-L,K,IV1,IV2)=DB(L,K,IV1,IV2)
              DB(-L,-K,IV1,IV2)=DB(L,K,IV1,IV2)
              DB(L,-K,IV1,IV2)=DB(L,K,IV1,IV2)
            END DO
          END DO
        END DO
      END DO

* P2PACKの初期化

      CALL P2INIT(JM,IM,ITJ,TJ,ITI,TI)

* グラフィックスの初期化

      CALL SWISET('IWIDTH',  900)
      CALL SWISET('IHEIGHT', 600)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)      
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL SLDIV('Y',3,2)
      CALL SLRAT(1.0,1.0)

      RETURN
*----------------------------------------------------------------------
*     スペクトル係数の初期化(2つの孤立渦)
*----------------------------------------------------------------------
      ENTRY SBINIA(VAR)

* |K|=k0 を中心とする波数にエネルギーピークを与える.
* 同じ|K|に対する位相は正規乱数を使ってランダムに与える.

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

      CALL P2G2SA(LM,KM,JM,IM,WG,VAR,WG(0,0,3),ITJ,TJ,ITI,TI)

      VAR(0,0,1)=F
      
* 傾度風バランスによる初期値化をするとき
      CALL P2SWBL(LM,KM,JM,IM,R,BARPHI,VAR(-LM,-KM,1),VAR(-LM,-KM,3),
     &  WS,WG,ITJ,TJ,ITI,TI)
      
* そうでないとき
*      VAR(0,0,3)=BARPHI

* UETONE のレベル設定      

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
*     スペクトル係数の初期化(乱流状態)      
*----------------------------------------------------------------------
      ENTRY SBINIB(VAR)

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
          CALL ISNORM(ISEED,VAR(L,K,1))
        END DO
      END DO

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=INT(SQRT(1D0*K*K+L*L)+0.5D0)
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,1)*VAR(L,K,1)/(K*K+L*L)
          END IF
        END DO
      END DO

      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            VAR(L,K,1)=VAR(L,K,1)/SQRT(ENE(KT))*SQRT(DIST(KT))
          END IF
        END DO
      END DO

      VAR(0,0,1)=F
      
* 傾度風バランスによる初期値化をするとき
      CALL P2SWBL(LM,KM,JM,IM,R,BARPHI,VAR(-LM,-KM,1),VAR(-LM,-KM,3),
     &  WS,WG,ITJ,TJ,ITI,TI)
      
* そうでないとき
*      VAR(0,0,3)=BARPHI

* UETONE のレベル設定      

      IPAT0=55
      DLEV=2
      NLEV=10
      IDLEV=3

      DO I=-NLEV,NLEV
        IPAT=(IPAT0+I*IDLEV)*1000+999
        IF(I.EQ.-NLEV) THEN
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
*     時刻(T)および渦度のスペクトル係数(Z)を入力として, 非線形項のスペ
*     クトル係数の配列(DZ)を求めるサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,VAR,DVAR)

      IF(ISPLIT.EQ.1) THEN
        CALL P2SWNN(LM,KM,JM,IM,R,BARPHI,F,
     &    VAR(-LM,-KM,1),VAR(-LM,-KM,2),VAR(-LM,-KM,3),
     &    DVAR(-LM,-KM,1),DVAR(-LM,-KM,2),DVAR(-LM,-KM,3),
     &    WS,WG,ITJ,TJ,ITI,TI)
      ELSE
        CALL P2SWNL(LM,KM,JM,IM,R,
     &    VAR(-LM,-KM,1),VAR(-LM,-KM,2),VAR(-LM,-KM,3),
     &    DVAR(-LM,-KM,1),DVAR(-LM,-KM,2),DVAR(-LM,-KM,3),
     &    WS,WG,ITJ,TJ,ITI,TI)
      END IF

      RETURN
*----------------------------------------------------------------------
*     保存量のチェック
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,VAR)

      CALL P2SWCK(LM,KM,JM,IM,R,
     &  VAR(-LM,-KM,1),VAR(-LM,-KM,2),VAR(-LM,-KM,3),AENE,AENS,
     &  WS,WG,ITJ,TJ,ITI,TI)

      PRINT *,'T=',T,' AENE=',AENE,' AENS=',AENS

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および時間刻み(DT)を入力として, 線形(粘性)項によるZの
*     T→T+DTにおける発展を解くサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,VAR)

* 重力波の部分      

      IF(ISPLIT.EQ.1) THEN
        DO K=-KM,KM
          DO L=-LM,LM
            Z1=VAR(L,K,1)
            Z2=VAR(L,K,2)
            Z3=VAR(L,K,3)
            VAR(L,K,1)=DB(L,K,1,1)*Z1+DB(L,K,1,2)*Z2+DB(L,K,1,3)*Z3
            VAR(L,K,2)=DB(L,K,2,1)*Z1+DB(L,K,2,2)*Z2+DB(L,K,2,3)*Z3
            VAR(L,K,3)=DB(L,K,3,1)*Z1+DB(L,K,3,2)*Z2+DB(L,K,3,3)*Z3
          END DO
        END DO
      END IF

* 高階粘性効果の部分      

      DO IV=1,3
        DO K=-KM,KM
          DO L=-LM,LM
            VAR(L,K,IV)=DL(L,K)*VAR(L,K,IV)
          END DO
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     Zに対応する渦度場をグラフィックス表示する.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,VAR)

      WRITE(CTIME,'(F5.1)') T
      
* 渦度場            

      CALL P2S2GA(LM,KM,JM,IM,VAR(-LM,-KM,1),
     &  WG,WG(0,0,2),ITJ,TJ,ITI,TI)
      DO I=0,IM-1
        DO J=0,JM-1
          RG(I,J)=WG(J,I,1)-F
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
      CALL UXSTTL('T','Vorticity  T='//CTIME,-1.0)

* 発散場                  

      CALL P2S2GA(LM,KM,JM,IM,VAR(-LM,-KM,2),
     &  WG,WG(0,0,2),ITJ,TJ,ITI,TI)
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

      CALL USDAXS
      CALL UXSTTL('T','Divergence',-1.0)      
      CALL UDCNTZ(RG,IM+1,IM+1,JM+1,IBR,NBR)

* ジオポテンシャル場                  

      CALL P2S2GA(LM,KM,JM,IM,VAR(-LM,-KM,3),
     &  WG,WG(0,0,2),ITJ,TJ,ITI,TI)
      DO I=0,IM-1
        DO J=0,JM-1
          RG(I,J)=WG(J,I,1)
          IF(WG(J,I,1).LE.0) THEN
            PRINT *, 'PHI IS NEGATIVE!! ',J,I,WG(J,I,1)
          END IF
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

      CALL USDAXS
      CALL UXSTTL('T','Geopotential',-1.0)
      CALL UDCNTZ(RG,IM+1,IM+1,JM+1,IBR,NBR)      

      RETURN
*----------------------------------------------------------------------
*     Zに対応するエネルギースペクトルをグラフィックス表示する.
*----------------------------------------------------------------------
      ENTRY SBSPEC(T,VAR)

      DO K=1,KM
        RK(K)=K
      END DO
      WRITE(CTIME,'(F5.1)') T

* 渦度場のパワースペクトル      

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,1)*VAR(L,K,1)
          END IF
        END DO
      END DO

      DO K=1,KM
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-20,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      CALL UXSTTL('T','Vorticity  T='//CTIME,-1.0)

* 発散場のパワースペクトル      

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,2)*VAR(L,K,2)
          END IF
        END DO
      END DO

      DO K=1,KM
        RENE(K)=ENE(K)
      END DO
      
      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-20,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      CALL UXSTTL('T','Divergence',-1.0)

* ジオポテンシャル場のパワースペクトル      

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,3)*VAR(L,K,3)
          END IF
        END DO
      END DO

      DO K=1,KM
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-20,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      CALL UXSTTL('T','Geopotential',-1.0)

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
