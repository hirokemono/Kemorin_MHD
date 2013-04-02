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
*   shallow.f: c2packを使って平面浅水方程式を解くプログラム
*                                                 2000/10/24 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* パラメターの設定 

      PARAMETER(LM=42,KM=42)    !切断波数の設定
      PARAMETER(N=(3*LM+2)*(2*KM+1)) !スペクトル変数の配列の大きさ
      DIMENSION VAR(N)        !変数のスペクトル係数の配列
      DIMENSION W(N*3)          !Runge-Kutta法のための作業領域
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kutta法が呼び出すサブルーチン

      NSTEP=300                 !時間発展するステップ数
      H=1D0                     !グラフィックス表示のための時間間隔
      M=3                       !Runge-Kuttaでのステップ分割数

      NV=5                      !高階粘性項のラプラシアンの階数
      DNU=1D-12                 !高階粘性係数
      DELTAT=H/M                !Runge-Kutta内部の時間刻み

* 初期化

      CALL SBINIT(NV,DNU,DELTAT)
      CALL SBINIA(VAR)          !2つの孤立渦の初期値
*      CALL SBINIB(VAR)          !不安定な渦層の初期値

* 時間発展およびグラフィックス表示

      T=0                       !Tは時刻を表す変数
      CALL SBGDCK(T,VAR)
      CALL SBGRPH(T,VAR)
      CALL SBSPEC(T,VAR)

      CALL APTIME(TIM1)
      DO ISTEP=1,NSTEP
        CALL TDRKNU(N,M,H,T,VAR,W,SBGDZL,SBGDZN)
        CALL SBGDCK(T,VAR)        
        CALL SBGRPH(T,VAR)
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
      PARAMETER(PI=3.1415926535897932385D0)      
      PARAMETER(JM=64,IM=128)    !格子点数の設定(JM: y方向, IM: x方向)
      PARAMETER(LM=42,KM=42)     !切断波数の設定
      PARAMETER(BARPHI=0.02D0)   !平均ジオポテンシャル
      PARAMETER(ISPLIT=1)        !重力波を分離して扱うかどうかのフラグ
      PARAMETER(IDL=LM+1,IHL=2*LM+2) !発散, ジオポテンシャルの先頭位置
      PARAMETER(RAT=1D0)         !アスペクト比
      DIMENSION VAR(-KM:KM,3*LM+2)  !渦度のスペクトル係数の配列
      DIMENSION DVAR(-KM:KM,3*LM+2) !非線形項のスペクトル係数の配列
      DIMENSION WS(-KM:KM,0:LM)     !作業領域
      DIMENSION WG(0:JM,0:IM-1,4)   !作業領域
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2) !C2PACKで使われる配列
      DIMENSION DL(-KM:KM,0:LM)     !粘性効果のために使われる配列
      DIMENSION DB(-KM:KM,0:LM,3)   !線形項の発展のために使われる配列
      DIMENSION ENE(KM+LM),DIST(KM+LM)      !初期値設定に使われる配列
      REAL RG(0:IM,0:JM)        !グラフィックスに使われる配列
      REAL DLEV,TLEV1,TLEV2     !グラフィックスに使われる変数
      REAL RENE(KM),RK(KM)      !グラフィックスに使われる配列
      SAVE

* 与えられた引数から, SBGDZNで使われる配列DL(粘性による減衰効果を与える)
* を設定する. なお, SBGDZNはRunge-Kuttaの内部で呼び出されるが, この際,
* 常にDT=DELTAT/2で呼び出されるから, それを考慮に入れると, DLは以下のよ
* うに設定できる.

      DO K=-KM,KM
        DO L=0,LM
          DL(K,L)=EXP(-DNU*DELTAT/2*(1D0*(RAT*K*RAT*K+L*L))**NV)
        END DO
      END DO

* 与えられた引数から, SBGDZNで使われる配列を設定する. これは重力波モード
* を分離して解くためのものである. なお, SBGDZNはRunge-Kuttaの内部で呼び
* 出されるが, この際, 常にDT=DELTAT/2で呼び出されるから, それを考慮に入
* れると, DBは以下のように設定できる.

      DO L=1,LM
        DO K=-KM,KM
          RK2=RAT*K*RAT*K+L*L
          SIGMA2=BARPHI*RK2
          SIGMA=SQRT(SIGMA2)
          ALPHA=(DELTAT/2)*SIGMA
          CA=COS(ALPHA)
          SA=SIN(ALPHA)
          DB(K,L,1)=CA
          DB(K,L,2)=SIGMA*SA/BARPHI
          DB(K,L,3)=-SIGMA*SA/RK2
        END DO
      END DO

      L=0
      DO K=1,KM
        RK2=RAT*K*RAT*K
        SIGMA2=BARPHI*RK2
        SIGMA=SQRT(SIGMA2)
        ALPHA=(DELTAT/2)*SIGMA
        CA=COS(ALPHA)
        SA=SIN(ALPHA)
        DB(K,L,1)=CA
        DB(K,L,2)=SIGMA*SA/BARPHI
        DB(K,L,3)=-SIGMA*SA/RK2
        DB(-K,L,1)=CA
        DB(-K,L,2)=SIGMA*SA/BARPHI
        DB(-K,L,3)=-SIGMA*SA/RK2
      END DO

      DB(0,0,1)=1
      DB(0,0,2)=0
      DB(0,0,3)=0

* C2PACKの初期化

      CALL C2INIT(JM,IM,ITJ,TJ,ITI,TI)

* グラフィックスの初期化

*      CALL SWISET('IWIDTH',  300)
      CALL SWISET('IWIDTH',  600)
      CALL SWISET('IHEIGHT', 540)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
*      CALL SLDIV('T',1,3)
      CALL SLDIV('T',2,3)
      CALL SLRAT(1.0,0.6)

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
*     2つの孤立渦の初期値を与える
*----------------------------------------------------------------------
      ENTRY SBINIA(VAR)

      CALL BSSET0((3*LM+2)*(2*KM+1),VAR)

      X1=PI-PI/4
      Y1=PI/2
      X2=PI+PI/4
      Y2=PI/2
      SIGMA=PI/16

      DO I=0,IM-1
        X=2*PI*I/IM
        DO J=0,JM
          Y=PI*J/JM
          WG(J,I,1)= EXP(-((X-X1)**2+(Y-Y1)**2)/(2*SIGMA**2))
     &              +EXP(-((X-X2)**2+(Y-Y2)**2)/(2*SIGMA**2))
        END DO
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG,VAR(-KM,1),WG(0,0,3),ITJ,TJ,ITI,TI,1)
      CALL C2SWBL(LM,KM,JM,IM,RAT,BARPHI,VAR(-KM,1),VAR(-KM,IHL),WS,WG,
     &  ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     不安定な渦層の初期値を与える
*----------------------------------------------------------------------
      ENTRY SBINIB(VAR)

      CALL BSSET0((3*LM+2)*(2*KM+1),VAR)

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

      CALL C2G2SA(LM,KM,JM,IM,WG,VAR(-KM,1),WG(0,0,3),ITJ,TJ,ITI,TI,1)
      CALL C2SWBL(LM,KM,JM,IM,RAT,BARPHI,VAR(-KM,1),VAR(-KM,IHL),WS,WG,
     &  ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および渦度のスペクトル係数(Z)を入力として, 非線形項のスペ
*     クトル係数の配列(DZ)を求めるサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,VAR,DVAR)

      IF(ISPLIT.EQ.1) THEN
        CALL C2SWNN(LM,KM,JM,IM,RAT,BARPHI,
     &    VAR(-KM,1),VAR(-KM,IDL),VAR(-KM,IHL),
     &    DVAR(-KM,1),DVAR(-KM,IDL),DVAR(-KM,IHL),
     &    WS,WG,ITJ,TJ,ITI,TI)
      ELSE
        CALL C2SWNL(LM,KM,JM,IM,RAT,
     &    VAR(-KM,1),VAR(-KM,IDL),VAR(-KM,IHL),
     &    DVAR(-KM,1),DVAR(-KM,IDL),DVAR(-KM,IHL),
     &    WS,WG,ITJ,TJ,ITI,TI)
      END IF

      RETURN
*----------------------------------------------------------------------
*     保存量のチェック
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,VAR)

      CALL C2SWCK(LM,KM,JM,IM,RAT,
     &  VAR,VAR(-KM,IDL),VAR(-KM,IHL),AENE,AENS,AMOM,
     &  WS,WG,ITJ,TJ,ITI,TI)

      PRINT *,'T=',T,' AENE=',AENE,' AENS=',AENS,' AMOM=',AMOM

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および時間刻み(DT)を入力として, 線形(粘性)項によるZの
*     T→T+DTにおける発展を解くサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,VAR)

* 重力波の部分      

      IF(ISPLIT.EQ.1) THEN
        DO L=0,LM        
          DO K=-KM,KM
            TMPVAR=VAR(K,IDL+L)
            VAR(K,IDL+L)=DB(K,L,1)*TMPVAR+DB(K,L,2)*VAR(K,IHL+L)
            VAR(K,IHL+L)=DB(K,L,3)*TMPVAR+DB(K,L,1)*VAR(K,IHL+L)            
          END DO
        END DO
      END IF

* 高階粘性効果の部分      

      DO L=1,LM
        DO K=-KM,KM
          VAR(K,L)=DL(K,L)*VAR(K,L)
        END DO
      END DO
      DO L=0,LM
        DO K=-KM,KM
          VAR(K,IDL+L)=DL(K,L)*VAR(K,IDL+L)
          VAR(K,IHL+L)=DL(K,L)*VAR(K,IHL+L)
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     VARに対応する場をグラフィックス表示する.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,VAR)

      WRITE(CTIME,'(F5.1)') T

* 渦度場            

      CALL C2S2GA(LM,KM,JM,IM,VAR,WG,WG(0,0,2),ITJ,TJ,ITI,TI,1)
      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)
        END DO
      END DO
      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.13,0.53)
      CALL SGSWND(0.0,2.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UETONE(RG,IM+1,IM+1,JM+1)
      CALL USDAXS
      CALL UXSTTL('T','Vorticity  T='//CTIME,-1.0)

* 発散場      

      CALL C2S2GA(LM,KM,JM,IM,VAR(-KM,IDL),WG,WG(0,0,2),ITJ,TJ,ITI,TI,2)
      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)
        END DO
      END DO
      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.13,0.53)
      CALL SGSWND(0.0,2.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL USDAXS
      CALL UXSTTL('T','Divergence',-1.0)      
      CALL UDCNTR(RG,IM+1,IM+1,JM+1)      

* ジオポテンシャル場

      CALL C2S2GA(LM,KM,JM,IM,VAR(-KM,IHL),WG,WG(0,0,2),ITJ,TJ,ITI,TI,2)
      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)
        END DO
      END DO
      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.13,0.53)
      CALL SGSWND(0.0,2.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL USDAXS
      CALL UXSTTL('T','Geopotential',-1.0)
      CALL UDCNTR(RG,IM+1,IM+1,JM+1)      

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
        DO L=1,LM
          KT=SQRT(1D0*RAT*K*RAT*K+L*L)+0.5D0
          FACT=1D0/2
          IF(K.EQ.0) THEN
            FACT=FACT/2
          END IF
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+FACT*VAR(K,L)*VAR(K,L)
          END IF
        END DO
      END DO
      DO K=1,KM
        RENE(K)=ENE(K)
      END DO      

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.08,0.53)      
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
        DO L=0,LM
          KT=SQRT(1D0*RAT*K*RAT*K+L*L)+0.5D0
          FACT=1D0/2
          IF(K.EQ.0) THEN
            FACT=FACT/2
          END IF
          IF(L.EQ.0) THEN
            FACT=FACT*2
          END IF
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+FACT*VAR(K,IDL+L)*VAR(K,IDL+L)
          END IF
        END DO
      END DO
      DO K=1,KM
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.08,0.53)      
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
        DO L=1,LM
          KT=SQRT(1D0*RAT*K*RAT*K+L*L)+0.5D0
          FACT=1D0/2
          IF(K.EQ.0) THEN
            FACT=FACT/2
          END IF
          IF(L.EQ.0) THEN
            FACT=FACT*2
          END IF
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+FACT*VAR(K,IHL+L)*VAR(K,IHL+L)
          END IF
        END DO
      END DO
      DO K=1,KM
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.08,0.53)
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
