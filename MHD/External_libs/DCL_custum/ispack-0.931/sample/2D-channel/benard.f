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
*     convection.f: c2packを使ってベナール対流を計算するプログラム
*                                                  2000/10/16 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* パラメターの設定 

      PARAMETER(LM=42,KM=42)    !切断波数の設定
      PARAMETER(N=LM*(2*KM+1)*2)  !スペクトル変数の配列の大きさ
      DIMENSION Z(N)            !渦度のスペクトル係数の配列
      DIMENSION W(N,3)          !Runge-Kutta法のための作業領域
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kutta法が呼び出すサブルーチン

      NSTEP=200                 !時間発展するステップ数
      H=10D0                     !グラフィックス表示のための時間間隔
      M=2                       !Runge-Kuttaでのステップ分割数      

      DELTAT=H/M                !Runge-Kutta内部の時間刻み

* 初期化

      CALL SBINIT(DELTAT)
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
      SUBROUTINE SBINIT(DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CTIME*6
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(JM=64,IM=128)    !格子点数の設定(JM: y方向, IM: x方向)
      PARAMETER(LM=42,KM=42)     !切断波数の設定
      PARAMETER(ISPLIT=0)        !モード分解して扱うかどうかのフラグ
                                 !1なら分離する. それ以外なら分離しない. 
      DIMENSION Z(-KM:KM,LM,2)   !渦度のと温度スペクトル係数の配列
      DIMENSION DZ(-KM:KM,LM,2)  !非線形項のスペクトル係数の配列
      DIMENSION WS(-KM:KM,0:LM)  !作業領域
      DIMENSION WG(0:JM,0:IM-1,4) !作業領域
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2) !C2PACKで使われる配列
      DIMENSION DL(-KM:KM,LM,2,2) !固有モードの発展に使われる配列
      DIMENSION DIFF(-KM:KM,LM,2) !粘性効果のために使われる配列      
      REAL RG(0:IM,0:JM)        !グラフィックスに使われる配列
      REAL DLEV,TLEV1,TLEV2     !グラフィックスに使われる変数
      SAVE

*--------------------------------------------------
* 実験パラメターの設定. 物性値はとりあえず水のものをCGS単位で与えてある.
*      
      ALPHA=2.1D-4              !体積膨張率
      GRAV=980D0                !重力加速度
      DKAPPA=1.4D-3             !熱拡散係数
      DNU=1D-2                  !粘性係数
      DEPTH=1D0                 !水深
      DTEMP=0.1D0               !上端と下端の温度差
*      DTEMP=0.05D0               !上端と下端の温度差      
*      DTEMP=0.0448D0             !上端と下端の温度差(クリティカルな値)
      DLENG=2*SQRT(2D0)*DEPTH   !水平方向の周期
*--------------------------------------------------

      RAT=2*DEPTH/DLENG

      DKAPPB=DKAPPA*(PI/DEPTH)*(PI/DEPTH)
      DNUD=DNU*(PI/DEPTH)*(PI/DEPTH)      

      COEF1=ALPHA*GRAV*2*PI/DLENG
      COEF2=DTEMP/PI*RAT

      RAYLGH=ALPHA*GRAV*DTEMP*DEPTH**3/(DNU*DKAPPA)
      RC=PI**4*27/4

      WRITE(6,*) 'Rayleigh Number=',RAYLGH
      WRITE(6,*) 'Critical Rayleigh Number=',RC

* 与えられた引数から, SBGDZNで使われる配列DL(粘性による減衰効果を与える)
* を設定する. なお, SBGDZNはRunge-Kuttaの内部で呼び出されるが, この際,
* 常にDT=DELTAT/2で呼び出されるから, それを考慮に入れると, DLは以下のよ
* うに設定できる.

      IF(ISPLIT.EQ.1) THEN
        DO K=-KM,KM
          DO L=1,LM
            RK=RAT*K
            A=-DNUD*(RK*RK+L*L)
            B=-ALPHA*GRAV*2*PI/DLENG*K
            C=-DTEMP/PI*RK/(RK*RK+L*L)
            D=-DKAPPB*(RK*RK+L*L)
            SIGMA1=(A+D-SQRT((A-D)*(A-D)+4*B*C))/2
            SIGMA2=(A+D+SQRT((A-D)*(A-D)+4*B*C))/2
            ESIG1=EXP(SIGMA1*DELTAT/2)
            ESIG2=EXP(SIGMA2*DELTAT/2)
            SIG21=SIGMA2-SIGMA1
*          WRITE(6,*) K,SIGMA2
            DL(K,L,1,1)=((SIGMA2-A)*ESIG1+(A-SIGMA1)*ESIG2)/SIG21
            DL(K,L,1,2)=B/SIG21*(-ESIG1+ESIG2)
            DL(K,L,2,1)=C/SIG21*(-ESIG1+ESIG2)          
            DL(K,L,2,2)=((SIGMA2-D)*ESIG1+(D-SIGMA1)*ESIG2)/SIG21
          END DO
        END DO
      ELSE
        DO K=-KM,KM
          DO L=1,LM
            DIFF(K,L,1)=EXP(-DNUD*DELTAT/2*((RAT*K)*(RAT*K)+L*L))
            DIFF(K,L,2)=EXP(-DKAPPB*DELTAT/2*((RAT*K)*(RAT*K)+L*L))          
          END DO
        END DO
      END IF

* C2PACKの初期化

      CALL C2INIT(JM,IM,ITJ,TJ,ITI,TI)

* グラフィックスの初期化

      CALL SWISET('IWIDTH',  400)
      CALL SWISET('IHEIGHT', 400)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL SLDIV('T',1,2)
      CALL SLRAT(1.0,0.5)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)

      IPAT0=25
      DLEV=0.005
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

* ここでは, 不安定な渦層に微小擾乱を加えた場を与え,
* それをC2G2SAを使ってスペクトル係数に変換して, それをZの初期値と
* している.

      CALL BSSET0((2*KM+1)*LM*2,Z)
      CALL BSSET0(IM*(JM+1),WG)
      
      WG(JM/2,IM/2,1)=1D-6
      CALL C2G2SA(LM,KM,JM,IM,WG,Z(-KM,1,2),WG(0,0,3),ITJ,TJ,ITI,TI,1)

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および渦度のスペクトル係数(Z)を入力として, 非線形項のスペ
*     クトル係数の配列(DZ)を求めるサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL C2AJB2(LM,KM,JM,IM,RAT,Z,Z(-KM,1,2),DZ,DZ(-KM,1,2),
     &  WS,WG,ITJ,TJ,ITI,TI)

      IF(ISPLIT.NE.1) THEN
        DO L=1,LM
          DO K=-KM,KM
            DZ(K,L,1)=DZ(K,L,1)-COEF1*K*Z(-K,L,2)
            DZ(K,L,2)=DZ(K,L,2)-COEF2*K*
     &        (-Z(-K,L,1)/((RAT*K)*(RAT*K)+L*L))
          END DO
        END DO
      END IF

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および時間刻み(DT)を入力として, 線形(粘性)項によるZの
*     T→T+DTにおける発展を解くサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      IF(ISPLIT.EQ.1) THEN
        DO L=1,LM
          DO K=-KM,KM
            TMPZ1=Z(-K,L,1)
            Z(-K,L,1)=DL(K,L,1,1)*TMPZ1-DL(K,L,1,2)*Z(K,L,2)
            Z(K,L,2)=-DL(K,L,2,1)*TMPZ1+DL(K,L,2,2)*Z(K,L,2)
          END DO
        END DO
      ELSE
        DO L=1,LM
          DO K=-KM,KM
            Z(K,L,1)=DIFF(K,L,1)*Z(K,L,1)
            Z(K,L,2)=DIFF(K,L,2)*Z(K,L,2)
          END DO
        END DO
      END IF

      RETURN
*----------------------------------------------------------------------
*     Zに対応する場をグラフィックス表示する.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,Z)

      WRITE(CTIME,'(F6.1)') T

* 温度場の表示      

      CALL C2S2GA(LM,KM,JM,IM,Z(-KM,1,2),WG,WG(0,0,3),ITJ,TJ,ITI,TI,1)      

      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)+DTEMP/DEPTH*(JM-J)/JM
        END DO
      END DO

      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.08,0.08+1/(2*SQRT(2.0))*0.8)
      CALL SGSWND(0.0,2*SQRT(2.0),0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UETONE(RG,IM+1,IM+1,JM+1)
      CALL USDAXS
      CALL UXSTTL('T','Temperature',0.0)
      CALL UXSTTL('T','T='//CTIME,1.0)
      CALL UDCNTR(RG,IM+1,IM+1,JM+1)

* 流線関数場の表示        

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-Z(K,L,1)/((RAT*K)*(RAT*K)+L*L)          
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG,WG(0,0,3),ITJ,TJ,ITI,TI,1)

      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)
        END DO
      END DO

      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.15,0.15+1/(2*SQRT(2.0))*0.8)
      CALL SGSWND(0.0,2*SQRT(2.0),0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL USDAXS      
      CALL UDCNTR(RG,IM+1,IM+1,JM+1)
      CALL UXSTTL('T','Stream-Function',0.0)      

      RETURN
*----------------------------------------------------------------------
*     終了処理
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
