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
************************************************************************
*     回転球面上のバロトロピック渦度方程式の時間発展          2010/02/28 
************************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CF*80
      PARAMETER(IU=11)      
      PARAMETER(MM=85) ! 切断波数. SBOPEN内でも同じ値を与えること.
      PARAMETER(LM=(MM+1)*(MM+1))
      PARAMETER(NB=1024*8)
      DIMENSION VRT(LM),W(LM*3)
      EXTERNAL SBDVRT,SBDISS

!---- 実験パラメターの設定 ---------------------------------------------

      CF='data.dat'             !データ出力ファイル名
      OMEGA=300                 !自転角速度Ω
      
      ISEED=1                   !乱数の種
      GAMMA=1000                !初期のエネルギー分布を与えるγ
      N0=50                     !初期のエネルギー分布を与える n_0

      LEV=10                    !高階粘性項のラプラシアンの階数
      ITM=100                   !時間発展するステップ数
      NDV=20                    !Runge-Kuttaでのステップ分割数            
      DT=0.1D0                  !ファイル出力の時間間隔      

      DNU=10D0/(1D0*(MM*(MM+1)-2))**LEV !高階粘性係数の設定

*---- OPEN FHPACK ------------------------------------------------------

      CALL SBOPEN(LEV,DNU,NDV,DT,OMEGA,ISEED,GAMMA,N0)
      CALL SBINIT(VRT)
      CALL FHUOPN(IU,CF,'W',NB)

*---- TIME EVOLUTION BY RUNGE-KUTTA METHOD -----------------------------

      I=0
      TIM=0
      CALL FEPUTD(IU,LM,VRT)
      CALL SBCHCK(I,VRT)

      DO I=1,ITM
        CALL TDRKNU(LM,NDV,DT,TIM,VRT,W,SBDISS,SBDVRT)
        CALL FEPUTD(IU,LM,VRT)
        CALL SBCHCK(I,VRT)
      END DO

*---- CLOSE FHPACK -----------------------------------------------------

      CALL FHUCLS(IU)

      END
************************************************************************
*     OPEN SUBROUTINE PACKAGE
************************************************************************
      SUBROUTINE SBOPEN(LEV,DNU,NDV,DT,OMEGAD,ISEEDD,GAMMAD,N0D)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=85,JM=128,IM=256,NM=MM+2)
      PARAMETER(LM=(MM+1)*(MM+1))
      DIMENSION PSI(LM),VRT(LM),DVRT(LM)
      DIMENSION DRN(LM),DIST(MM),PSIX(LM)
      DIMENSION SW1((MM+6)*MM+3),SW2((MM+6)*MM+3)      
      DIMENSION G1(IM*JM),G2(IM*JM),G3(IM*JM)
      DIMENSION IT(2,2),T(IM*3,2)      
      DIMENSION P(JM/2,MM+4),Q(JM/2,11),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION WS1(2*(MM+3)),WS2(2*(MM+3))
      DIMENSION WG((IM+2)*JM)
      DIMENSION W1((JM+1)*IM),W2((JM+1)*IM)
      DIMENSION C((MM+1)*(MM+1))
      DIMENSION D((MM+1)*(MM+1),2)            
      DIMENSION E((5*MM*(MM+1)+4)/2)
      SAVE

      SQRT05=SQRT(0.5D0)            

      OMEGA=OMEGAD
      ISEED=ISEEDD
      GAMMA=GAMMAD
      N0=N0D

      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)
      CALL SJINIC(MM,C)      
      CALL SJINID(MM,D)
      CALL SJINIE(MM,E)      

      SDIST=0
      DO N=2,MM
        DIST(N)=((SQRT(1D0*N/N0)+SQRT(1D0*N0/N))/2)**(-GAMMA)
        SDIST=SDIST+DIST(N)
      END DO
      DO N=2,MM
        DIST(N)=DIST(N)/SDIST
      END DO

      DTDNU=DNU*DT/(2*NDV)

      DRN(1)=1
      DO L=1,LM
        DRN(L)=EXP(-DTDNU*(ABS(D(L,1))-2)**LEV)
      END DO

      RETURN
*-----------------------------------------------------------------------
*     渦度場の時間変化率の計算
*-----------------------------------------------------------------------
      ENTRY SBDVRT(TIM,VRT,DVRT)

      CALL SJCLAP(MM,VRT,PSI,D,2)
      CALL SJCS2X(MM,PSI,PSIX)

      CALL SJABNL(MM,NM,IM,JM,PSI,DVRT,IT,T,P,Q,R,C,E,SW1,SW2,
     &    G1,G2,WS1,WS2,WG,W1,W2)

      DO L=1,LM
        DVRT(L)=-DVRT(L)-2*OMEGA*PSIX(L)
      END DO

      RETURN
*-----------------------------------------------------------------------
*     高階粘性項による減衰効果
*-----------------------------------------------------------------------
      ENTRY SBDISS(TIM,DTIM,VRT)

      DO L=1,LM
        VRT(L)=DRN(L)*VRT(L)
      END DO

      RETURN
*-----------------------------------------------------------------------
*     全エネルギー, 全エンストロフィーの値の出力
*-----------------------------------------------------------------------
      ENTRY SBCHCK(I,VRT)

      CALL SJCLAP(MM,VRT,PSI,D,2)

      ENE=0
      ENS=0
      DO L=3,MM+1
        ENE=ENE-D(L,2)*VRT(L)*VRT(L)
        ENS=ENS+VRT(L)*VRT(L)
      END DO
      DO L=MM+2,LM
        ENE=ENE-2*D(L,2)*VRT(L)*VRT(L)
        ENS=ENS+2*VRT(L)*VRT(L)
      END DO
      ENE=ENE/2
      ENS=ENS/2

      WRITE(6,'(A,I3,A,F21.15,A,F21.15)')
     &  'step=',I,' energy=',ENE,' enstrophy=',ENS

      RETURN
*-----------------------------------------------------------------------
*     渦度場のスペクトルの初期値の設定
*-----------------------------------------------------------------------
      ENTRY SBINIT(VRT)

      CALL BSSET0(LM,VRT)

      DO N=2,MM
        ENE=0
        CALL SJNM2L(MM,N,0,L)
        CALL APNORM(ISEED,VRT(L))
        ENE=ENE-0.5D0*D(L,2)*VRT(L)*VRT(L)
        DO M=1,N
          CALL SJNM2L(MM,N,M,L)
          CALL APNORM(ISEED,VRT(L))
          VRT(L)=SQRT05*VRT(L)
          ENE=ENE-D(L,2)*VRT(L)*VRT(L)          
          CALL SJNM2L(MM,N,-M,L)
          CALL APNORM(ISEED,VRT(L))
          VRT(L)=SQRT05*VRT(L)          
          ENE=ENE-D(L,2)*VRT(L)*VRT(L)
        END DO
        DO M=-N,N
          CALL SJNM2L(MM,N,M,L)
          VRT(L)=VRT(L)/SQRT(ENE/DIST(N))
        END DO
      END DO
      
      END
