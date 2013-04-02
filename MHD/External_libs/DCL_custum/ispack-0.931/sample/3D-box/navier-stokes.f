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
*    navier-stokes.f:
*    P3PACKを使って3次元 Navier-Stokes 方程式を解くプログラム
*                                                  2002/03/16 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* パラメターの設定 

      PARAMETER(NM=10,MM=10,LM=10) ! 切断波数     
      PARAMETER(N=(2*LM+1)*(2*MM+1)*(2*NM+1)*2)
      PARAMETER(IU=10,NB=1024)
      DIMENSION Z(N)    ! 渦度ベクトルを格納する従属変数
      DIMENSION W(N,3)  ! 作業領域
      EXTERNAL SBGDZN,SBGDZL ! 時間変化を求めるサブルーチン

      NSTEP=10                 !時間発展するステップ数
      H=1D0                    !ファイル出力のための時間間隔
      M=20                     !Runge-Kuttaでのステップ分割数
      DELTAT=H/M               !Runge-Kutta内部の時間刻み
      DNU=1D-2                 !動粘性係数

* 初期化

      CALL FHUOPN(IU,'test.dat','W',NB)
      CALL SBINIT(DNU,DELTAT)
      CALL SBINIZ(Z)

* 時間発展およびファイル出力

      T=0                       !Tは時刻を表す変数
      CALL SBGDCK(T,Z)
      CALL FEPUTD(IU,N,Z)
      
      DO ISTEP=1,NSTEP
        CALL TDRKNU(N,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGDCK(T,Z)
        CALL FEPUTD(IU,N,Z)        
      END DO

      CALL FHUCLS(IU)      

      END
***********************************************************************
*     時間積分等に必要な種々の処理をまとめたサブルーチンパッケージ
*----------------------------------------------------------------------
*     パッケージの初期化をするサブルーチン
*----------------------------------------------------------------------
      SUBROUTINE SBINIT(DNU,DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NM=10,MM=10,LM=10) ! main programと同じ値を与えること.
      PARAMETER(KM=32,JM=32,IM=32) ! 格子点のサイズ
      PARAMETER(IFLAG=1) ! 強制を与えるかどうかのフラグ
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)  ! 渦度ベクトルの2成分
      DIMENSION DZ(-NM:NM,-MM:MM,-LM:LM,2) ! Zの時間変化率
      DIMENSION DL(-NM:NM,-MM:MM,-LM:LM,2) !粘性項の効果を表す配列
      DIMENSION WS(-NM:NM,-MM:MM,-LM:LM) !作業領域
      DIMENSION W(KM*JM*IM*4)              !作業領域
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                           ! P3PACKで使われる配列      
      SAVE

* 粘性項の効果を表す配列の初期化

      DO L=-LM,LM
        DO M=-MM,MM
          DO N=-NM,NM
            DL(N,M,L,1)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
            DL(N,M,L,2)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))            
          END DO
        END DO
      END DO

      IF(IFLAG.EQ.1) THEN
        DO IV=1,2
          DO I=-1,1,2
            DL(0,0,I,IV)=1
            DL(0,I,0,IV)=1
            DL(I,0,0,IV)=1
          END DO
        END DO
      END IF

* P3PACKの初期化

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     従属変数(渦度ベクトルの2成分に対応)の初期化
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

      CALL BSSET0((2*NM+1)*(2*MM+1)*(2*LM+1)*2,Z)
      
      Z( 0, 0, 1, 1)= 1D0/4
      Z( 0, 0,-1, 1)= 1/(4*SQRT(3D0))
      Z( 0, 0, 1, 2)= -1/(2*SQRT(3D0))
      Z( 0, 0,-1, 2)= 0
      
      Z( 0, 1, 0, 1)= 1D0/4
      Z( 0,-1, 0, 1)= 1/(4*SQRT(3D0))
      Z( 0, 1, 0, 2)= -1/(2*SQRT(3D0))
      Z( 0,-1, 0, 2)= 0
      
      Z( 1, 0, 0, 1)= 1D0/4
      Z(-1, 0, 0, 1)= 1/(4*SQRT(3D0))
      Z( 1, 0, 0, 2)= -1/(2*SQRT(3D0))
      Z(-1, 0, 0, 2)= 0

      RETURN
*----------------------------------------------------------------------
*     非線形項による時間変化率の計算
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P3ELNL(NM,MM,LM,KM,JM,IM,Z,DZ,WS,W,ITK,TK,ITJ,TJ,ITI,TI)

      IF(IFLAG.EQ.1) THEN
        DO IV=1,2
          DO I=-1,1,2
            DZ(0,0,I,IV)=0
            DZ(0,I,0,IV)=0
            DZ(I,0,0,IV)=0
          END DO
        END DO
      END IF

      RETURN
*----------------------------------------------------------------------
*     粘性項による発展を解くサブルーチン
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO L=-LM,LM
        DO M=-MM,MM
          DO N=-NM,NM
            Z(N,M,L,1)=DL(N,M,L,1)*Z(N,M,L,1)
            Z(N,M,L,2)=DL(N,M,L,2)*Z(N,M,L,2)
          END DO
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     保存量のチェック(エネルギー(E)およびヘリシティ(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CNSV(NM,MM,LM,Z,E,H)
      WRITE(6,'(A,F5.2,2(A,F17.15))')
     &  'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H

      END
