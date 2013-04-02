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
*    P3PACK-MPIを使って3次元 Navier-Stokes 方程式を解くプログラム
*                                                  2002/05/07 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      

* パラメターの設定 

      PARAMETER(NM=10,MM=10,LM=10) ! 切断波数
      PARAMETER(NPROC=1) ! 並列実行時に用いるプロセス数のとりうる最小値
                         ! もし多数のプロセス数でしか計算しないなら
                         ! これをそのプロセス数に合わせることによって
                         ! 必要なメモリ量を削減できる.      
      PARAMETER(N=(2*MM+1)*(2*NM+1)*2*2*(LM/NPROC+1))
      DIMENSION Z(N)   ! 渦度ベクトルを格納する従属変数
      DIMENSION W(N*3) ! 作業領域
      EXTERNAL SBGDZN,SBGDZL

* MPIの初期化とプロセス番号, プロセス数の取得.
      
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.LT.NPROC) THEN
        WRITE(6,*) 'NPROC MUST .LE. NUMBER OF PROCESSES.'
        STOP
      END IF

* このプロセスに分散配置されるスペクトルデータのについて,
* L の個数(LC), L の絶対値の最小値(LS), L の絶対値の最大値(LE),
* および全データ数 ND を求める.

      LP=LM/NP+1
      LS=LP*IP
      LE=MIN(LP*(IP+1)-1,LM)
      IF(LE.GE.LS) THEN
        LC=LE-LS+1
      ELSE
        LC=0
        LS=0
        LE=0
      END IF

      IF(LS.EQ.0.AND.LC.NE.0) THEN
        ND=(2*LC-1)*(2*MM+1)*(2*NM+1)*2
      ELSE IF(LC.NE.0) THEN
        ND=(2*LC)*(2*MM+1)*(2*NM+1)*2
      ELSE
        ND=0
      END IF

* 時間発展および保存量のチェック

      NSTEP=10                 !時間発展するステップ数
      H=1D0                    !保存量のチェックのための時間間隔      
      M=20                     !Runge-Kuttaでのステップ分割数
      DELTAT=H/M               !Runge-Kutta内部の時間刻み
      DNU=1D-2                 !動粘性係数
      
* パッケージと従属変数の初期化

      CALL SBINIT(DNU,DELTAT)
      CALL SBINIZ(Z)

* 時間発展および保存量のチェック

      T=0                       !Tは時刻を表す変数
      CALL SBGDCK(T,Z)
      CALL SBPUTD(Z)
      DO ISTEP=1,NSTEP
        CALL TDRKNU(ND,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGDCK(T,Z)
        CALL SBPUTD(Z)
      END DO

* パッケージの終了

      CALL SBCLOS

* MPIの終了      

      CALL MPI_FINALIZE(IERR)      

      END
***********************************************************************
*     時間積分等に必要な種々の処理をまとめたサブルーチンパッケージ
*----------------------------------------------------------------------
*     パッケージの初期化をするサブルーチン
*----------------------------------------------------------------------
      SUBROUTINE SBINIT(DNU,DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'            
      PARAMETER(NM=10,MM=10,LM=10) ! main programと同じ値を与えること.
      PARAMETER(KM=32,JM=32,IM=32) ! 格子点のサイズ
      PARAMETER(IFLAG=1) ! 強制を与えるかどうかのフラグ
      PARAMETER(IU=10,NB=1024) ! 出力ファイル番号およびバッファ長
      PARAMETER(NPROC=1) ! 並列実行時に用いるプロセス数のとりうる最小値
                         ! もし多数のプロセス数でしか計算しないなら
                         ! これをそのプロセス数に合わせることによって
                         ! 必要なメモリ量を削減できる.      
      DIMENSION Z(-NM:NM,-MM:MM,2,0:*)  ! 渦度ベクトルの2成分
      DIMENSION DZ(-NM:NM,-MM:MM,2,0:*) ! Zの時間変化率
      DIMENSION DL(-NM:NM,-MM:MM,2,0:LM/NPROC*2+1)
                         ! 粘性の効果を格納する配列
      DIMENSION W(KM*IM*4*((JM-1)/NPROC+1)) ! 作業領域
      ! JM=IM の場合はこれでよいが, JM≠IM の場合は異なる大きさに
      ! しなければならない場合もあるので注意すること.
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                       ! P3PACKで使われる配列
      DIMENSION ISTAT(MPI_STATUS_SIZE) ! MPI_SEND,MPI_RECVで使われる配列
      SAVE

* プロセス番号, プロセス数の取得.      

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.LT.NPROC) THEN
        WRITE(6,*) 'NPROC MUST .LE. NUMBER OF PROCESSES.'
        STOP
      END IF

* このプロセスに分散配置されるスペクトルデータのについて,
* L の個数(LC), L の絶対値の最小値(LS), L の絶対値の最大値(LE),
* および全データ数 ND を求める.
* なお, LT は L が負の方のデータを参照するために使う値である.
* また, L2 は正負を含めた L の数の合計 -1 の値である.

      LP=LM/NP+1
      LS=LP*IP
      LE=MIN(LP*(IP+1)-1,LM)
      IF(LE.GE.LS) THEN
        LC=LE-LS+1
      ELSE
        LC=0
        LS=0
        LE=0
      END IF

      LT=2*LC-1+LS

      IF(LS.EQ.0.AND.LC.NE.0) THEN
        ND=(2*LC-1)*(2*MM+1)*(2*NM+1)*2
      ELSE IF(LC.NE.0) THEN
        ND=(2*LC)*(2*MM+1)*(2*NM+1)*2
      ELSE
        ND=0
      END IF

      IF(LS.EQ.0.AND.LC.NE.0) THEN
        L2=2*LC-2
      ELSE IF(LC.NE.0) THEN
        L2=2*LC-1
      ELSE
        L2=-1
      END IF

      IF(LC.GT.0) THEN
        DO L=LS,LE
          DO M=-MM,MM
            DO N=-NM,NM
              DL(N,M,1,L-LS)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
              DL(N,M,2,L-LS)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))            
              DL(N,M,1,LT-L)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
              DL(N,M,2,LT-L)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
            END DO
          END DO
        END DO
      END IF

      IF(IFLAG.EQ.1) THEN
        IF(LE.GE.1.AND.LS.LE.1) THEN
          DO IV=1,2
            DL(0,0,IV,1-LS)=1
            DL(0,0,IV,LT-1)=1            
          END DO
        END IF
        IF(IP.EQ.0) THEN      
          DO IV=1,2
            DO I=-1,1,2
              DL(0,I,IV,0)=1
              DL(I,0,IV,0)=1
            END DO
          END DO
        END IF
      END IF

* P3PACKの初期化

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

* FHPACKの初期化

      IF(IP.EQ.0) THEN
        CALL FHUOPN(IU,'test.dat','W',NB)
      END IF

      RETURN
*----------------------------------------------------------------------
*     従属変数(渦度ベクトルの2成分に対応)の初期化
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

      CALL BSSET0(ND,Z)

      IF(LE.GE.1.AND.LS.LE.1) THEN
        Z( 0, 0, 1, 1-LS)= 1D0/4
        Z( 0, 0, 1, LT-1)= 1/(4*SQRT(3D0))
        Z( 0, 0, 2, 1-LS)= -1/(2*SQRT(3D0))
        Z( 0, 0, 2, LT-1)= 0
      END IF

      IF(IP.EQ.0) THEN
        Z( 0, 1, 1, 0)= 1D0/4
        Z( 0,-1, 1, 0)= 1/(4*SQRT(3D0))
        Z( 0, 1, 2, 0)= -1/(2*SQRT(3D0))
        Z( 0,-1, 2, 0)= 0
      
        Z( 1, 0, 1, 0)= 1D0/4
        Z(-1, 0, 1, 0)= 1/(4*SQRT(3D0))
        Z( 1, 0, 2, 0)= -1/(2*SQRT(3D0))
        Z(-1, 0, 2, 0)= 0
      END IF

      RETURN
*----------------------------------------------------------------------
*     従属変数(渦度ベクトルの2成分に対応)の時間変化率の計算
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P3EMNL(NM,MM,LM,KM,JM,IM,Z,DZ,W,ITK,TK,ITJ,TJ,ITI,TI)

      IF(IFLAG.EQ.1) THEN
        IF(LE.GE.1.AND.LS.LE.1) THEN
          DO IV=1,2
            DZ(0,0,IV,1-LS)=0
            DZ(0,0,IV,LT-1)=0            
          END DO
        END IF
        IF(IP.EQ.0) THEN      
          DO IV=1,2
            DO I=-1,1,2
              DZ(0,I,IV,0)=0
              DZ(I,0,IV,0)=0
            END DO
          END DO
        END IF
      END IF

      RETURN
*----------------------------------------------------------------------
*     時刻(T)および時間刻み(DT)を入力として, 線形(粘性)項によるZの
*     T→T+DTにおける発展を解くサブルーチン(Runge-Kuttaで使われる)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO L=0,L2
        DO M=-MM,MM
          DO N=-NM,NM
            Z(N,M,1,L)=DL(N,M,1,L)*Z(N,M,1,L)
            Z(N,M,2,L)=DL(N,M,2,L)*Z(N,M,2,L)
          END DO
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     データの通常配置モードへの集約およびファイルへの出力
*     (プロセス0に集約し, プロセス0でファイルに出力)
*----------------------------------------------------------------------
      ENTRY SBPUTD(Z)

      DO L=-LM,-LM/NP-1
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,1,LT+L),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,LM+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,LM+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      IF(IP.EQ.0) THEN
        DO L=-LM/NP,-1
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,1,LT+L))
        END DO
        DO L=0,LM/NP
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,1,L))
        END DO
      END IF

      DO L=LM/NP+1,LM
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,1,L-LS),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,LM+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,LM+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      DO L=-LM,-LM/NP-1
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,2,LT+L),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,3*LM+1+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,3*LM+1+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      IF(IP.EQ.0) THEN
        DO L=-LM/NP,-1
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,2,LT+L))
        END DO
        DO L=0,LM/NP
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,2,L))
        END DO
      END IF

      DO L=LM/NP+1,LM
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,2,L-LS),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,3*LM+1+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,3*LM+1+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      RETURN
*----------------------------------------------------------------------
*     保存量のチェック(エネルギー(E)およびヘリシティ(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CMSV(NM,MM,LM,Z,E,H)
      IF(IP.EQ.0) THEN
        WRITE(6,'(A,F5.2,2(A,F17.15))')
     &    'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H
      END IF

      RETURN
*----------------------------------------------------------------------
*     パッケージの終了処理をするサブルーチン
*----------------------------------------------------------------------
      ENTRY SBCLOS

      IF(IP.EQ.0) THEN
        CALL FHUCLS(IU)
      END IF

      END
