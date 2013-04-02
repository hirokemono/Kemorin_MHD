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
*     euler-mpi.f:
*              P3PACK-MPIを使って3次元非発散Euler方程式を解くプログラム
*                                                  2002/05/07 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      

* パラメターの設定 

      PARAMETER(NM=21,MM=21,LM=21) ! 切断波数
      PARAMETER(NPROC=1) ! 並列実行時に用いるプロセス数のとりうる最小値
                         ! もし多数のプロセス数でしか計算しないなら
                         ! これをそのプロセス数に合わせることによって
                         ! 必要なメモリ量を削減できる.      
      PARAMETER(N=(2*MM+1)*(2*NM+1)*2*2*(LM/NPROC+1))
      DIMENSION Z(N)   ! 渦度ベクトルを格納する従属変数
      DIMENSION W(N*3) ! 作業領域
      EXTERNAL SBGDZN

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

* 時間積分のためのパラメター設定

      NSTEP=10                 !時間発展するステップ数
      DT=0.01D0                !保存量のチェックのための時間間隔
      M=1                      !Runge-Kuttaでのステップ分割数

* パッケージと従属変数の初期化

      CALL SBINIT
      CALL SBINIZ(Z)

* 時間発展および保存量のチェック

      T=0                       !Tは時刻を表す変数
      CALL SBGDCK(T,Z) 
      DO ISTEP=1,NSTEP
        CALL TDRK4U(ND,M,DT,T,Z,W,SBGDZN)
        CALL SBGDCK(T,Z)
      END DO

* MPIの終了

      CALL MPI_FINALIZE(IERR)      

      END
***********************************************************************
*     時間積分等に必要な種々の処理をまとめたサブルーチンパッケージ
*----------------------------------------------------------------------
*     パッケージの初期化をするサブルーチン
*----------------------------------------------------------------------
      SUBROUTINE SBINIT

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'            
      PARAMETER(NM=21,MM=21,LM=21) ! main programと同じ値を与えること.
      PARAMETER(KM=64,JM=64,IM=64) ! 格子点のサイズ
      PARAMETER(NPROC=1) ! 並列実行時に用いるプロセス数のとりうる最小値
                         ! もし多数のプロセス数でしか計算しないなら
                         ! これをそのプロセス数に合わせることによって
                         ! 必要なメモリ量を削減できる.      
      PARAMETER(N=(2*MM+1)*(2*NM+1)*2*2*(LM/NPROC+1))
      DIMENSION Z(-NM:NM,-MM:MM,2,0:*)  ! 渦度ベクトルの2成分
      DIMENSION DZ(-NM:NM,-MM:MM,2,0:*) ! Zの時間変化率
      DIMENSION W(KM*IM*4*((JM-1)/NPROC+1))   ! 作業領域
      ! JM=IM の場合はこれでよいが, JM≠IM の場合は異なる大きさに
      ! しなければならない場合もあるので注意すること.
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                           ! P3PACKで使われる配列
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
      
* P3PACKの初期化

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

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

      RETURN
*----------------------------------------------------------------------
*     保存量のチェック(エネルギー(E)およびヘリシティ(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CMSV(NM,MM,LM,Z,E,H)
      IF(IP.EQ.0) THEN ! プロセス0でのみ出力
        WRITE(6,'(A,F5.2,2(A,F17.15))')
     &    'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H
      END IF

      END
