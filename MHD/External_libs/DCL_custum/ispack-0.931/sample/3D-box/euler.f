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
*     euler.f: P3PACKを使って3次元非発散Euler方程式を解くプログラム
*                                                  2002/03/16 K.Ishioka
*                      (P3ELNLに必要な作業領域が減ったのに対応して変更)
*                                                  2002/05/06 K.Ishioka      
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* パラメターの設定 

      PARAMETER(NM=21,MM=21,LM=21) ! 切断波数
      PARAMETER(N=(2*LM+1)*(2*MM+1)*(2*NM+1)*2)
      DIMENSION Z(N)   ! 渦度ベクトルを格納する従属変数
      DIMENSION W(N,3) ! 作業領域
      EXTERNAL SBGDZN

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
        CALL TDRK4U(N,M,DT,T,Z,W,SBGDZN)
        CALL SBGDCK(T,Z)
      END DO

      END
***********************************************************************
*     時間積分等に必要な種々の処理をまとめたサブルーチンパッケージ
*----------------------------------------------------------------------
*     パッケージの初期化をするサブルーチン
*----------------------------------------------------------------------
      SUBROUTINE SBINIT

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NM=21,MM=21,LM=21) ! main programと同じ値を与えること.
      PARAMETER(KM=64,JM=64,IM=64) ! 格子点のサイズ
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)  ! 渦度ベクトルの2成分
      DIMENSION DZ(-NM:NM,-MM:MM,-LM:LM,2) ! Zの時間変化率
      DIMENSION WS(-NM:NM,-MM:MM,-LM:LM) ! 作業領域
      DIMENSION W(KM*JM*IM*4)              ! 作業領域
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                           ! P3PACKで使われる配列
      SAVE

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
*     従属変数(渦度ベクトルの2成分に対応)の時間変化率の計算
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P3ELNL(NM,MM,LM,KM,JM,IM,Z,DZ,WS,W,ITK,TK,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     保存量のチェック(エネルギー(E)およびヘリシティ(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CNSV(NM,MM,LM,Z,E,H)
      WRITE(6,'(A,F5.2,2(A,F17.15))')
     &  'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H

      END
