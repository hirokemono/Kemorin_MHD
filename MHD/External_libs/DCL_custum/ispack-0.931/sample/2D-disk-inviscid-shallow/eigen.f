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
*     基本場の線形安定性解析を行うプログラム 2003/04/10 By K. Ishioka
************************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(F=1D0)       ! コリオリパラメターの値
      PARAMETER(MM=10,JM=9) ! 切断次数 MM および 分点数 JM の設定
*-----------------------------------------------------------------------
*     使用する配列の宣言など(特に変更する必要はない)
*-----------------------------------------------------------------------
      PARAMETER(MMP=(MM+8)*MM/4+1)
      DIMENSION P(JM,2*MMP)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)      
      DIMENSION B((MM/2+1)**2*9)
      DIMENSION WORK(JM*9)
      DIMENSION H(JM,2),U(JM,2)
      DIMENSION WR((MM/2+1)*3),WI((MM/2+1)*3)
      DIMENSION WORKL((MM/2+1)*9)
*-----------------------------------------------------------------------            

      CALL DKAIN0(MM,JM,P,A) ! DKPACKの初期化(配列 P,A のみ)

*-----------------------------------------------------------------------
*     基本場の U,H の設定.
*     Yは r座標の値, U(J,1)は u, U(J,2)は ∂(ru)/∂s
*                    H(J,1)は h, U(J,2)は ∂(h)/∂s を与えること.
*      ここに, s=r^2
*-----------------------------------------------------------------------
      DO J=1,JM
        Y=P(J,1)
        X=Y*Y
        U(J,1)=Y*(1-X)**2
        U(J,2)=(1-X)**2-2*X*(1-X)
        H(J,1)=(-(1-X)**5/5-F*(1-X)**3/3)/2+1
        H(J,2)=((1-X)**4+F*(1-X)**2)/2        
      END DO
*-----------------------------------------------------------------------      

      WRITE(6,*) 'M=?'  ! 安定性解析をするθ方向の波数 m を与える
      READ(5,*) M

*-----------------------------------------------------------------------
*     以下, LAPACK の DGEEVを用いて固有値を計算し,
*      固有値の実部(振動数)および虚部(発達率)を出力する.
*-----------------------------------------------------------------------
      CALL DKALNR(MM,JM,M,F,H,U,B,WORK,P,A)

      IF(M.EQ.0) THEN
        N=MM/2*3+1
      ELSE
        N=((MM-M)/2+1)*3
      END IF
      
      LWORK=3*N
      CALL DGEEV('N','N',N,B,N,WR,WI,VL,1,VR,1,WORKL,LWORK,INFO)
      print *,'INFO=',INFO
      DO L=1,N
        print *,L,WR(L),WI(L)
      END DO

      END
