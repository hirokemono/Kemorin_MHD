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
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'            
      PARAMETER(NM=10,MM=10,LM=10) ! main programと同じ値を与えること.
      PARAMETER(KM=32,JM=32,IM=32) ! 格子点のサイズ
      PARAMETER(NPROC=1)
      PARAMETER(ISIZE1=IM*KM*((JM-1)/NPROC+1))
      DIMENSION Z(-NM:NM,-MM:MM,2,0:LM/NPROC*2+1)  ! 渦度ベクトルの2成分
      DIMENSION W(ISIZE1)              ! 作業領域
      DIMENSION U(ISIZE1,3),O(ISIZE1,3)
      ! 速度ベクトルと渦度ベクトルが格納される作業領域
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      ! P3PACKで使われる配列

      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

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

      JP=(JM-1)/NP+1
      JS=JP*IP
      JE=MIN(JP*(IP+1)-1,JM-1)
      IF(JE.GE.JS) THEN
        JC=JE-JS+1
      ELSE
        JC=0
        JS=0
        JE=0
      END IF

      IF(LS.EQ.0.AND.LC.NE.0) THEN
        ND=(2*LC-1)*(2*MM+1)*(2*NM+1)*2
      ELSE IF(LC.NE.0) THEN
        ND=(2*LC)*(2*MM+1)*(2*NM+1)*2
      ELSE
        ND=0
      END IF

* P3PACKの初期化

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

* Zの値の設定      

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

* 保存量のチェック1 (P3CMSVを用いてZから直接計算)

      CALL P3CMSV(NM,MM,LM,Z,E,H)
      IF(IP.EQ.0) THEN
        WRITE(6,'(A,2(A,F17.15))')
     &    'USING P3CMSV:     ', '  ENERGY = ',E,'  HELICITY = ',H
      END IF

* 保存量のチェック2 (格子点上の速度と渦度を求めて計算)

      DO ISW=1,3
        CALL P3GMTU(NM,MM,LM,Z,U(1,ISW),ISW)
        CALL P3SMGB(NM,MM,LM,KM,JM,IM,U(1,ISW),
     &    W,ITK,TK,ITJ,TJ,ITI,TI)
        CALL P3GMTO(NM,MM,LM,Z,O(1,ISW),ISW)
        CALL P3SMGB(NM,MM,LM,KM,JM,IM,O(1,ISW),
     &    W,ITK,TK,ITJ,TJ,ITI,TI)
      END DO

      IJKM=IM*JM*KM
      ED=0
      HD=0
      DO IJK=1,IM*JC*KM
        ED=ED+U(IJK,1)**2+U(IJK,2)**2+U(IJK,3)**2
        HD=HD+U(IJK,1)*O(IJK,1)+U(IJK,2)*O(IJK,2)+U(IJK,3)*O(IJK,3)
      END DO
      ED=ED/(2*IJKM)
      HD=HD/IJKM

      EDD=ED
      HDD=HD
      
      CALL MPI_ALLREDUCE(HDD,HD,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(EDD,ED,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      IF(IP.EQ.0) THEN
        WRITE(6,'(A,2(A,F17.15))')
     &    'USING GRID-VALUES:','  ENERGY = ',ED,'  HELICITY = ',HD
      END IF

      CALL MPI_FINALIZE(IERR)      

      END
