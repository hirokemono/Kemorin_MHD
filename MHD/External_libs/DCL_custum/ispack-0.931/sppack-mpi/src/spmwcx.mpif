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
*     浅水方程式の保存量のチェック(作業領域削減版)
*     (角運動量を3成分計算する. ただし回転系では地軸まわり以外の
*      角運動量は保存量にならないことに注意)
*     (MPIによる安直並列化)                                   2002/05/20      
************************************************************************
      SUBROUTINE SPMWCX(MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AM1,AM2,AM3,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      IF(JM.EQ.0) THEN
        AM1D=0
        AM2D=0
        AM3D=0                
        AENED=0
        AENSD=0
      ELSE
        CALL SPSWCX(MM,IM,ID,JM,JD,OMEGA,AVT,DIV,PHI,
     &    AM1D,AM2D,AM3D,AENED,AENSD,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)
      END IF
      
      CALL MPI_ALLREDUCE(AM1D,AM1,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(AM2D,AM2,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(AM3D,AM3,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(AENED,AENE,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(AENSD,AENS,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      END
