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
*     2次元非線形項の計算(省エネ版下位ルーチン)
*     (MPIによる安直並列化)                                   2002/05/20      
************************************************************************
      SUBROUTINE SOMDNL(MM,IM,ID,JM,JD,
     &  S,SOUT,IT,T,Y,IP2,P2,R2,IA,A,IB,B,Q,WS,WW,W)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'
      DIMENSION S((MM+1)*(MM+1)),SOUT((MM+1)*(MM+1))
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP2(2*((MM+2)/2+MM+2)*2)
      DIMENSION P2(2*((MM+2)/2+MM+2)*JM)
      DIMENSION R2(2*((MM+2)/2*2+3)*((MM+1)/2+1))
      DIMENSION IA((MM+2)*(MM+2)*4)
      DIMENSION A((MM+2)*(MM+2)*6)
      DIMENSION Q(2*((MM+2)/2+MM+2)*JM)
      DIMENSION WS(*)
      DIMENSION WW((MM+2)*(MM+2),2)
      DIMENSION IB((MM+1)*(MM+1),3)
      DIMENSION B((MM+1)*(MM+1),3)
      DIMENSION W((MM+1)*(MM+1))

      LM=(MM+1)*(MM+1)      

      IF(JM.EQ.0) THEN
        CALL BSSET0(LM,W)
      ELSE
        CALL SONDNL(MM,IM,ID,JM,JD,
     &  S,W,IT,T,Y,IP2,P2,R2,IA,A,IB,B,Q,WS,WW)
      END IF

      CALL MPI_ALLREDUCE(W,SOUT,LM,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)        

      END
