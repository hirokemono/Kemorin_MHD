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
*     浅水方程式の保存量のチェック 
*     (MPIによる安直並列化)                                   2002/05/20      
************************************************************************
      SUBROUTINE SPMWCK(MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AMOM,AENE,AENS,
     &  RN,IT,T,Y,IP4,P4,R4,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP4(4*((MM+1)/2+MM+1)*2)
      DIMENSION P4(4*((MM+1)/2+MM+1)*JM)
      DIMENSION R4(4*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(4*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(ID,JD,4)

      IF(JM.EQ.0) THEN
        AMOMD=0
        AENED=0
        AENSD=0
      ELSE
        CALL SPSWCK(MM,IM,ID,JM,JD,OMEGA,
     &    AVT,DIV,PHI,AMOMD,AENED,AENSD,
     &    RN,IT,T,Y,IP4,P4,R4,IA,A,Q,WS,WW)
      END IF
      
      CALL MPI_ALLREDUCE(AMOMD,AMOM,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(AENED,AENE,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(AENSD,AENS,1,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      END
