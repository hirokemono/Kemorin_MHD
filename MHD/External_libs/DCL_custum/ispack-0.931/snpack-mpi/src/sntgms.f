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
*     TRANSFORM SPECTRA TO GRID
*     (MPIによる安直並列化)                                   2002/05/20
************************************************************************
      SUBROUTINE SNTGMS(MM,IM,ID,JM,JD,KM,
     &  G,S,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,IPOW,IFLAG,W)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION G(ID*JD*KM)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1))
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*),WW(*)
      DIMENSION W((MM+1)*(MM+1)*KM)

      LM=(MM+1)*(MM+1)

      IF(JM.EQ.0) THEN
        CALL BSSET0((MM+1)*(MM+1)*KM,W)
      ELSE
        CALL SNTG2S(MM,IM,ID,JM,JD,KM,
     &  G,W,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,IPOW,IFLAG)
      END IF
      
      CALL MPI_ALLREDUCE(W,S,LM*KM,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      END
