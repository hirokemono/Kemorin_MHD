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
*     INITIALIZATION OF SOPACK                                  99/02/19
************************************************************************
      SUBROUTINE SOINIT(MM,IB,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IB((MM+1)*(MM+1),3)
      DIMENSION B((MM+1)*(MM+1),3)

      EPSL(N,M)=SQRT((1D0*N*N-M*M)/(4D0*N*N-1))

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        CALL SNL2NM(L,N,M)
        MA=ABS(M)
        B(L,1)=M
        CALL SNNM2L(N,-M,IB(L,1))
        CALL SNNM2L(N+1,M,IB(L,3))
        B(L,3)=-N*EPSL(N+1,M)
        IF(N.EQ.MA) THEN
          IB(L,2)=L
          B(L,2)=0
        ELSE
          CALL SNNM2L(N-1,M,IB(L,2))
          B(L,2)=(N+1)*EPSL(N,M)
        END IF
      END DO

      END
************************************************************************
*     2次元非発散方程式の非線形項の計算(省エネ版)
************************************************************************
      SUBROUTINE SONDNL(MM,IM,ID,JM,JD,
     &  S,SOUT,IT,T,Y,IP2,P2,R2,IA,A,IB,B,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
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

      LM=(MM+1)*(MM+1)

      CALL SOTNLT(MM+1,IM,ID,JM,JD,S,IT,T,Y,IP2,P2,R2,IA,A,Q,WS,WW)

      DO L=1,LM
        SOUT(L)=B(L,1)*WW(IB(L,1),1)
     &    +B(L,2)*WW(IB(L,2),2)+B(L,3)*WW(IB(L,3),2)
      END DO

      END
