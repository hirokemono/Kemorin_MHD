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
*     2次元ヤコビアンの計算                                   2010/01/29
************************************************************************
      SUBROUTINE SJAJCB(MM,NM,IM,JM,SA,SB,SC,IT,T,P,Q,R,C,SW1,SW2,
     &  G1,G2,G3,WS1,WS2,WG,W1,W2)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA((MM+1)*(MM+1)),SB((MM+1)*(MM+1))
      DIMENSION SC((MM+1)*(MM+1))      
      DIMENSION SW1((MM+4)*MM+2),SW2((MM+4)*MM+2)
      DIMENSION G1(IM*JM),G2(IM*JM),G3(IM*JM)
      DIMENSION IT(2,2),T(IM*3,2)      
      DIMENSION P(JM/2,MM+4),Q(JM/2,11),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION C((MM+1)*(MM+1))
      DIMENSION WS1(2*(MM+2)),WS2(2*(MM+2))
      DIMENSION WG((IM+2)*JM)
      DIMENSION W1((JM+1)*IM),W2((JM+1)*IM)

      CALL SJCS2Y(MM,SA,SW2,C)      
      CALL SJTS2G(MM,NM,MM+1,IM,JM,SW2,G1,IT,T,P,Q,R,WS1,WG,W1,1)

      CALL SJCS2X(MM,SA,SW1)
      CALL SJMS2G(MM,NM,MM,IM,JM,SW1,SB,G2,G3,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,1)

      DO IJ=1,IM*JM
        G1(IJ)=G1(IJ)*G3(IJ)
        G2(IJ)=G2(IJ)*G3(IJ)
      END DO

      CALL SJMG2S(MM,NM,MM+1,IM,JM,SW1,SW2,G1,G2,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,0)

      CALL SJCY2S(MM,SW2,SC,C)
      CALL SJCRDN(MM,MM+1,SW1,SW2)
      CALL SJCS2X(MM,SW2,SW1)

      DO L=1,(MM+1)*(MM+1)
        SC(L)=SC(L)-SW1(L)
      END DO
      
      END
