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
*     testomp-align.f のためのサブルーチン                    2009/08/21
************************************************************************      
      SUBROUTINE SUB(T,Q,G1,G2,G1O,G2O,WG,W1,W2)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=170,JM=256,IM=512,NT=16)
      PARAMETER(NM=MM,NN=MM,IPOW=0)
      DIMENSION S1((MM+1)*(MM+1)),S2((MM+1)*(MM+1))      
      DIMENSION S1O((MM+1)*(MM+1)),S2O((MM+1)*(MM+1))
      DIMENSION G1(IM*JM),G2(IM*JM)
      DIMENSION G1O(IM*JM),G2O(IM*JM)      
      DIMENSION IT(4),T(IM*6)      
      DIMENSION P(JM/2*(MM+4)),Q(JM/2*11*NT),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION WG((IM+2)*JM)
      DIMENSION WS1(2*(NM+1)*NT),W1((JM+1)*IM)
      DIMENSION WS2(2*(NM+1)*NT),W2((JM+1)*IM)      

      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)

      DO L=1,(MM+1)*(MM+1)
        CALL SJL2NM(MM,L,N,M)
        S1(L)=1D0/((ABS(M)+1D0)*(N+1D0))
        S2(L)=-1D0/((ABS(M)+1D0)*(N+1D0))        
      END DO

* 逆変換のテスト

      CALL SJTS2G(MM,NM,NN,IM,JM,S1,G1,IT,T,P,Q,R,WS1,WG,W1,IPOW)
      CALL SJTSOG(MM,NM,NN,IM,JM,S1,G1O,IT,T,P,Q,R,WS1,WG,W1,IPOW)

      INDEX=0
      DO IJ=1,IM*JM
        IF(G1(IJ).NE.G1O(IJ)) THEN
          INDEX=INDEX+1
        END IF
      END DO

      print *,'SJTS2G and SJTSOG check:'
      IF(INDEX.EQ.0) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF

      CALL SJMS2G(MM,NM,NN,IM,JM,S1,S2,G1,G2,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,IPOW)
      CALL SJMSOG(MM,NM,NN,IM,JM,S1,S2,G1O,G2O,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,IPOW)

      INDEX=0
      DO IJ=1,IM*JM
        IF(G1(IJ).NE.G1O(IJ).OR.G2(IJ).NE.G2O(IJ)) THEN
          INDEX=INDEX+1
        END IF
      END DO

      print *,'SJTS2G and SJTSOG check:'
      IF(INDEX.EQ.0) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF

* 正変換のテスト

      CALL SJTG2S(MM,NM,NN,IM,JM,S1,G1,IT,T,P,Q,R,WS1,WG,W1,IPOW)
      CALL SJTGOS(MM,NM,NN,IM,JM,S1O,G1,IT,T,P,Q,R,WS1,WG,W1,IPOW)

      INDEX=0
      DO L=1,(MM+1)*(MM+1)
        IF(S1(L).NE.S1O(L)) THEN
          INDEX=INDEX+1
        END IF
      END DO

      print *,'SJTG2S and SJTGOS check:'
      IF(INDEX.EQ.0) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF

      CALL SJMG2S(MM,NM,NN,IM,JM,S1,S2,G1,G2,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,IPOW)
      CALL SJMGOS(MM,NM,NN,IM,JM,S1O,S2O,G1,G2,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,IPOW)

      INDEX=0
      DO L=1,(MM+1)*(MM+1)
        IF(S1(L).NE.S1O(L).OR.S2(L).NE.S2O(L)) THEN
          INDEX=INDEX+1
        END IF
      END DO

      print *,'SJMG2S and SJMGOS check:'
      IF(INDEX.EQ.0) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF
        
      END
