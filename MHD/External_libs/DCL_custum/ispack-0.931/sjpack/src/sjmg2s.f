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
*     TRANSFORM GRID TO SPECTRA                               2009/08/11
*     (2成分同時変換)
************************************************************************
      SUBROUTINE SJMG2S(MM,NM,NN,IM,JM,S1,S2,G1,G2,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S1((2*NN+1-MM)*MM+NN+1),S2((2*NN+1-MM)*MM+NN+1)
      DIMENSION G1(0:IM-1,JM),G2(0:IM-1,JM)
      DIMENSION IT(2,2),T(IM*3,2)      
      DIMENSION P(JM/2,MM+4),Q(JM/2,11),R(*)
      DIMENSION WS1(2,0:NN),WS2(2,0:NN)
      DIMENSION WG(0:IM+1,JM)      
      DIMENSION W1(0:(JM+1)*(MM*2+1)-1),W2(0:(JM+1)*(MM*2+1)-1)

      F=1D0/IM      

      DO J=1,JM
        CALL FJRRUN(G1(0,J),WG(0,J),W1,T(1,2),IT(1,2))
      END DO

      DO J=1,JM
        W1(J-1)=WG(0,J)*F
      END DO
      DO M=1,MM
        DO J=1,JM        
          W1((JM+1)*(2*M-1)+2*(J-1))=WG(2*M,J)*F
          W1((JM+1)*(2*M-1)+2*(J-1)+1)=WG(2*M+1,J)*F
        END DO
      END DO

      DO J=1,JM
        CALL FJRRUN(G2(0,J),WG(0,J),W2,T(1,2),IT(1,2))        
      END DO

      DO J=1,JM
        W2(J-1)=WG(0,J)*F        
      END DO
      DO M=1,MM
        DO J=1,JM        
          W2((JM+1)*(2*M-1)+2*(J-1))=WG(2*M,J)*F
          W2((JM+1)*(2*M-1)+2*(J-1)+1)=WG(2*M+1,J)*F
        END DO
      END DO

      CALL LJMGZS(NM,NN,JM,S1,S2,W1(0),W2(0),P,Q,R,WS1,WS2,IPOW)
      DO M=1,MM
        NS=1+NN+1+(M-1)*(NN+NN+2-M)        
        CALL LJMGWS(NM,NN,JM,M,S1(NS),S2(NS),
     &    W1((JM+1)*(2*M-1)),W2((JM+1)*(2*M-1)),P,Q,R,WS1,WS2,IPOW)
      END DO

      END
