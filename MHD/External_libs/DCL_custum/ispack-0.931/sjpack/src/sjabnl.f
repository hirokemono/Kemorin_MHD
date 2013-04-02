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
*     2次元バロトロピック方程式の非線形項の計算               2010/02/02
************************************************************************
      SUBROUTINE SJABNL(MM,NM,IM,JM,S,SOUT,IT,T,P,Q,R,C,E,SW1,SW2,
     &  G1,G2,WS1,WS2,WG,W1,W2)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)),SOUT((MM+1)*(MM+1))
      DIMENSION SW1((MM+6)*MM+3),SW2((MM+6)*MM+3)
      DIMENSION G1(IM*JM),G2(IM*JM)
      DIMENSION IT(2,2),T(IM*3,2)      
      DIMENSION P(JM/2,MM+4),Q(JM/2,11),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION C((MM+1)*(MM+1))
      DIMENSION E((5*MM*(MM+1)+4)/2)      
      DIMENSION WS1(2*(MM+3)),WS2(2*(MM+3))
      DIMENSION WG((IM+2)*JM)
      DIMENSION W1((JM+1)*IM),W2((JM+1)*IM)

      CALL SJCS2Y(MM,S,SW2,C)      

      M=0
      DO N=0,MM+1
        SW1(N+1)=0
      END DO
      DO M=1,MM
        NS=MM+1+(MM+(MM-(M-2)))*(M-1)+1
        NSD=MM+2+(MM+1+(MM+1-(M-2)))*(M-1)+1        
        DO N=M,MM
          L=NS+2*(N-M)
          LD=NSD+2*(N-M)          
          SW1(LD)=-M*S(L+1)
          SW1(LD+1)=M*S(L)
        END DO
        LD=NSD+2*(MM+1-M)
        SW1(LD)=0
        SW1(LD+1)=0
      END DO
      
      CALL SJMS2G(MM,NM,MM+1,IM,JM,SW1,SW2,G1,G2,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,2)

      ! G1: v/\sqrt(1-\mu^2), G2: -u/\sqrt(1-\mu^2)

      DO IJ=1,IM*JM
        GD=G1(IJ)
        G1(IJ)=-GD*G2(IJ) ! uv/(1-\mu^2)
        G2(IJ)=GD*GD-G2(IJ)*G2(IJ) ! (v^2-u^2)/(1-\mu^2)
      END DO

      CALL SJMG2S(MM,NM,MM+2,IM,JM,SW1,SW2,G1,G2,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,0)
      
      IND=1
      M=0
      N=0
      SOUT(N+1)=E(IND)*SW1(N+3)+E(IND+1)*SW1(N+1)
      IND=IND+2
      N=1
      SOUT(N+1)=E(IND)*SW1(N+3)+E(IND+1)*SW1(N+1)
      IND=IND+2      
      DO N=2,MM
        SOUT(N+1)
     &    =E(IND)*SW1(N+3)+E(IND+1)*SW1(N+1)+E(IND+2)*SW1(N-1)
        IND=IND+3
      END DO
      DO M=1,MM-1
        NS=MM+1+(MM+(MM-(M-2)))*(M-1)+1
        NSD=MM+3+(MM+2+(MM+2-(M-2)))*(M-1)+1
        N=M
        L=NS+2*(N-M)
        LD=NSD+2*(N-M)
        SOUT(L)=E(IND)*SW1(LD+4)+E(IND+1)*SW1(LD)+E(IND+2)*SW2(LD+3)
        SOUT(L+1)=E(IND)*SW1(LD+5)+E(IND+1)*SW1(LD+1)-E(IND+2)*SW2(LD+2)
        IND=IND+3
        N=M+1
        L=NS+2*(N-M)
        LD=NSD+2*(N-M)
        SOUT(L)=E(IND)*SW1(LD+4)+E(IND+1)*SW1(LD)+E(IND+2)*SW2(LD+3)
     &    +E(IND+3)*SW2(LD-1)
        SOUT(L+1)=E(IND)*SW1(LD+5)+E(IND+1)*SW1(LD+1)-E(IND+2)*SW2(LD+2)
     &    -E(IND+3)*SW2(LD-2)
        IND=IND+4
        DO N=M+2,MM
          L=NS+2*(N-M)
          LD=NSD+2*(N-M)
          SOUT(L)=E(IND)*SW1(LD+4)+E(IND+1)*SW1(LD)
     &      +E(IND+2)*SW1(LD-4)
     &      +E(IND+3)*SW2(LD+3)+E(IND+4)*SW2(LD-1)
          SOUT(L+1)=E(IND)*SW1(LD+5)+E(IND+1)*SW1(LD+1)
     &      +E(IND+2)*SW1(LD-3)
     &      -E(IND+3)*SW2(LD+2)-E(IND+4)*SW2(LD-2)
          IND=IND+5
        END DO
      END DO
      M=MM
      NS=MM+1+(MM+(MM-(M-2)))*(M-1)+1
      NSD=MM+3+(MM+2+(MM+2-(M-2)))*(M-1)+1
      N=M
      L=NS+2*(N-M)
      LD=NSD+2*(N-M)
      SOUT(L)=E(IND)*SW1(LD+4)+E(IND+1)*SW1(LD)+E(IND+2)*SW2(LD+3)
      SOUT(L+1)=E(IND)*SW1(LD+5)+E(IND+1)*SW1(LD+1)-E(IND+2)*SW2(LD+2)
      IND=IND+3

      END
