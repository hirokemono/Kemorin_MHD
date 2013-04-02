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
*     TRANSFORM SPECTRA TO GRID                               2009/08/11
************************************************************************
      SUBROUTINE SJTS2G(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((2*NN+1-MM)*MM+NN+1),G(0:IM-1,JM)
      DIMENSION IT(2,2),T(IM*3,2)      
      DIMENSION P(JM/2,MM+4),Q(JM/2,7),R(*)
      DIMENSION WS(2,0:NN),W(0:(JM+1)*(MM*2+1)-1),WG(0:IM-1)

      CALL LJTSZG(NM,NN,JM,S,W(0),P,Q,R,WS,IPOW)
      DO M=1,MM
        NS=1+NN+1+(M-1)*(NN+NN+2-M)
        CALL LJTSWG(NM,NN,JM,M,S(NS),W((JM+1)*(2*M-1)),P,Q,R,WS,IPOW)
      END DO

      DO J=1,JM
        G(0,J)=W(J-1)
        G(1,J)=0
        DO M=1,MM
          G(2*M,J)=W((JM+1)*(2*M-1)+2*(J-1))
          G(2*M+1,J)=W((JM+1)*(2*M-1)+2*(J-1)+1)
        END DO
        DO I=2*MM+2,IM-1
          G(I,J)=0
        END DO
      END DO

      DO J=1,JM
        CALL FJRRUN(G(0,J),WG,W,T(1,1),IT(1,1))
      END DO

      END
