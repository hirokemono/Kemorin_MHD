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
*     TRANSFORM GRID TO SPECTRA(for CUDA)                     2010/09/13
************************************************************************
      SUBROUTINE SJVG2S(MM,NM,NN,IM,JM,S,G,IT,T,P,R,WS,WG,W,IPOW,IP)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((2*NN+1-MM)*MM+NN+1),G(0:IM-1,JM)
      DIMENSION IT(2,2),T(IM*3,2)      
      DIMENSION P(JM/2,MM+4),Q(JM/2,7),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION WS((2*NN+1-MM)*MM+NN+1),W(0:IM-1,JM)
      DIMENSION WG(0:IM-1,JM)
      INTEGER IP(8)

!$omp parallel do
      DO J=1,JM
        CALL FJRRUN(G(0,J),WG(0,J),W(0,J),T(1,2),IT(1,2))
      END DO
!$omp end parallel do

      CALL SJWG2S(MM,NM,NN,IM,JM,P,R,WS,WG,IPOW,IP)

      F=1D0/IM
      S(1)=WS(1)*F
      DO N=1,NN
        S(N+1)=R(N)*WS(N+1)*F
      END DO
      DO M=1,MM
        NS=NN+2+(M-1)*(2*NN+2-M)
        NSR=(2*NM-M)*M
        S(NS  )=WS(NS)*F
        S(NS+1)=WS(NS+1)*F
        DO N=M+1,NN
          S(NS+(N-M)*2  )=R(N-M+NSR)*WS(NS+(N-M)*2  )*F
          S(NS+(N-M)*2+1)=R(N-M+NSR)*WS(NS+(N-M)*2+1)*F
        END DO
      END DO

      END
