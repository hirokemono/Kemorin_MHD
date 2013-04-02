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
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=682,JM=1024,IM=2048)
*      PARAMETER(MM=341,JM=512,IM=1024)
*      PARAMETER(MM=170,JM=256,IM=512)            
      PARAMETER(NM=MM,NN=MM,IPOW=0)
      DIMENSION S((2*NN+1-MM)*MM+NN+1),G(IM*JM)
      DIMENSION SD((2*NN+1-MM)*MM+NN+1),GD(IM*JM)
      DIMENSION IT(4),T(IM*6)      
      DIMENSION P(JM/2*(MM+4)),Q(JM/2*7),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION WS((2*NN+1-MM)*MM+NN+1),WG((IM+2)*JM),W((JM+1)*IM)      
      INTEGER IP(8)

      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)
      CALL SJVOPN(MM,NM,JM,IM,P,R,IP)

      DO L=1,(2*NN+1-MM)*MM+NN+1
        CALL SJL2NM(MM,L,N,M)
        S(L)=1D0/((ABS(M)+1D0)*(N+1D0))
      END DO

      CALL SJTS2G(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)
      CALL BSSET0((JM+1)*IM,W)
      CALL SJVS2G(MM,NM,NN,IM,JM,S,GD,IT,T,P,R,WS,WG,W,IPOW,IP)

      ERMAX=0
      DO IJ=1,IM*JM
        ER=ABS(G(IJ)-GD(IJ))/(ABS(G(IJ))+ABS(GD(IJ))+1)
        IF(ER.GT.ERMAX) ERMAX=ER
      END DO

      print *,'SJVS2G check:'
      IF(ERMAX.LT.1D-12) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF


      CALL SJTG2S(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)
      CALL BSSET0((JM+1)*IM,W)
      CALL SJVG2S(MM,NM,NN,IM,JM,SD,G,IT,T,P,R,WS,WG,W,IPOW,IP)

      ERMAX=0
      DO L=1,(2*NN+1-MM)*MM+NN+1
        ER=ABS(S(L)-SD(L))/(ABS(S(L))+ABS(SD(L))+1)
        IF(ER.GT.ERMAX) ERMAX=ER
      END DO

      print *,'SJVG2S check:'
      IF(ERMAX.LT.1D-14) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF

      CALL SJVCLS(IP)

      END
