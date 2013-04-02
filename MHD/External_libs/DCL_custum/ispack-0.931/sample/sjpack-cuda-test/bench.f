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
      PARAMETER(NTRY=8,NTR=10)
      PARAMETER(MM=682,JM=1024,IM=2048,NT=16)
*      PARAMETER(MM=170,JM=256,IM=512,NT=16)
      PARAMETER(NM=MM,NN=MM,IPOW=0)
      DIMENSION S((MM+1)*(MM+1)),G(0:IM-1,JM)
      DIMENSION IT(4),T(IM*6)      
      DIMENSION P(JM/2*(MM+4)),Q(JM/2*7*NT),R((MM+1)*(2*NM-MM-1))      
      DIMENSION WS(2*(NN+1)*NT),WG((IM+2)*JM),W((JM+1)*IM)
      DIMENSION WSC((2*NN+1-MM)*MM+NN+1)
      INTEGER IP(8)      

      RC=1D0*5*IM*LOG(1D0*IM)/LOG(2D0)*0.5D0*JM+1D0*(MM+1)*(MM+1)*JM
      ! 逆変換/正変換の1回あたりの演算数概算

      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)
      CALL SJVOPN(MM,NM,JM,IM,P,R,IP)      

      DO L=1,(MM+1)*(MM+1)
        S(L)=0        
      END DO

      DO ITRY=1,NTRY
        CALL APTIME(TIM0)
        DO ITR=1,NTR      
          CALL SJTSOG(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)
          CALL SJTGOS(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)
        END DO
        CALL APTIME(TIM1)
        TIM=TIM1-TIM0
        IF(ITRY.EQ.1) THEN
          TIMMIN=TIM
        ELSE IF(TIM.LT.TIMMIN) THEN
          TIMMIN=TIM
        END IF
      END DO

      GFLOPS=2*RC*NTR/TIMMIN/1D9
      WRITE(6,'(A,F6.3,A)') 'SJTSOG+SJTGOS: ',GFLOPS,' GFlops'

      DO ITRY=1,NTRY
        CALL APTIME(TIM0)
        DO ITR=1,NTR
          CALL SJVS2G(MM,NM,NN,IM,JM,S,G,IT,T,P,R,WSC,WG,W,IPOW,IP)
          CALL SJVG2S(MM,NM,NN,IM,JM,S,G,IT,T,P,R,WSC,WG,W,IPOW,IP)
        END DO
        CALL APTIME(TIM1)
        TIM=TIM1-TIM0
        IF(ITRY.EQ.1) THEN
          TIMMIN=TIM
        ELSE IF(TIM.LT.TIMMIN) THEN
          TIMMIN=TIM
        END IF
      END DO

      GFLOPS=2*RC*NTR/TIMMIN/1D9
      WRITE(6,'(A,F6.3,A)') 'SJVS2G+SJVG2S: ',GFLOPS,' GFlops'

      CALL SJVCLS(IP)      

      END
