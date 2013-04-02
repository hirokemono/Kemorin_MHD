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
*     bench-align.f のためのサブルーチン                      2009/08/21
************************************************************************      
      SUBROUTINE SUB(T,Q,G,WG,W)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NTRY=8,NTR=10)
      PARAMETER(MM=170,JM=256,IM=512)
      PARAMETER(NM=MM,NN=MM,IPOW=0)
      PARAMETER(JW=JM*3/2,IW=IM+3*(MM+1))
      PARAMETER(ID=IM+1,JD=JM+1)
      DIMENSION S((MM+1)*(MM+1)),G(0:IM-1,JM)
      DIMENSION SD((MM+1)*(MM+1))
      DIMENSION IT(4),T(IM*6)      
      DIMENSION P(JM/2*(MM+4)),Q(JM/2*7),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION WS(2*(NN+1)),WG((IM+2)*JM),W((JM+1)*IM)

      DIMENSION SN((MM+1)*(MM+1)),GN(0:ID-1,JD)
      DIMENSION ITN(5),TN(IM*2)
      DIMENSION YN(JM/2,4)
      DIMENSION IPN(((MM+1)/2+MM+1)*2)
      DIMENSION PN(((MM+1)/2+MM+1)*JM)
      DIMENSION RN(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IAN((MM+1)*(MM+1),4)
      DIMENSION AN((MM+1)*(MM+1),6)
      DIMENSION QN(((MM+1)/2+MM+1)*JM)
      DIMENSION WWN(JW*IW),WSN(JW*IW)

      RC=1D0*5*IM*LOG(1D0*IM)/LOG(2D0)*0.5D0*JM+1D0*(MM+1)*(MM+1)*JM
      ! 逆変換/正変換の1回あたりの演算数概算

      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)
      CALL SNINIT(MM,IM,JM,ITN,TN,YN,IPN,PN,RN,IAN,AN)

      DO L=1,(MM+1)*(MM+1)
        SN(L)=0
        S(L)=0        
      END DO

      DO ITRY=1,NTRY
        CALL APTIME(TIM0)
        DO ITR=1,NTR      
          CALL SJTS2G(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)
          CALL SJTG2S(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)
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
      WRITE(6,'(A,F6.3,A)') 'SJPACK: ',GFLOPS,' GFlops'

      DO ITRY=1,NTRY
        CALL APTIME(TIM0)
        DO ITR=1,NTR      
          CALL SNTS2G(MM,IM,ID,JM,JD,1,
     &      SN,GN,ITN,TN,YN,IPN,PN,RN,IAN,AN,QN,WSN,WWN,IPOW,0)
          CALL SNTG2S(MM,IM,ID,JM,JD,1,
     &      GN,SN,ITN,TN,YN,IPN,PN,RN,IAN,AN,QN,WSN,WWN,IPOW,0)
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
      WRITE(6,'(A,F6.3,A)') 'SNPACK: ',GFLOPS,' GFlops'
        
      END
