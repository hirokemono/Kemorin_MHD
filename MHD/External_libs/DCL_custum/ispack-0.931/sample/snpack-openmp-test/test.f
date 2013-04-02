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
      PARAMETER(MM=170,IM=512,JM=256,KM=1)
      PARAMETER(ID=IM+1,JD=JM+1)
      PARAMETER(NP=8,ITR=1)
      PARAMETER(LM=(MM+1)*(MM+1),JH=JM/2)
      PARAMETER(IW=IM+3*(MM+1),JW=JD*3/2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION G(ID,JM,KM)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(KM*JW*IW),WS(KM*JW*IW),WV(KM*(MM+3)*(MM+4)*NP)

      CALL SNINIT(MM,IM,JM,IT,T,Y,IP,P,R,IA,A)

      IFLAG=0

      DO L=1,LM
        S(L,1)=1+0.00001D0*L
      END DO
      
      CALL APTIME(TIME0)

      DO I=1,ITR
        CALL SNTSOG(MM,IM,ID,JM,KM,
     &    S,G,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,WV,0,IFLAG)
        CALL SNTGOS(MM,IM,ID,JM,KM,
     &    G,S,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,WV,0,IFLAG)
      END DO        
        CALL APTIME(TIME1)
        print *,'TIME: ',TIME1-TIME0

      DO L=1,LM
        print *, L,S(L,1)
      END DO

      END
