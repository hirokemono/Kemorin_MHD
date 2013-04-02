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
*     test2-align.f のためのサブルーチン                      2009/08/21
************************************************************************      
      SUBROUTINE SUB(T,Q,G,WG,W)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=170,JM=256,IM=512)
      PARAMETER(NM=MM+1)
      DIMENSION S((MM+1)*(MM+1))
      DIMENSION SY((MM+4)*MM+2)
      DIMENSION SX((MM+1)*(MM+1))
      DIMENSION SXR((MM+4)*MM+2)
      DIMENSION SYD((MM+1)*(MM+1))
      DIMENSION SXD((MM+1)*(MM+1))
      DIMENSION SD((MM+1)*(MM+1))
      DIMENSION SL((MM+1)*(MM+1))                  
      DIMENSION C((MM+1)*(MM+1))
      DIMENSION D((MM+1)*(MM+1)*2)
      DIMENSION GX(0:IM-1,JM),GY(0:IM-1,JM)
      DIMENSION IT(4),T(IM*6)      
      DIMENSION P(JM/2*(MM+4)),Q(JM/2*11),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION WG((IM+2)*JM)
      DIMENSION WS1(2*(NM+1)),WS2(2*(NM+1))
      DIMENSION W1((JM+1)*IM),W2((JM+1)*IM)      

      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)
      CALL SJINIC(MM,C)
      CALL SJINID(MM,D)

      DO L=1,(MM+1)*(MM+1)
        CALL SJL2NM(MM,L,N,M)
        S(L)=1D0/((ABS(M)+1D0)*(N+1D0))
      END DO

      CALL SJCS2X(MM,S,SX)
      CALL SJCRUP(MM,MM+1,SX,SXR)
      CALL SJCS2Y(MM,S,SY,C)
      
      CALL SJMS2G(MM,NM,MM+1,IM,JM,SXR,SY,GX,GY,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,1)

      CALL SJMG2S(MM,NM,MM+1,IM,JM,SXR,SY,GX,GY,IT,T,P,Q,R,WS1,WS2,
     &  WG,W1,W2,1)

      CALL SJCY2S(MM,SY,SYD,C)
      CALL SJCRDN(MM,MM+1,SXR,SX)
      CALL SJCS2X(MM,SX,SXD)

      DO L=1,(MM+1)*(MM+1)
        SD(L)=SXD(L)+SYD(L)
      END DO

      CALL SJCLAP(MM,S,SL,D,1)

      SLMAX=0
      DO L=1,(MM+1)*(MM+1)
        IF(ABS(SL(L)).GT.SLMAX) THEN
          SLMAX=ABS(SL(L))
        END IF
      END DO

      EPS=1D-11
      INDEX=0
      DO L=1,(MM+1)*(MM+1)
        CALL SJL2NM(MM,L,N,M)
        IF(ABS(SL(L)-SD(L)).GT.EPS*SLMAX) THEN
          print *,L,N,M,SL(L),SD(L),ABS(SL(L)-SD(L))/SLMAX
          INDEX=INDEX+1
        END IF
      END DO

      print *,'gradient and divergence check:'
      IF(INDEX.EQ.0) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF
        
      END
