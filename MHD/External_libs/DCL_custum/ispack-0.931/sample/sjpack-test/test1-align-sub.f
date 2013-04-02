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
*     test1-align.f のためのサブルーチン                      2009/08/21
************************************************************************      
      SUBROUTINE SUB(T,Q,G,WG,W)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=170,JM=256,IM=512)
      PARAMETER(NM=MM,NN=MM,IPOW=0)
      PARAMETER(JW=JM*3/2,IW=IM+3*(MM+1))
      DIMENSION S((MM+1)*(MM+1)),G(0:IM-1,JM)
      DIMENSION SD((MM+1)*(MM+1))
      DIMENSION IT(4),T(IM*6)
      DIMENSION P(JM/2*(MM+4)),Q(JM/2*7),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION WS(2*(NN+1)),WG((IM+2)*JM),W((JM+1)*IM)

      DIMENSION SN((MM+1)*(MM+1)),GN(0:IM-1,JM)
      DIMENSION ITN(5),TN(IM*2)
      DIMENSION YN(JM/2,4)
      DIMENSION IPN(((MM+1)/2+MM+1)*2)
      DIMENSION PN(((MM+1)/2+MM+1)*JM)
      DIMENSION RN(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IAN((MM+1)*(MM+1),4)
      DIMENSION AN((MM+1)*(MM+1),6)
      DIMENSION QN(((MM+1)/2+MM+1)*JM)
      DIMENSION WWN(JW*IW),WSN(JW*IW)
      
      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)
      CALL SNINIT(MM,IM,JM,ITN,TN,YN,IPN,PN,RN,IAN,AN)

* 逆変換のチェック

      DO L=1,(MM+1)*(MM+1)
        CALL SNL2NM(L,N,M)
        SN(L)=1D0/((ABS(M)+1D0)*(N+1D0))
      END DO
      CALL SJCN2J(MM,SN,S)
      
      CALL SJTS2G(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)
      CALL SNTS2G(MM,IM,IM,JM,JM,1,
     &  SN,GN,ITN,TN,YN,IPN,PN,RN,IAN,AN,QN,WSN,WWN,IPOW,0)

      GMAX=0
      DO I=0,IM-1
        DO J=1,JM
          IF(ABS(G(I,J)).GT.GMAX) THEN
            GMAX=ABS(G(I,J))
          END IF
        END DO
      END DO

      EPS=1D-11

      INDEX=0
      DO I=0,IM-1
        DO J=1,JM
          IF(ABS(G(I,J)-GN(I,J)).GT.EPS*GMAX) THEN
            print *,I,J,G(I,J),GN(I,J),G(I,J)-GN(I,J)
            INDEX=INDEX+1
          END IF
        END DO
      END DO

      print *,'backward transform:'
      IF(INDEX.EQ.0) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF

* 正変換のチェック

      DO I=0,IM-1
        DO J=1,JM
            GN(I,J)=G(I,J)
        END DO
      END DO

      CALL SNTG2S(MM,IM,IM,JM,JM,1,
     &  GN,SN,ITN,TN,YN,IPN,PN,RN,IAN,AN,QN,WSN,WWN,IPOW,0)
      CALL SJTG2S(MM,NM,NN,IM,JM,S,G,IT,T,P,Q,R,WS,WG,W,IPOW)

      CALL SJCJ2N(MM,S,SD)

      SMAX=0
      DO L=1,(MM+1)*(MM+1)
        IF(ABS(SD(L)).GT.SMAX) THEN
          SMAX=ABS(SD(L))
        END IF
      END DO

      INDEX=0
      DO L=1,(MM+1)*(MM+1)
        IF(ABS(SD(L)-SN(L)).GT.EPS*SMAX) THEN
          print *,L,SD(L),SN(L),SD(L)-SN(L)
          INDEX=INDEX+1
        END IF
      END DO

      print *,'forward transform:'
      IF(INDEX.EQ.0) THEN
        print *,'** OK'
      ELSE
        print *,'** Fail'
      END IF
        
      END
