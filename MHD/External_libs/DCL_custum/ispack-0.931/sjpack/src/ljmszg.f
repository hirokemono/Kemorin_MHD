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
*     TRANSFORM SPECTRA TO GRID (zonal component)             2009/08/11
*     (2��ʬƱ���Ѵ�)
*-----------------------------------------------------------------------
      SUBROUTINE LJMSZG(NM,NN,JM,S1,S2,G1,G2,P,Q,R,WS1,WS2,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S1(0:NN),S2(0:NN),G1(JM),G2(JM)
      DIMENSION P(JM/2,4),Q(JM/2,7),R(*)      
      DIMENSION WS1(0:NN),WS2(0:NN)      

      JH=JM/2
      M=0

      WS1(M)=S1(M)
      DO N=M+1,NN
        WS1(N)=R(N)*S1(N)
      END DO

      WS2(M)=S2(M)
      DO N=M+1,NN
        WS2(N)=R(N)*S2(N)
      END DO

      N=M
      DO J=1,JH
        Q(J,1)=P(J,1)
        Q(J,2)=1
        Q(J,3)=P(J,1)
        Q(J,4)=WS1(N)        
        Q(J,5)=0
        Q(J,6)=WS2(N)        
        Q(J,7)=0
      END DO

      NS=NM

      IQ2=3
      IQ3=5-IQ2
      IQ4=5
      IQ6=7
      DO N=M+1,NN-1
        CALL LJNSZG(JH,WS1(N),WS2(N),R(NS+N),
     &    Q(1,1),Q(1,IQ2),Q(1,IQ3),Q(1,IQ4),Q(1,IQ6))
        IQ2=5-IQ2
        IQ3=5-IQ3
        IQ4=9-IQ4
        IQ6=13-IQ6
      END DO

      N=NN
      DO J=1,JH
        Q(J,IQ4)=Q(J,IQ4)+WS1(N)*Q(J,IQ2)
        Q(J,IQ6)=Q(J,IQ6)+WS2(N)*Q(J,IQ2)
      END DO

      IF(IPOW.EQ.0) THEN
        DO J=1,JH
          G1(JH+J)  =Q(J,4)+Q(J,5)
          G1(JH-J+1)=Q(J,4)-Q(J,5)
          G2(JH+J)  =Q(J,6)+Q(J,7)
          G2(JH-J+1)=Q(J,6)-Q(J,7)
        END DO
      ELSE IF(IPOW.EQ.1) THEN
        DO J=1,JH
          G1(JH+J)  =(Q(J,4)+Q(J,5))*P(J,4)
          G1(JH-J+1)=(Q(J,4)-Q(J,5))*P(J,4)
          G2(JH+J)  =(Q(J,6)+Q(J,7))*P(J,4)
          G2(JH-J+1)=(Q(J,6)-Q(J,7))*P(J,4)
        END DO
      ELSE IF(IPOW.EQ.2) THEN        
        DO J=1,JH
          G1(JH+J)  =(Q(J,4)+Q(J,5))*P(J,4)*P(J,4)
          G1(JH-J+1)=(Q(J,4)-Q(J,5))*P(J,4)*P(J,4)
          G2(JH+J)  =(Q(J,6)+Q(J,7))*P(J,4)*P(J,4)
          G2(JH-J+1)=(Q(J,6)-Q(J,7))*P(J,4)*P(J,4)
        END DO
      ELSE
        CALL BSDMSG('E','LJMSZG','IPOW MUST BE 0, 1, or 2.')
      END IF

      END
