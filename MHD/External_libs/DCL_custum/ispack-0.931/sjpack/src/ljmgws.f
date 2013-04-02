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
*     TRANSFORM GRID TO SPECTRA (wave component)              2009/08/11
*     (2成分同時変換)
*-----------------------------------------------------------------------
      SUBROUTINE LJMGWS(NM,NN,JM,M,S1,S2,G1,G2,P,Q,R,WS1,WS2,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S1(2,M:NN),S2(2,M:NN)
      DIMENSION G1(2,JM),G2(2,JM)
      DIMENSION P(JM/2,*),Q(JM/2,11),R(*)
      DIMENSION WS1(2,M:NN),WS2(2,M:NN)      

      JH=JM/2

      IF(IPOW.EQ.0) THEN
        DO J=1,JH
          Q(J,4)=(G1(1,JH+J)+G1(1,JH-J+1))*P(J,2)
          Q(J,5)=(G1(2,JH+J)+G1(2,JH-J+1))*P(J,2)        
          Q(J,6)=(G1(1,JH+J)-G1(1,JH-J+1))*P(J,2)        
          Q(J,7)=(G1(2,JH+J)-G1(2,JH-J+1))*P(J,2)        
          Q(J,8)=(G2(1,JH+J)+G2(1,JH-J+1))*P(J,2)
          Q(J,9)=(G2(2,JH+J)+G2(2,JH-J+1))*P(J,2)        
          Q(J,10)=(G2(1,JH+J)-G2(1,JH-J+1))*P(J,2)        
          Q(J,11)=(G2(2,JH+J)-G2(2,JH-J+1))*P(J,2)        
        END DO
      ELSE IF(IPOW.EQ.1) THEN
        DO J=1,JH
          Q(J,4)=(G1(1,JH+J)+G1(1,JH-J+1))*P(J,2)*P(J,4)
          Q(J,5)=(G1(2,JH+J)+G1(2,JH-J+1))*P(J,2)*P(J,4)
          Q(J,6)=(G1(1,JH+J)-G1(1,JH-J+1))*P(J,2)*P(J,4) 
          Q(J,7)=(G1(2,JH+J)-G1(2,JH-J+1))*P(J,2)*P(J,4)        
          Q(J,8)=(G2(1,JH+J)+G2(1,JH-J+1))*P(J,2)*P(J,4)
          Q(J,9)=(G2(2,JH+J)+G2(2,JH-J+1))*P(J,2)*P(J,4)
          Q(J,10)=(G2(1,JH+J)-G2(1,JH-J+1))*P(J,2)*P(J,4)  
          Q(J,11)=(G2(2,JH+J)-G2(2,JH-J+1))*P(J,2)*P(J,4) 
        END DO
      ELSE IF(IPOW.EQ.2) THEN
        DO J=1,JH
          Q(J,4)=(G1(1,JH+J)+G1(1,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,5)=(G1(2,JH+J)+G1(2,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,6)=(G1(1,JH+J)-G1(1,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,7)=(G1(2,JH+J)-G1(2,JH-J+1))*P(J,2)*P(J,4)*P(J,4)        
          Q(J,8)=(G2(1,JH+J)+G2(1,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,9)=(G2(2,JH+J)+G2(2,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,10)=(G2(1,JH+J)-G2(1,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,11)=(G2(2,JH+J)-G2(2,JH-J+1))*P(J,2)*P(J,4)*P(J,4) 
        END DO
      ELSE
        CALL BSDMSG('E','LJMGWS','IPOW MUST BE 0, 1, or 2.')
      END IF

      N=M

      WS1(1,N)=0
      WS1(2,N)=0            
      WS2(1,N)=0
      WS2(2,N)=0            
      DO J=1,JH
        Q(J,1)=P(J,1)
        Q(J,2)=P(J,4+M)
        Q(J,3)=P(J,1)*P(J,4+M)        
        WS1(1,N)=WS1(1,N)+Q(J,4)*P(J,4+M)
        WS1(2,N)=WS1(2,N)+Q(J,5)*P(J,4+M)        
        WS2(1,N)=WS2(1,N)+Q(J,8)*P(J,4+M)
        WS2(2,N)=WS2(2,N)+Q(J,9)*P(J,4+M)        
      END DO

      NS=(2*NM-M)*M+NM-M

      IQ2=3
      IQ3=5-IQ2
      IQ4=6
      IQ8=10
      
      DO N=M+1,NN-1
        CALL LJNGWS(JH,WS1(1,N),WS2(1,N),R(N-M+NS),
     &    Q(1,1),Q(1,IQ2),Q(1,IQ3),Q(1,IQ4),Q(1,IQ8))
        IQ2=5-IQ2
        IQ3=5-IQ3
        IQ4=10-IQ4
        IQ8=18-IQ8
      END DO

      IQ5=IQ4+1
      IQ9=IQ8+1

      N=NN
      IF(N.NE.M) THEN
        WS1(1,N)=0
        WS1(2,N)=0            
        WS2(1,N)=0
        WS2(2,N)=0            
        DO J=1,JH
          WS1(1,N)=WS1(1,N)+Q(J,IQ4)*Q(J,IQ2)
          WS1(2,N)=WS1(2,N)+Q(J,IQ5)*Q(J,IQ2)        
          WS2(1,N)=WS2(1,N)+Q(J,IQ8)*Q(J,IQ2)
          WS2(2,N)=WS2(2,N)+Q(J,IQ9)*Q(J,IQ2)        
        END DO
      END IF

      NS=(2*NM-M)*M      

      N=M
      S1(1,N)=WS1(1,N)
      S1(2,N)=WS1(2,N)
      S2(1,N)=WS2(1,N)
      S2(2,N)=WS2(2,N)
      DO N=M+1,NN
        S1(1,N)=R(N-M+NS)*WS1(1,N)
        S1(2,N)=R(N-M+NS)*WS1(2,N)
        S2(1,N)=R(N-M+NS)*WS2(1,N)
        S2(2,N)=R(N-M+NS)*WS2(2,N)
      END DO

      END
