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
*-----------------------------------------------------------------------
      SUBROUTINE LJTGWS(NM,NN,JM,M,S,G,P,Q,R,WS,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(2,M:NN),G(2,JM)
      DIMENSION P(JM/2,*),Q(JM/2,7),R(*)      
      DIMENSION WS(2,M:NN)

      JH=JM/2

      IF(IPOW.EQ.0) THEN
        DO J=1,JH
          Q(J,4)=(G(1,JH+J)+G(1,JH-J+1))*P(J,2)
          Q(J,6)=(G(1,JH+J)-G(1,JH-J+1))*P(J,2)        
          Q(J,5)=(G(2,JH+J)+G(2,JH-J+1))*P(J,2)
          Q(J,7)=(G(2,JH+J)-G(2,JH-J+1))*P(J,2)        
        END DO
      ELSE IF(IPOW.EQ.1) THEN
        DO J=1,JH
          Q(J,4)=(G(1,JH+J)+G(1,JH-J+1))*P(J,2)*P(J,4)
          Q(J,6)=(G(1,JH+J)-G(1,JH-J+1))*P(J,2)*P(J,4) 
          Q(J,5)=(G(2,JH+J)+G(2,JH-J+1))*P(J,2)*P(J,4) 
          Q(J,7)=(G(2,JH+J)-G(2,JH-J+1))*P(J,2)*P(J,4) 
        END DO
      ELSE IF(IPOW.EQ.2) THEN                
        DO J=1,JH
          Q(J,4)=(G(1,JH+J)+G(1,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,6)=(G(1,JH+J)-G(1,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
          Q(J,5)=(G(2,JH+J)+G(2,JH-J+1))*P(J,2)*P(J,4)*P(J,4) 
          Q(J,7)=(G(2,JH+J)-G(2,JH-J+1))*P(J,2)*P(J,4)*P(J,4)
        END DO
      ELSE
        CALL BSDMSG('E','LJTGWS','IPOW MUST BE 0, 1, or 2.')
      END IF

      N=M

      WS(1,N)=0
      WS(2,N)=0            
      DO J=1,JH
        Q(J,1)=P(J,1)
        Q(J,2)=P(J,4+M)
        Q(J,3)=P(J,1)*P(J,4+M)        
        WS(1,N)=WS(1,N)+Q(J,4)*P(J,4+M)
        WS(2,N)=WS(2,N)+Q(J,5)*P(J,4+M)        
      END DO

      NS=(2*NM-M)*M+NM-M

      IQ2=3
      IQ3=5-IQ2
      IQ4=6
      
      DO N=M+1,NN-1
        CALL LJLGWS(JH,WS(1,N),R(N-M+NS),
     &    Q(1,1),Q(1,IQ2),Q(1,IQ3),Q(1,IQ4))
        IQ2=5-IQ2
        IQ3=5-IQ3
        IQ4=10-IQ4
      END DO

      IQ5=IQ4+1      

      N=NN
      IF(N.NE.M) THEN
        WS(1,N)=0
        WS(2,N)=0            
        DO J=1,JH
          WS(1,N)=WS(1,N)+Q(J,IQ4)*Q(J,IQ2)
          WS(2,N)=WS(2,N)+Q(J,IQ5)*Q(J,IQ2)        
        END DO
      END IF

      NS=(2*NM-M)*M      

      N=M
      S(1,N)=WS(1,N)
      S(2,N)=WS(2,N)
      DO N=M+1,NN
        S(1,N)=R(N-M+NS)*WS(1,N)
        S(2,N)=R(N-M+NS)*WS(2,N)
      END DO

      END
