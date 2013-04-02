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
*     TRANSFORM SPECTRA TO GRID (wave component)              2009/08/11
*-----------------------------------------------------------------------
      SUBROUTINE LJTSWG(NM,NN,JM,M,S,G,P,Q,R,WS,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(2,M:NN),G(2,JM),P(JM/2,*),Q(JM/2,7),R(*),WS(2,M:NN)

      JH=JM/2

      NS=(2*NM-M)*M

      N=M
      WS(1,N)=S(1,N)
      WS(2,N)=S(2,N)
      DO N=M+1,NN
        WS(1,N)=R(N-M+NS)*S(1,N)
        WS(2,N)=R(N-M+NS)*S(2,N)
      END DO

      N=M
      DO J=1,JH
        Q(J,1)=P(J,1)
        Q(J,2)=P(J,4+M)
        Q(J,3)=P(J,1)*P(J,4+M)        
        Q(J,4)=WS(1,N)*P(J,4+M)
        Q(J,5)=WS(2,N)*P(J,4+M)
        Q(J,6)=0
        Q(J,7)=0
      END DO

      IQ2=3
      IQ3=5-IQ2
      IQ4=6

      NS=NS+NM-M
      
      DO N=M+1,NN-1
        CALL LJLSWG(JH,WS(1,N),R(N-M+NS),
     &    Q(1,1),Q(1,IQ2),Q(1,IQ3),Q(1,IQ4))
        IQ2=5-IQ2
        IQ3=5-IQ3
        IQ4=10-IQ4
      END DO

      IQ5=IQ4+1
      N=NN
      IF(N.NE.M) THEN
        DO J=1,JH
          Q(J,IQ4)=Q(J,IQ4)+WS(1,N)*Q(J,IQ2)
          Q(J,IQ5)=Q(J,IQ5)+WS(2,N)*Q(J,IQ2)
        END DO
      END IF
      
      IF(IPOW.EQ.0) THEN
        DO J=1,JH
          G(1,JH+J)  =Q(J,4)+Q(J,6)
          G(1,JH-J+1)=Q(J,4)-Q(J,6)
          G(2,JH+J)  =Q(J,5)+Q(J,7)
          G(2,JH-J+1)=Q(J,5)-Q(J,7)
        END DO
      ELSE IF(IPOW.EQ.1) THEN        
        DO J=1,JH
          G(1,JH+J)  =(Q(J,4)+Q(J,6))*P(J,4)
          G(1,JH-J+1)=(Q(J,4)-Q(J,6))*P(J,4)
          G(2,JH+J)  =(Q(J,5)+Q(J,7))*P(J,4)
          G(2,JH-J+1)=(Q(J,5)-Q(J,7))*P(J,4)
        END DO
      ELSE IF(IPOW.EQ.2) THEN
        DO J=1,JH
          G(1,JH+J)  =(Q(J,4)+Q(J,6))*P(J,4)*P(J,4)
          G(1,JH-J+1)=(Q(J,4)-Q(J,6))*P(J,4)*P(J,4)
          G(2,JH+J)  =(Q(J,5)+Q(J,7))*P(J,4)*P(J,4)
          G(2,JH-J+1)=(Q(J,5)-Q(J,7))*P(J,4)*P(J,4)
        END DO
      ELSE
        CALL BSDMSG('E','LJTSWG','IPOW MUST BE 0, 1, or 2.')
      END IF
        
      END
