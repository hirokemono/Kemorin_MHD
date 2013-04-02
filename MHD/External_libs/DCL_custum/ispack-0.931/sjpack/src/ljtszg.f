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
*-----------------------------------------------------------------------
      SUBROUTINE LJTSZG(NM,NN,JM,S,G,P,Q,R,WS,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(0:NN),G(JM),WS(0:NN),P(JM/2,4),Q(JM/2,5),R(*)

      JH=JM/2
      M=0

      WS(M)=S(M)
      DO N=M+1,NN
        WS(N)=R(N)*S(N)
      END DO

      N=M
      DO J=1,JH
        Q(J,1)=P(J,1)
        Q(J,2)=1
        Q(J,3)=P(J,1)
        Q(J,4)=WS(N)        
        Q(J,5)=0
      END DO

      NS=NM

      IQ2=3
      IQ3=5-IQ2
      IQ4=5
      DO N=M+1,NN-1
        CALL LJLSZG(JH,WS(N),R(NS+N),Q(1,1),Q(1,IQ2),Q(1,IQ3),Q(1,IQ4))
        IQ2=5-IQ2
        IQ3=5-IQ3
        IQ4=9-IQ4
      END DO

      N=NN
      DO J=1,JH
        Q(J,IQ4)=Q(J,IQ4)+WS(N)*Q(J,IQ2)
      END DO

      IF(IPOW.EQ.0) THEN
        DO J=1,JH
          G(JH+J)  =Q(J,4)+Q(J,5)
          G(JH-J+1)=Q(J,4)-Q(J,5)
        END DO
      ELSE IF(IPOW.EQ.1) THEN
        DO J=1,JH
          G(JH+J)  =(Q(J,4)+Q(J,5))*P(J,4)
          G(JH-J+1)=(Q(J,4)-Q(J,5))*P(J,4)
        END DO
      ELSE IF(IPOW.EQ.2) THEN        
        DO J=1,JH
          G(JH+J)  =(Q(J,4)+Q(J,5))*P(J,4)*P(J,4)
          G(JH-J+1)=(Q(J,4)-Q(J,5))*P(J,4)*P(J,4)
        END DO
      ELSE
        CALL BSDMSG('E','LJTSZG','IPOW MUST BE 0, 1, or 2.')
      END IF

      END
