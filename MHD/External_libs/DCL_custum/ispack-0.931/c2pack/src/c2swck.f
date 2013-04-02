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
*     浅水方程式の保存量のチェック                            2000/10/10
************************************************************************
      SUBROUTINE C2SWCK(LM,KM,JM,IM,R,AVT,DIV,PHI,AENE,AENS,AMOM,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-KM:KM,LM)
      DIMENSION DIV(-KM:KM,0:LM)
      DIMENSION PHI(-KM:KM,0:LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG(0:JM,IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      CALL C2S2GA(LM,KM,JM,IM,PHI,WG(0,1,2),WG,ITJ,TJ,ITI,TI,4)
      CALL C2S2GA(LM,KM,JM,IM,AVT,WG(0,1,3),WG,ITJ,TJ,ITI,TI,3)

      AENS=0
      DO I=1,IM
        DO J=0,JM-1
          AENS=AENS+WG(J,I,3)*WG(J,I,3)/WG(J,I,2)
        END DO
      END DO
      AENS=AENS/(2*JM*IM)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-(-R*K*AVT(-K,L)-L*DIV(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(0,1,3),WG,ITJ,TJ,ITI,TI,3)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-(-R*K*DIV(-K,L)-L*AVT(K,L))/(R*K*R*K+L*L)
        END DO
      END DO
      L=0
      DO K=1,KM
        WS(K,0)=-(-R*K*DIV(-K,0))/(R*K*R*K)
        WS(-K,0)=-(R*K*DIV(K,0))/(R*K*R*K)        
      END DO
      WS(0,0)=0

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(0,1,4),WG,ITJ,TJ,ITI,TI,4)
      
      AENE=0
      DO I=1,IM
        DO J=0,JM-1
          AENE=AENE
     &      +WG(J,I,2)*(WG(J,I,3)*WG(J,I,3)
     &                  +WG(J,I,4)*WG(J,I,4)+WG(J,I,2))
        END DO
      END DO
      AENE=AENE/(2*JM*IM)

      AMOM=0
      DO I=1,IM
        DO J=0,JM-1
          AMOM=AMOM+WG(J,I,2)*WG(J,I,4)
        END DO
      END DO
      AMOM=AMOM/(JM*IM)
      
      END
