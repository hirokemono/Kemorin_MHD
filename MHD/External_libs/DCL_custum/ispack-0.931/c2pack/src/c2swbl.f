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
*     浅水方程式の初期値化                                    2000/10/11
************************************************************************
      SUBROUTINE C2SWBL(LM,KM,JM,IM,R,BARPHI,AVT,PHI,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-KM:KM,LM)
      DIMENSION PHI(-KM:KM,0:LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* V --> WS, U --> PHI

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-(-R*K*AVT(-K,L))/(R*K*R*K+L*L)
          PHI(K,L)=-(-L*AVT(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      L=0
      DO K=-K,KM
        PHI(K,0)=0
      END DO

* U --> WG(*,2)

      CALL C2S2GA(LM,KM,JM,IM,PHI,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* V --> WG(*,3)
      
      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* -(U*U+V*V)/2 --> WG(*,4)
      
      DO JI=1,(JM+1)*IM
        WG(JI,4)=-0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

* -(U*U+V*V)/2 --> PHI

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),PHI,WG,ITJ,TJ,ITI,TI,4)      
      
* AVT --> WG(*,4)

      CALL C2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI,3)

* V*AVT --> WG(*,3), U*AVT --> WG(*,2)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,4)        
        WG(JI,2)=WG(JI,2)*WG(JI,4)
      END DO

* U*AVT --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

      DO L=1,LM
        DO K=-KM,KM
          PHI(K,L)=PHI(K,L)+(L*WS(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

* V*AVT --> WS
      
      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),WS,WG,ITJ,TJ,ITI,TI,4)

      DO L=1,LM
        DO K=-KM,KM
          PHI(K,L)=PHI(K,L)+(R*K*WS(-K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      L=0
      DO K=1,KM
        PHI(K,L)=PHI(K,L)+(R*K*WS(-K,L))/(R*K*R*K+L*L)
        PHI(-K,L)=PHI(-K,L)+(-R*K*WS(K,L))/(R*K*R*K+L*L)        
      END DO

      PHI(0,0)=BARPHI

      END
