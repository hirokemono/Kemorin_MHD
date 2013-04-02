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
*     CALCULATE NONLINEAR TERM FOR SW EQ.                     2000/10/06
************************************************************************
      SUBROUTINE C2SWNL(LM,KM,JM,IM,R,AVT,DIV,PHI,DAVT,DDIV,DPHI,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-KM:KM,LM)
      DIMENSION DIV(-KM:KM,0:LM)
      DIMENSION PHI(-KM:KM,0:LM)
      DIMENSION DAVT(-KM:KM,LM)
      DIMENSION DDIV(-KM:KM,0:LM)
      DIMENSION DPHI(-KM:KM,0:LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* V --> DAVT, U --> DDIV

      DO L=1,LM
        DO K=-KM,KM
          DAVT(K,L)=-(-R*K*AVT(-K,L)-L*DIV(K,L))/(R*K*R*K+L*L)
          DDIV(K,L)=-(-R*K*DIV(-K,L)-L*AVT(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      L=0
      DO K=1,KM
        DDIV(K,0)=-(-R*K*DIV(-K,0))/(R*K*R*K)
        DDIV(-K,0)=-(R*K*DIV(K,0))/(R*K*R*K)
      END DO
      DDIV(0,0)=0

* U --> WG(*,2)

      CALL C2S2GA(LM,KM,JM,IM,DDIV,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* CALCULATE PHI COMPONENT

      CALL C2S2GA(LM,KM,JM,IM,PHI,WG(1,4),WG,ITJ,TJ,ITI,TI,4)

* U*PHI --> WG(*,3)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,2)*WG(JI,4)        
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),DDIV,WG,ITJ,TJ,ITI,TI,4)

* V=WG(*,3)
      
      CALL C2S2GA(LM,KM,JM,IM,DAVT,WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* V*PHI --> WG(*,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,3)*WG(JI,4)
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),DAVT,WG,ITJ,TJ,ITI,TI,3)

* CALCULATE DPHI      

      DO L=1,LM
        DO K=-KM,KM
          DPHI(K,L)=R*K*DDIV(-K,L)-L*DAVT(K,L)
        END DO
      END DO

      L=0
      DO K=-KM,KM
        DPHI(K,0)=R*K*DDIV(-K,0)
      END DO

* (U*U+V*V)/2 --> WG(*,4)
      
      DO JI=1,(JM+1)*IM
        WG(JI,4)=0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

* (U*U+V*V)/2 --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS,WG,ITJ,TJ,ITI,TI,4)      
      
* AVT --> WG(*,4)

      CALL C2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI,3)

* V*AVT --> WG(*,3), U*AVT --> WG(*,2)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,4)        
        WG(JI,2)=WG(JI,2)*WG(JI,4)
      END DO

* V*AVT --> DDIV, U*AVT --> DAVT

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),DAVT,WG,ITJ,TJ,ITI,TI,3)
      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),DDIV,WG,ITJ,TJ,ITI,TI,4)

      DO L=1,LM
        DO K=1,KM
          DAVTK=DAVT(K,L)
          DDIVK=DDIV(-K,L)
          DAVT(K,L)=R*K*DAVT(-K,L)+L*DDIV(K,L)
          DDIV(-K,L)=R*K*DDIV(K,L)-L*DAVT(-K,L)
     &      +(R*K*R*K+L*L)*(WS(-K,L)+PHI(-K,L))
          DAVT(-K,L)=-R*K*DAVTK+L*DDIVK
          DDIV(K,L)=-R*K*DDIVK-L*DAVTK
     &      +(R*K*R*K+L*L)*(WS(K,L)+PHI(K,L))
        END DO
      END DO

      DO L=1,LM
        K=0
        DAVTK=DAVT(0,L)
        DAVT(0,L)=L*DDIV(0,L)
        DDIV(0,L)=-L*DAVTK+L*L*(WS(0,L)+PHI(0,L))
      END DO

      L=0
      DO K=1,KM
        DDIVK=DDIV(-K,0)
        DDIV(-K,0)=R*K*DDIV(K,0)+(R*K*R*K)*(WS(-K,0)+PHI(-K,0))
        DDIV(K,0)=-R*K*DDIVK+(R*K*R*K)*(WS(K,0)+PHI(K,0))
      END DO
      DDIV(0,0)=0

      END
