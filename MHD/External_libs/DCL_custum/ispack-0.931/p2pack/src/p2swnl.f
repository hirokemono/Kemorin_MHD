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
*     CALCULATE NONLINEAR TERM FOR SW EQ.                     2001/07/23
************************************************************************
      SUBROUTINE P2SWNL(LM,KM,JM,IM,R,AVT,DIV,PHI,DAVT,DDIV,DPHI,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-LM:LM,-KM:KM)
      DIMENSION DIV(-LM:LM,-KM:KM)
      DIMENSION PHI(-LM:LM,-KM:KM)
      DIMENSION DAVT(-LM:LM,-KM:KM)
      DIMENSION DDIV(-LM:LM,-KM:KM)
      DIMENSION DPHI(-LM:LM,-KM:KM)
      DIMENSION WS(-LM:LM,-KM:KM)
      DIMENSION WG(JM*IM,4)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      DO K=-KM,KM
        DO L=-LM,LM
          DPHI(L,K)=-((R*K)*(R*K)+L*L)
        END DO
      END DO
      DPHI(0,0)=1

* V --> DAVT, U --> DDIV      

      DO K=-KM,KM
        DO L=-LM,LM
          DAVT(L,K)=(-L*DIV(-L,-K)-R*K*AVT(-L,-K))/DPHI(L,K)          
          DDIV(L,K)=(-R*K*DIV(-L,-K)+L*AVT(-L,-K))/DPHI(L,K)
        END DO
      END DO

* U=WG(1,2)

      CALL P2S2GA(LM,KM,JM,IM,DDIV,WG(1,2),WG,ITJ,TJ,ITI,TI)

* CALCULATE PHI COMPONENT

      CALL P2S2GA(LM,KM,JM,IM,PHI,WG(1,4),WG,ITJ,TJ,ITI,TI)

* U*PHI --> WG(*,3) --> DDIV

      DO JI=1,JM*IM
        WG(JI,3)=WG(JI,2)*WG(JI,4)        
      END DO

      CALL P2G2SA(LM,KM,JM,IM,WG(1,3),DDIV,WG,ITJ,TJ,ITI,TI)

* V=WG(*,3)

      CALL P2S2GA(LM,KM,JM,IM,DAVT,WG(1,3),WG,ITJ,TJ,ITI,TI)

* V*PHI --> WG(*,4) --> DAVT

      DO JI=1,JM*IM
        WG(JI,4)=WG(JI,3)*WG(JI,4)
      END DO

      CALL P2G2SA(LM,KM,JM,IM,WG(1,4),DAVT,WG,ITJ,TJ,ITI,TI)

* CALCULATE DPHI

      DO K=-KM,KM
        DO L=-LM,LM
          DPHI(L,K)=R*K*DDIV(-L,-K)+L*DAVT(-L,-K)
        END DO
      END DO

* (U*U+V*V)/2 --> WG(*,4)
      
      DO JI=1,JM*IM
        WG(JI,4)=0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

* (U*U+V*V)/2 --> DDIV

      CALL P2G2SA(LM,KM,JM,IM,WG(1,4),DDIV,WG,ITJ,TJ,ITI,TI)

* AVT --> WG(*,4)

      CALL P2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI)

* V*AVT --> WG(*,3), U*AVT --> WG(*,2)

      DO JI=1,JM*IM
        WG(JI,3)=WG(JI,3)*WG(JI,4)        
        WG(JI,2)=WG(JI,2)*WG(JI,4)
      END DO

* U*AVT --> WS

      CALL P2G2SA(LM,KM,JM,IM,WG(1,2),WS,WG,ITJ,TJ,ITI,TI)

      DO K=-KM,KM
        DO L=-LM,LM
          DAVT(L,K)= R*K*WS(-L,-K)
          DDIV(L,K)= L*WS(-L,-K)+((R*K)*(R*K)+L*L)*(DDIV(L,K)+PHI(L,K))
     &              
        END DO
      END DO

* V*AVT --> WS
      
      CALL P2G2SA(LM,KM,JM,IM,WG(1,3),WS,WG,ITJ,TJ,ITI,TI)

      DO K=-KM,KM
        DO L=-LM,LM
          DAVT(L,K)=DAVT(L,K)+L*WS(-L,-K)
          DDIV(L,K)=DDIV(L,K)-R*K*WS(-L,-K)
        END DO
      END DO

      END
