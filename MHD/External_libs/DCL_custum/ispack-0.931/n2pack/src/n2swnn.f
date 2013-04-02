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
*     CALCULATE NONLINEAR TERM FOR SW EQ. (NL term ONLY)      2000/10/26
************************************************************************
      SUBROUTINE N2SWNN(LM,KM,JM,IM,BARPHI,F,AVT,DIV,PHI,DAVT,DDIV,DPHI,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-LM:LM,-KM:KM)
      DIMENSION DIV(-LM:LM,-KM:KM)
      DIMENSION PHI(-LM:LM,-KM:KM)
      DIMENSION DAVT(-LM:LM,-KM:KM)
      DIMENSION DDIV(-LM:LM,-KM:KM)
      DIMENSION DPHI(-LM:LM,-KM:KM)
      DIMENSION WS(-LM:LM,-KM:KM,2)
      DIMENSION WG(JM*IM,5)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      DO K=-KM,KM
        DO L=-LM,LM
          DPHI(L,K)=-(K*K+L*L)
        END DO
      END DO
      DPHI(0,0)=1

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K,1)=DIV(L,K)/DPHI(L,K)
          WS(L,K,2)=AVT(L,K)/DPHI(L,K)          
        END DO
      END DO

* CALCULATE V,U COMPONENT

      DO K=-KM,KM
        DO L=-LM,LM
          DAVT(L,K)=-K*WS(-L,-K,1)+L*WS(-L,-K,2)
          DDIV(L,K)=-L*WS(-L,-K,1)-K*WS(-L,-K,2)
        END DO
      END DO

* U=WG(1,2), V=WG(1,3)      

      CALL N2S2GA(LM,KM,JM,IM,DAVT,WG(1,2),WG,ITJ,TJ,ITI,TI)
      CALL N2S2GA(LM,KM,JM,IM,DDIV,WG(1,3),WG,ITJ,TJ,ITI,TI)      

* CALCULATE PHI COMPONENT

      CALL N2S2GA(LM,KM,JM,IM,PHI,WG(1,4),WG,ITJ,TJ,ITI,TI)

* CALCULATE U*PHI, V*PHI

      DO JI=1,JM*IM
        WG(JI,5)=WG(JI,3)*(WG(JI,4)-BARPHI)
        WG(JI,4)=WG(JI,2)*(WG(JI,4)-BARPHI)
      END DO

      CALL N2G2SA(LM,KM,JM,IM,WG(1,4),DAVT,WG,ITJ,TJ,ITI,TI)
      CALL N2G2SA(LM,KM,JM,IM,WG(1,5),DDIV,WG,ITJ,TJ,ITI,TI)

* CALCULATE DPHI

      DO K=-KM,KM
        DO L=-LM,LM
          DPHI(L,K)=K*DAVT(-L,-K)+L*DDIV(-L,-K)
        END DO
      END DO

* CALCULATE AVT COMPONENT

      CALL N2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI)

* CALCULATE U*AVT, V*AVT, (U*U+V*V)/2

      DO JI=1,JM*IM
        WG(JI,5)=WG(JI,3)*(WG(JI,4)-F)
        WG(JI,4)=WG(JI,2)*(WG(JI,4)-F)
        WG(JI,2)=0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

      CALL N2G2SA(LM,KM,JM,IM,WG(1,2),DDIV,WG,ITJ,TJ,ITI,TI)
      CALL N2G2SA(LM,KM,JM,IM,WG(1,4),WS(-LM,-KM,1),WG,ITJ,TJ,ITI,TI)
      CALL N2G2SA(LM,KM,JM,IM,WG(1,5),WS(-LM,-KM,2),WG,ITJ,TJ,ITI,TI)

      DO K=-KM,KM
        DO L=-LM,LM
          DAVT(L,K)= K*WS(-L,-K,1)+L*WS(-L,-K,2)
          DDIV(L,K)= L*WS(-L,-K,1)-K*WS(-L,-K,2)+(K*K+L*L)*DDIV(L,K)
        END DO
      END DO

      END
