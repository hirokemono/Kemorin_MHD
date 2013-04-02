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
*     浅水方程式の初期値化                                    2000/10/26
************************************************************************
      SUBROUTINE N2SWBL(LM,KM,JM,IM,BARPHI,AVT,PHI,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-LM:LM,-KM:KM)
      DIMENSION PHI(-LM:LM,-KM:KM)
      DIMENSION WS(-LM:LM,-KM:KM,2)
      DIMENSION WG(JM*IM,5)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      DO K=-KM,KM
        DO L=-LM,LM
          PHI(L,K)=-(K*K+L*L)
        END DO
      END DO
      PHI(0,0)=1

* CALCULATE V,U COMPONENT

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K,1)=L*AVT(-L,-K)/PHI(L,K)
          PHI(L,K)=-K*AVT(-L,-K)/PHI(L,K)
        END DO
      END DO

* U=WG(1,2), V=WG(1,3)      

      CALL N2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG,ITJ,TJ,ITI,TI)
      CALL N2S2GA(LM,KM,JM,IM,PHI,WG(1,3),WG,ITJ,TJ,ITI,TI)      

* CALCULATE AVT COMPONENT

      CALL N2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI)

* CALCULATE U*AVT, V*AVT, (U*U+V*V)/2

      DO JI=1,JM*IM
        WG(JI,5)=WG(JI,3)*WG(JI,4)        
        WG(JI,4)=WG(JI,2)*WG(JI,4)
        WG(JI,2)=0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

      CALL N2G2SA(LM,KM,JM,IM,WG(1,2),PHI,WG,ITJ,TJ,ITI,TI)
      CALL N2G2SA(LM,KM,JM,IM,WG(1,4),WS(-LM,-KM,1),WG,ITJ,TJ,ITI,TI)
      CALL N2G2SA(LM,KM,JM,IM,WG(1,5),WS(-LM,-KM,2),WG,ITJ,TJ,ITI,TI)

      DO K=-KM,-1
        DO L=-LM,LM
          PHI(L,K)= -(L*WS(-L,-K,1)-K*WS(-L,-K,2))/(K*K+L*L)-PHI(L,K)
        END DO
      END DO

      DO K=1,KM
        DO L=-LM,LM
          PHI(L,K)= -(L*WS(-L,-K,1)-K*WS(-L,-K,2))/(K*K+L*L)-PHI(L,K)
        END DO
      END DO

      K=0
      DO L=-LM,-1
        PHI(L,K)= -(L*WS(-L,-K,1)-K*WS(-L,-K,2))/(K*K+L*L)-PHI(L,K)
      END DO
      DO L=1,LM
        PHI(L,K)= -(L*WS(-L,-K,1)-K*WS(-L,-K,2))/(K*K+L*L)-PHI(L,K)
      END DO

      PHI(0,0)=BARPHI

      END
