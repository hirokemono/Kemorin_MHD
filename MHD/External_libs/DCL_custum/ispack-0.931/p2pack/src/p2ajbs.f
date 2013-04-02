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
*     CALCULATE NONLINEAR TERM                                2001/07/23
************************************************************************
      SUBROUTINE P2AJBS(LM,KM,JM,IM,R,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-LM:LM,-KM:KM)
      DIMENSION DZ(-LM:LM,-KM:KM)
      DIMENSION WS(-LM:LM,-KM:KM)
      DIMENSION WG(JM*IM,3)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      DO K=-KM,KM
        DO L=-LM,LM
          DZ(L,K)=-((R*K)*(R*K)+L*L)
        END DO
      END DO
      DZ(0,0)=1

      DO K=-KM,KM
        DO L=-LM,LM
          DZ(L,K)=Z(L,K)/DZ(L,K)
        END DO
      END DO

* CALCULATE V COMPONENT

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K)=-R*K*DZ(-L,-K)
        END DO
      END DO

      CALL P2S2GA(LM,KM,JM,IM,WS,WG(1,1),WG(1,3),ITJ,TJ,ITI,TI)

* CALCULATE U COMPONENT

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K)=L*DZ(-L,-K)
        END DO
      END DO

      CALL P2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG(1,3),ITJ,TJ,ITI,TI)

      DO JI=1,JM*IM
        WG(JI,3)=WG(JI,1)*WG(JI,1)-WG(JI,2)*WG(JI,2)
        WG(JI,2)=WG(JI,1)*WG(JI,2)
      END DO

      CALL P2G2SA(LM,KM,JM,IM,WG(1,3),WS,WG,ITJ,TJ,ITI,TI)
      CALL P2G2SA(LM,KM,JM,IM,WG(1,2),DZ,WG,ITJ,TJ,ITI,TI)

      DO K=-KM,KM
        DO L=-LM,LM
          DZ(L,K)=((R*K)*(R*K)-L*L)*DZ(L,K)+(R*K)*L*WS(L,K)
        END DO
      END DO

      END
