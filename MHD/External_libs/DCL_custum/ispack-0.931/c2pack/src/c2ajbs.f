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
*     CALCULATE NONLINEAR TERM                                2000/10/02
************************************************************************
      SUBROUTINE C2AJBS(LM,KM,JM,IM,R,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-KM:KM,LM)
      DIMENSION DZ(-KM:KM,LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,3)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      DO L=1,LM
        DO K=-KM,KM
          DZ(K,L)=-Z(K,L)/((R*K)*(R*K)+L*L)
        END DO
      END DO

* CALCULATE V COMPONENT --> WG(*,2)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-R*K*DZ(-K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,2),WG,ITJ,TJ,ITI,TI,3)

* CALCULATE U COMPONENT --> WG(*,3)

      CALL BSSET0(2*KM+1,WS)
      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-L*DZ(K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(1,3),WG,ITJ,TJ,ITI,TI,4)

* V*V-U*U --> WG(*,1), UV --> WG(*,2)

      DO JI=1,(JM+1)*IM
        WG(JI,1)=WG(JI,2)*WG(JI,2)-WG(JI,3)*WG(JI,3)
        WG(JI,2)=WG(JI,2)*WG(JI,3)
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG,WS,WG(1,3),ITJ,TJ,ITI,TI,4)      
      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),DZ,WG,ITJ,TJ,ITI,TI,3)

      DO L=1,LM
        DO K=-KM,KM
          DZ(K,L)=((R*K)*(R*K)-L*L)*DZ(K,L)-(R*K)*L*WS(-K,L)
        END DO
      END DO

      END
