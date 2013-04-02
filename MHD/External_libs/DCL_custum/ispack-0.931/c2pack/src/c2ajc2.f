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
*     CALCULATE JACOBIAN for 2 components                     2000/10/06
************************************************************************
      SUBROUTINE C2AJC2(LM,KM,JM,IM,SA,SB1,SB2,SC1,SC2,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA(-KM:KM,LM)
      DIMENSION SB1(-KM:KM,LM),SB2(-KM:KM,LM)
      DIMENSION SC1(-KM:KM,LM),SC2(-KM:KM,LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* ¢ßA/¢ßy --> WG(JI,2)

      CALL BSSET0(2*KM+1,WS)
      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=L*SA(K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* B1 --> WG(JI,3)

      CALL C2S2GA(LM,KM,JM,IM,SB1,WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* B1 ¡ß ¢ßA/¢ßy  --> WG(JI,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,3)*WG(JI,2)
      END DO

* B1 ¡ß ¢ßA/¢ßy ¤Î¥¹¥Ú¥¯¥È¥ë --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

* ¢ß(B1 ¡ß ¢ßA/¢ßy)/¢ßx --> SC1

      DO L=1,LM
        DO K=-KM,KM
          SC1(K,L)=-K*WS(-K,L)
        END DO
      END DO

* B2 --> WG(JI,4)

      CALL C2S2GA(LM,KM,JM,IM,SB2,WG(1,4),WG,ITJ,TJ,ITI,TI,3)

* B2 ¡ß ¢ßA/¢ßy  --> WG(JI,2)

      DO JI=1,(JM+1)*IM
        WG(JI,2)=WG(JI,4)*WG(JI,2)
      END DO

* B2 ¡ß ¢ßA/¢ßy ¤Î¥¹¥Ú¥¯¥È¥ë --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

* ¢ß(B2 ¡ß ¢ßA/¢ßy)/¢ßx --> SC1

      DO L=1,LM
        DO K=-KM,KM
          SC2(K,L)=-K*WS(-K,L)
        END DO
      END DO

*---------------------
* ¢ßA/¢ßx --> WG(JI,2)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-K*SA(-K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,2),WG,ITJ,TJ,ITI,TI,3)

* B1 ¡ß ¢ßA/¢ßx  --> WG(JI,3)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,2)
      END DO

* B1 ¡ß ¢ßA/¢ßx ¤Î¥¹¥Ú¥¯¥È¥ë --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),WS,WG,ITJ,TJ,ITI,TI,4)

* Finally calculate Jacobian (SC1)

      DO L=1,LM
        DO K=-KM,KM
          SC1(K,L)=-(SC1(K,L)+L*WS(K,L))
        END DO
      END DO

* B2 ¡ß ¢ßA/¢ßx  --> WG(JI,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,4)*WG(JI,2)
      END DO

* B1 ¡ß ¢ßA/¢ßx ¤Î¥¹¥Ú¥¯¥È¥ë --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS,WG,ITJ,TJ,ITI,TI,4)

* Finally calculate Jacobian (SC2)

      DO L=1,LM
        DO K=-KM,KM
          SC2(K,L)=-(SC2(K,L)+L*WS(K,L))
        END DO
      END DO

      END
