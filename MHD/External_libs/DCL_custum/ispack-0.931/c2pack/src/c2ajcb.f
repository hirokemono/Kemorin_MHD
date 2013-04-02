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
*     CALCULATE JACOBIAN                                      2000/10/02
************************************************************************
      SUBROUTINE C2AJCB(LM,KM,JM,IM,SA,SB,SC,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA(-KM:KM,LM)
      DIMENSION SB(-KM:KM,LM)
      DIMENSION SC(-KM:KM,LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,3)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* A --> WG(JI,3)

      CALL C2S2GA(LM,KM,JM,IM,SA,WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* ¢ßB/¢ßy --> WG(JI,2)

      CALL BSSET0(2*KM+1,WS)
      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=L*SB(K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* A ¡ß ¢ßB/¢ßy  --> WG(JI,2)

      DO JI=1,(JM+1)*IM
        WG(JI,2)=WG(JI,3)*WG(JI,2)
      END DO

* A ¡ß ¢ßB/¢ßy ¤Î¥¹¥Ú¥¯¥È¥ë --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

* ¢ß(A ¡ß ¢ßB/¢ßy)/¢ßx --> SC      

      DO L=1,LM
        DO K=-KM,KM
          SC(K,L)=-K*WS(-K,L)
        END DO
      END DO
      
* ¢ßB/¢ßx --> WG(JI,2)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-K*SB(-K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,2),WG,ITJ,TJ,ITI,TI,3)

* A ¡ß ¢ßB/¢ßx  --> WG(JI,2)

      DO JI=1,(JM+1)*IM
        WG(JI,2)=WG(JI,3)*WG(JI,2)
      END DO

* A ¡ß ¢ßB/¢ßx ¤Î¥¹¥Ú¥¯¥È¥ë --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS,WG,ITJ,TJ,ITI,TI,4)

* Finally calculate Jacobian

      DO L=1,LM
        DO K=-KM,KM
          SC(K,L)=SC(K,L)+L*WS(K,L)
        END DO
      END DO

      END
