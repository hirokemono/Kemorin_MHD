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
*     CALCULATE JACOBIAN                                      2003/11/05
************************************************************************
      SUBROUTINE P2AJCB(LM,KM,JM,IM,SA,SB,SC,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA(-LM:LM,-KM:KM)
      DIMENSION SB(-LM:LM,-KM:KM)      
      DIMENSION SC(-LM:LM,-KM:KM)
      DIMENSION WS(-LM:LM,-KM:KM)
      DIMENSION WG(JM*IM,3)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

* A --> WG(JI,3)

      CALL P2S2GA(LM,KM,JM,IM,SA,WG(1,3),WG,ITJ,TJ,ITI,TI)

* ¢ßB/¢ßy --> WG(JI,2)

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K)=-L*SB(-L,-K)
        END DO
      END DO

      CALL P2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG,ITJ,TJ,ITI,TI)

* A ¡ß ¢ßB/¢ßy  --> WG(JI,2)

      DO JI=1,JM*IM
        WG(JI,2)=WG(JI,3)*WG(JI,2)
      END DO

* A ¡ß ¢ßB/¢ßy ¤Î¥¹¥Ú¥¯¥È¥ë --> WS

      CALL P2G2SA(LM,KM,JM,IM,WG(1,2),WS,WG,ITJ,TJ,ITI,TI)

* ¢ß(A ¡ß ¢ßB/¢ßy)/¢ßx --> SC      

      DO K=-KM,KM
        DO L=-LM,LM
          SC(L,K)=-K*WS(-L,-K)
        END DO
      END DO

* ¢ßB/¢ßx --> WG(JI,2)

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K)=-K*SB(-L,-K)
        END DO
      END DO

      CALL P2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG,ITJ,TJ,ITI,TI)

* A ¡ß ¢ßB/¢ßx  --> WG(JI,2)

      DO JI=1,JM*IM
        WG(JI,2)=WG(JI,3)*WG(JI,2)
      END DO

* A ¡ß ¢ßB/¢ßx ¤Î¥¹¥Ú¥¯¥È¥ë --> WS

      CALL P2G2SA(LM,KM,JM,IM,WG(1,2),WS,WG,ITJ,TJ,ITI,TI)      

* Finally calculate Jacobian

      DO K=-KM,KM
        DO L=-LM,LM
          SC(L,K)=SC(L,K)+L*WS(-L,-K)
        END DO
      END DO

      END
