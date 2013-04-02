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
*     浅水方程式の保存量のチェック                            2000/02/17
************************************************************************
      SUBROUTINE N2SWCK(LM,KM,JM,IM,AVT,DIV,PHI,AENE,AENS,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-LM:LM,-KM:KM)
      DIMENSION DIV(-LM:LM,-KM:KM)
      DIMENSION PHI(-LM:LM,-KM:KM)
      DIMENSION WS(-LM:LM,-KM:KM,4)
      DIMENSION WG(JM*IM,4)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      CALL N2S2GA(LM,KM,JM,IM,PHI,WG(1,2),WG,ITJ,TJ,ITI,TI)
      CALL N2S2GA(LM,KM,JM,IM,AVT,WG(1,3),WG,ITJ,TJ,ITI,TI)

      AENS=0
      DO JI=1,JM*IM
        AENS=AENS+WG(JI,3)*WG(JI,3)/WG(JI,2)
      END DO
      AENS=AENS/(2*JM*IM)

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K,3)=-(K*K+L*L)
        END DO
      END DO
      WS(0,0,3)=1

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K,1)=DIV(L,K)/WS(L,K,3)
          WS(L,K,2)=AVT(L,K)/WS(L,K,3)
        END DO
      END DO

      DO K=-KM,KM
        DO L=-LM,LM
          WS(L,K,3)=-K*WS(-L,-K,1)+L*WS(-L,-K,2)
          WS(L,K,4)=-L*WS(-L,-K,1)-K*WS(-L,-K,2)
        END DO
      END DO

      CALL N2S2GA(LM,KM,JM,IM,WS(-LM,-KM,3),WG(1,3),WG,ITJ,TJ,ITI,TI)
      CALL N2S2GA(LM,KM,JM,IM,WS(-LM,-KM,4),WG(1,4),WG,ITJ,TJ,ITI,TI)

      AENE=0
      DO JI=1,JM*IM
        AENE=AENE
     &    +WG(JI,2)*(WG(JI,3)*WG(JI,3)+WG(JI,4)*WG(JI,4)+WG(JI,2))
      END DO
      AENE=AENE/(2*JM*IM)

      END
