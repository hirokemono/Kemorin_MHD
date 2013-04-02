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
*     浅水方程式の保存量のチェック                            2009/05/25
************************************************************************
      SUBROUTINE PZSWCK(LM,KM,JM,IM,R,AVT,DIV,PHI,AENE,AENS,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-LM:LM,-KM:KM)
      DIMENSION DIV(-LM:LM,-KM:KM)
      DIMENSION PHI(-LM:LM,-KM:KM)
      DIMENSION WS(-LM:LM,-KM:KM)
      DIMENSION WG(JM*IM,4)
      DIMENSION ITJ(4),TJ(JM*6),ITI(4),TI(IM*8)      

      CALL PZS2GA(LM,KM,JM,IM,PHI,WG(1,2),WG,ITJ,TJ,ITI,TI)
      CALL PZS2GA(LM,KM,JM,IM,AVT,WG(1,3),WG,ITJ,TJ,ITI,TI)

      AENS=0
      DO JI=1,JM*IM
        AENS=AENS+WG(JI,3)*WG(JI,3)/WG(JI,2)
      END DO
      AENS=AENS/(2*JM*IM)

      DO K=-KM,-1
        DO L=-LM,LM
          WS(L,K)=-(-R*K*DIV(-L,-K)+L*AVT(-L,-K))/(R*K*R*K+L*L)
        END DO
      END DO
      DO K=1,KM
        DO L=-LM,LM
          WS(L,K)=-(-R*K*DIV(-L,-K)+L*AVT(-L,-K))/(R*K*R*K+L*L)
        END DO
      END DO
      K=0
      DO L=-LM,-1
        WS(L,K)=-(L*AVT(-L,-K))/(R*K*R*K+L*L)
      END DO
      DO L=1,LM
        WS(L,K)=-(L*AVT(-L,-K))/(R*K*R*K+L*L)
      END DO
      WS(0,0)=0      

      CALL PZS2GA(LM,KM,JM,IM,WS,WG(1,3),WG,ITJ,TJ,ITI,TI)

      DO K=-KM,-1
        DO L=-LM,LM
          WS(L,K)=-(-L*DIV(-L,-K)-R*K*AVT(-L,-K))/(R*K*R*K+L*L)
        END DO
      END DO
      DO K=1,KM
        DO L=-LM,LM
          WS(L,K)=-(-L*DIV(-L,-K)-R*K*AVT(-L,-K))/(R*K*R*K+L*L)
        END DO
      END DO
      K=0
      DO L=-LM,-1
        WS(L,K)=-(-L*DIV(-L,-K))/(R*K*R*K+L*L)
      END DO
      DO L=1,LM
        WS(L,K)=-(-L*DIV(-L,-K))/(R*K*R*K+L*L)
      END DO
      WS(0,0)=0
      
      CALL PZS2GA(LM,KM,JM,IM,WS,WG(1,4),WG,ITJ,TJ,ITI,TI)

      AENE=0
      DO JI=1,JM*IM
        AENE=AENE
     &    +WG(JI,2)*(WG(JI,3)*WG(JI,3)+WG(JI,4)*WG(JI,4)+WG(JI,2))
      END DO
      AENE=AENE/(2*JM*IM)

      END
