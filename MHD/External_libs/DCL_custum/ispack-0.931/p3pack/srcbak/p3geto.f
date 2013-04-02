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
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2002 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE VORTICITY VECTOR                              2002/03/12
************************************************************************
      SUBROUTINE P3GETO(NM,MM,LM,KM,JM,IM,
     &  Z,O,WS,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)
      DIMENSION WS(-NM:NM,-MM:MM,-LM:LM)
      DIMENSION O(KM*JM*IM,3)
      DIMENSION W(KM*JM*IM)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

* OMEGA1 --> O(*,1)

      DO L=-LM,-1
        DO M=-MM,MM
          DO N=-NM,NM
            WS(N,M,L)=-(M*Z(N,M,L,1)+N*Z(N,M,L,2))/L
          END DO
        END DO
      END DO
      DO L=1,LM
        DO M=-MM,MM
          DO N=-NM,NM
            WS(N,M,L)=-(M*Z(N,M,L,1)+N*Z(N,M,L,2))/L            
          END DO
        END DO
      END DO

*      L=0
      CALL BSCOPY((2*NM+1)*(2*MM+1),Z(-NM,-MM,0,1),WS(-NM,-MM,0))
      WS(0,0,0)=0

      CALL P3S2GA(NM,MM,LM,KM,JM,IM,WS,O(1,1),W,ITK,TK,ITJ,TJ,ITI,TI)

* OMEGA2 --> O(*,2)

      CALL BSCOPY((2*NM+1)*(2*MM+1)*(2*LM+1),Z,WS)
      WS(0,0,0)=0

      L=0
      DO M=-MM,-1
        DO N=-NM,NM
          WS(N,M,L)=-(N*Z(N,M,L,2))/M
        END DO
      END DO
      DO M=1,MM
        DO N=-NM,NM
          WS(N,M,L)=-(N*Z(N,M,L,2))/M          
        END DO
      END DO
      
*      L=0
*      M=0
      CALL BSCOPY(2*NM+1,Z(-NM,0,0,2),WS(-NM,0,0))

*      L=0
*      M=0
*      N=0
      WS(0,0,0)=0      

      CALL P3S2GA(NM,MM,LM,KM,JM,IM,WS,O(1,2),W,ITK,TK,ITJ,TJ,ITI,TI)

* OMEGA3 --> O(*,3)

      CALL BSCOPY((2*NM+1)*(2*MM+1)*(2*LM+1),Z(-NM,-MM,-LM,2),WS)

*      L=0
*      M=0
      CALL BSSET0(2*NM+1,WS(-NM,0,0))

      CALL P3S2GA(NM,MM,LM,KM,JM,IM,WS,O(1,3),W,ITK,TK,ITJ,TJ,ITI,TI)

      END
