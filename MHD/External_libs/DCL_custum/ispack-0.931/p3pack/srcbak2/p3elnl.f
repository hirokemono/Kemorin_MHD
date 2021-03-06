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
*     CALCULATE NONLINEAR TERM FOR 3D EULER EQ.               2002/03/07
************************************************************************
      SUBROUTINE P3ELNL(NM,MM,LM,KM,JM,IM,
     &  Z,DZ,WS,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)
      DIMENSION DZ(-NM:NM,-MM:MM,-LM:LM,2)      
      DIMENSION WS(-NM:NM,-MM:MM,-LM:LM,3)
      DIMENSION W(KM*JM*IM,5)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      IJKM=IM*JM*KM      

* U --> W(*,2)

      CALL P3GETU(NM,MM,LM,Z,WS,1)
      CALL P3S2GA(NM,MM,LM,KM,JM,IM,WS,W(1,2),W,ITK,TK,ITJ,TJ,ITI,TI)

* V --> W(*,3)

      CALL P3GETU(NM,MM,LM,Z,WS,2)
      CALL P3S2GA(NM,MM,LM,KM,JM,IM,WS,W(1,3),W,ITK,TK,ITJ,TJ,ITI,TI)

* W --> W(*,4)

      CALL P3GETU(NM,MM,LM,Z,WS,3)
      CALL P3S2GA(NM,MM,LM,KM,JM,IM,WS,W(1,4),W,ITK,TK,ITJ,TJ,ITI,TI)

* UV --> W(*,5) --> WS(*,1)
      
      DO IJK=1,IJKM
        W(IJK,5)=W(IJK,2)*W(IJK,3)
      END DO

      CALL P3G2SA(NM,MM,LM,KM,JM,IM,W(1,5),WS,
     &  W,ITK,TK,ITJ,TJ,ITI,TI)

* VW --> W(*,5) --> WS(*,2)
      
      DO IJK=1,IJKM
        W(IJK,5)=W(IJK,3)*W(IJK,4)
      END DO

      CALL P3G2SA(NM,MM,LM,KM,JM,IM,W(1,5),WS(-NM,-MM,-LM,2),
     &  W,ITK,TK,ITJ,TJ,ITI,TI)

* WU --> W(*,5) --> WS(*,3)
      
      DO IJK=1,IJKM
        W(IJK,5)=W(IJK,4)*W(IJK,2)
      END DO

      CALL P3G2SA(NM,MM,LM,KM,JM,IM,W(1,5),WS(-NM,-MM,-LM,3),
     &  W,ITK,TK,ITJ,TJ,ITI,TI)

* U*U-W*W--> W(*,5) --> DZ(*,1)
      
      DO IJK=1,IJKM
        W(IJK,5)=W(IJK,2)*W(IJK,2)-W(IJK,4)*W(IJK,4)
      END DO

      CALL P3G2SA(NM,MM,LM,KM,JM,IM,W(1,5),DZ(-NM,-MM,-LM,1),
     &  W,ITK,TK,ITJ,TJ,ITI,TI)

* V*V-U*U--> W(*,5) --> DZ(*,2)
      
      DO IJK=1,IJKM
        W(IJK,5)=W(IJK,3)*W(IJK,3)-W(IJK,2)*W(IJK,2)
      END DO

      CALL P3G2SA(NM,MM,LM,KM,JM,IM,W(1,5),DZ(-NM,-MM,-LM,2),
     &  W,ITK,TK,ITJ,TJ,ITI,TI)

* COMPUTING DZ
      
      DO L=-LM,-1
        DO M=-MM,MM
          DO N=-NM,NM
            DZ(N,M,L,1)=L*N*DZ(N,M,L,1)+(N*N-L*L)*WS(N,M,L,3)
     &        +M*(N*WS(N,M,L,1)-L*WS(N,M,L,2))
            DZ(N,M,L,2)=L*M*DZ(N,M,L,2)+(L*L-M*M)*WS(N,M,L,1)
     &        +N*(L*WS(N,M,L,2)-M*WS(N,M,L,3))
          END DO
        END DO
      END DO
      DO L=1,LM
        DO M=-MM,MM
          DO N=-NM,NM
            DZ(N,M,L,1)=L*N*DZ(N,M,L,1)+(N*N-L*L)*WS(N,M,L,3)
     &        +M*(N*WS(N,M,L,1)-L*WS(N,M,L,2))
            DZ(N,M,L,2)=L*M*DZ(N,M,L,2)+(L*L-M*M)*WS(N,M,L,1)
     &        +N*(L*WS(N,M,L,2)-M*WS(N,M,L,3))
          END DO
        END DO
      END DO

      L=0
      DO M=-MM,-1
        DO N=-NM,NM
          DZ(N,M,L,1)=-M*N*(DZ(N,M,L,1)+DZ(N,M,L,2))
     &      +(M*M-N*N)*WS(N,M,L,2)
          DZ(N,M,L,2)=-M*(M*WS(N,M,L,1)+N*WS(N,M,L,3))
        END DO
      END DO
      DO M=1,MM
        DO N=-NM,NM
          DZ(N,M,L,1)=-M*N*(DZ(N,M,L,1)+DZ(N,M,L,2))
     &      +(M*M-N*N)*WS(N,M,L,2)
          DZ(N,M,L,2)=-M*(M*WS(N,M,L,1)+N*WS(N,M,L,3))
        END DO
      END DO
      
      L=0
      M=0
      DO N=-NM,-1
        DZ(N,M,L,1)=-N*N*WS(N,M,L,2)
        DZ(N,M,L,2)= N*N*WS(N,M,L,3)
      END DO
      DO N=1,NM
        DZ(N,M,L,1)=-N*N*WS(N,M,L,2)
        DZ(N,M,L,2)= N*N*WS(N,M,L,3)
      END DO

      L=0
      M=0
      N=0
      DZ(N,M,L,1)=0
      DZ(N,M,L,2)=0

      END
