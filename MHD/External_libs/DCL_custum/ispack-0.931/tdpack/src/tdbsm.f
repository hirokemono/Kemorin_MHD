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
*     TIME-INTEGRATION BY BULIRSCH-STOER METHOD      95/10/13 K.ISHIOIKA
************************************************************************
      SUBROUTINE TDBSMU(N,M,L,H,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N*(L+3))
      EXTERNAL SUB

      DX=H/M
      DO 10 I=1,M
        CALL TDBSMM(N,L,DX,X,Y,W,SUB)
   10 CONTINUE

      END
************************************************************************
      SUBROUTINE TDBSMM(N,L,H,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,-2:L)
      EXTERNAL SUB

      CALL SUB(X,Y,W)

      DO K=1,L
        M=2*K
        DX=H/M
        DK=DX*DX
        CALL TDBSML(N,M,DX,X,Y,W(1,K),W,SUB)
        DO J=K-1,1,-1
          DJ=(H/(2*J))*(H/(2*J))
          DO I=1,N
            W(I,J)=(W(I,J)*DK-W(I,J+1)*DJ)/(DK-DJ)
          END DO
        END DO
      END DO

      X=X+H
      DO I=1,N
        Y(I)=W(I,1)
      END DO

      END
************************************************************************
      SUBROUTINE TDBSML(N,M,DX,X,Y,Z,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),Z(N),W(N,0:2)
      EXTERNAL SUB

      XD=X

      DO I=1,N
        W(I,1)=Y(I)+DX*W(I,0)
      END DO
      XD=XD+DX

      CALL SUB(XD,W(1,1),W(1,2))
      DO I=1,N
        Z(I)=Y(I)+(2*DX)*W(I,2)
      END DO
      XD=XD+DX

      DO J=3,M-1,2
        CALL SUB(XD,Z,W(1,2))
        DO I=1,N
          W(I,1)=W(I,1)+(2*DX)*W(I,2)
        END DO
        XD=XD+DX
        CALL SUB(XD,W(1,1),W(1,2))
        DO I=1,N
          Z(I)=Z(I)+(2*DX)*W(I,2)
        END DO
        XD=XD+DX
      END DO

      END

