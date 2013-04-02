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
*     TIME-INTEGRATION BY ADAMS METHOD               95/10/13 K.ISHIOIKA
************************************************************************
      SUBROUTINE TDADMU(N,M,H,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,-3:2)
      EXTERNAL SUB

      DX=H/M

      DO J=1,3
        CALL SUB(X,Y,W(1,J-4))
        CALL TDADML(N,DX,X,Y,W(1,J-4),W(1,0),SUB)
      END DO

      DO J=1,M-3
        CALL TDADMM(N,DX,X,Y,W,SUB)
      END DO

      END
************************************************************************
      SUBROUTINE TDADMM(N,DX,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,-3:1)
      EXTERNAL SUB

      CALL SUB(X,Y,W(1,0))

      DO I=1,N
        W(I,-3)=Y(I)+DX*(55*W(I,0)-59*W(I,-1)+37*W(I,-2)-9*W(I,-3))/24
      END DO

      X=X+DX
      CALL SUB(X,W(1,-3),W(1,1))

      DO I=1,N
        Y(I)=Y(I)+DX*(9*W(I,1)+19*W(I,0)-5*W(I,-1)+W(I,-2))/24
      END DO

      DO I=1,N
        W(I,-3)=W(I,-2)
        W(I,-2)=W(I,-1)
        W(I,-1)=W(I,0)
      END DO

      END
************************************************************************
      SUBROUTINE TDADML(N,DX,X,Y,DY,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),DY(N),W(N,3)
      EXTERNAL SUB

      DO I=1,N
        W(I,3)=Y(I)+(DX/2)*DY(I)
      END DO

      X=X+DX/2
      CALL SUB(X,W(1,3),W(1,2))
      DO I=1,N
        W(I,3)=Y(I)+(DX/2)*W(I,2)
        W(I,1)=DY(I)+2*W(I,2)
      END DO

      CALL SUB(X,W(1,3),W(1,2))
      DO I=1,N
        W(I,3)=Y(I)+DX*W(I,2)
        W(I,1)=W(I,1)+2*W(I,2)
      END DO

      X=X+DX/2
      CALL SUB(X,W(1,3),W(1,2))
      DO I=1,N
        Y(I)=Y(I)+(DX/6)*(W(I,1)+W(I,2))
      END DO

      END
