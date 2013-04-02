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
*     TIME-INTEGRATION BY RUNGE-KUTTA (4) METHOD     95/10/13 K.ISHIOIKA
************************************************************************
      SUBROUTINE TDRK4U(N,M,H,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,3)
      EXTERNAL SUB

      DX=H/M

      DO I=1,M
        CALL TDRK4L(N,DX,X,Y,W,SUB)
      END DO

      END
************************************************************************
      SUBROUTINE TDRK4L(N,DX,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,3)
      EXTERNAL SUB

      CALL SUB(X,Y,W(1,1))
      DO I=1,N
        W(I,3)=Y(I)+(DX/2)*W(I,1)
      END DO

      X=X+DX/2
      CALL SUB(X,W(1,3),W(1,2))
      DO I=1,N
        W(I,3)=Y(I)+(DX/2)*W(I,2)
        W(I,1)=W(I,1)+2*W(I,2)
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
