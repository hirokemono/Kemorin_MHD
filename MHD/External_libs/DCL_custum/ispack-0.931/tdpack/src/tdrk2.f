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
*     TIME-INTEGRATION BY RUNGE-KUTTA (2) METHOD     95/10/25 K.ISHIOIKA
************************************************************************
      SUBROUTINE TDRK2U(N,M,H,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,2)
      EXTERNAL SUB

      DX=H/M

      DO I=1,M
        CALL TDRK2L(N,DX,X,Y,W,SUB)
      END DO

      END
************************************************************************
      SUBROUTINE TDRK2L(N,DX,X,Y,W,SUB)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,2)
      EXTERNAL SUB

      CALL SUB(X,Y,W(1,1))
      DO I=1,N
        W(I,2)=Y(I)+(DX/2)*W(I,1)
        Y(I)=Y(I)+DX*W(I,1)
      END DO

      X=X+DX
      CALL SUB(X,Y,W(1,1))
      DO I=1,N
        Y(I)=W(I,2)+(DX/2)*W(I,1)
      END DO

      END
