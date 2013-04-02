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
*     TIME-INTEGRATION BY RUNGE-KUTTA (4) METHOD WITH OPERATER DIVIDING
*                                                    95/10/27 K.ISHIOIKA
************************************************************************
      SUBROUTINE TDRKNU(N,M,H,X,Y,W,SUBL,SUBN)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,3)
      EXTERNAL SUBL,SUBN

      DX=H/M

      DO I=1,M
        CALL TDRKNL(N,DX,X,Y,W,SUBL,SUBN)
      END DO

      END
************************************************************************
      SUBROUTINE TDRKNL(N,DX,X,Y,W,SUBL,SUBN)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(N),W(N,3)
      EXTERNAL SUBL,SUBN

      CALL SUBN(X,Y,W(1,1))

      DO I=1,N
        W(I,2)=Y(I)+(DX/2)*W(I,1)
        W(I,3)=Y(I)+(DX/6)*W(I,1)
      END DO

      CALL SUBL(X,DX/2,Y)
      CALL SUBL(X,DX/2,W(1,2))
      CALL SUBL(X,DX/2,W(1,3))
      CALL SUBN(X+DX/2,W(1,2),W(1,1))

      DO I=1,N
        W(I,2)=Y(I)+(DX/2)*W(I,1)
        W(I,3)=W(I,3)+(DX/3)*W(I,1)
      END DO

      CALL SUBN(X+DX/2,W(1,2),W(1,1))

      DO I=1,N
        W(I,2)=Y(I)+DX*W(I,1)
        W(I,3)=W(I,3)+(DX/3)*W(I,1)
      END DO

      CALL SUBL(X+DX/2,DX/2,W(1,2))
      CALL SUBL(X+DX/2,DX/2,W(1,3))
      CALL SUBN(X+DX,W(1,2),W(1,1))

      X=X+DX
      DO I=1,N
        Y(I)=W(I,3)+(DX/6)*W(I,1)
      END DO

      END
