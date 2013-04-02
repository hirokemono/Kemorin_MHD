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
*     LOWER ROUTINES FOR SMPACK (REARRANGEMENT)                 98/01/06
************************************************************************
      SUBROUTINE SMRGGB(IM,ID,JD,KM,X,Y)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(JD*KM,IM),Y(ID,JD*KM)

      DO I=1,IM
        DO JK=1,JD*KM
          Y(I,JK)=X(JK,I)
        END DO
      END DO

      DO I=IM+1,ID
        DO JK=1,JD*KM
          Y(I,JK)=Y(IM,JK)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SMRGGF(IM,ID,JD,KM,Y,X)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(ID,JD*KM),X(JD*KM,IM)

      DO I=1,IM
        DO JK=1,JD*KM
          X(JK,I)=Y(I,JK)
        END DO
      END DO

      END
