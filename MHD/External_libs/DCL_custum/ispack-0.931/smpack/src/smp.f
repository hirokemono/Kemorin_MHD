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
*     LOWER ROUTINES FOR SMPACK (PARITY TRANSFORM)              98/02/16
************************************************************************
      SUBROUTINE SMPGWB(MM,ID,JM,JD,KM,G,W,IP,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(JD,KM*ID),G(KM*ID,JD)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION Q(JM/2)

      JH=JM/2

      DO J=1,JH
        DO I=1,KM*(2*MM+1)
          W(JH-J+1,I)=      (G(I,J)+G(I,JH+J))*Q(J)
          W(JH+J,  I)=IP(I)*(G(I,J)-G(I,JH+J))*Q(J)
        END DO
      END DO

      DO J=JM+1,JD
        DO I=1,KM*(2*MM+1)
          W(J,I)=W(JM,I)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SMPWGF(MM,ID,JM,JD,KM,W,G,IP,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(JD,KM*ID)
      DIMENSION G(KM*ID,JD)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION Q(JM/2)

      JH=JM/2

      DO J=1,JH
        DO I=1,KM*(2*MM+1)
          G(I,   J)=(W(JH-J+1,I)+IP(I)*W(JH+J,I))*Q(J)
          G(I,JH+J)=(W(JH-J+1,I)-IP(I)*W(JH+J,I))*Q(J)
        END DO
      END DO

      END
