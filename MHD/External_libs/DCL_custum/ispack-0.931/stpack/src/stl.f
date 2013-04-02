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
*     LOWER SUBROUTNES OF ST(LT)PACK                            95/10/03
************************************************************************
*     CALCULATE THE POSITION OF A SPECTRUM COEFFICIENT OF P_M^M (M>0)
************************************************************************
      SUBROUTINE LTLMML(MM,M,L)

      L=M*(2*MM-M+3)-MM

      END
************************************************************************
*     DIVIDE BY SYMMETRY AND ITS REVERSE
************************************************************************
*     FOR BACKWARD TRANSFORMATION
************************************************************************
      SUBROUTINE LTLBGZ(JM,G,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(JM/2,2),P(JM/2)

      JH=JM/2

      DO J=1,JH
        P(J)  =G(J,1)-G(J,2)
        G(J,2)=G(J,1)+G(J,2)
      END DO

      DO J=1,JH
        G(JH+1-J,1)=P(J)
      END DO

      END
************************************************************************
      SUBROUTINE LTLBGW(JM,G,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(JM/2,2,2),P(JM/2,2)

      JH=JM/2

      DO J=1,JH
        P(J,1)  =G(J,1,1)-G(J,2,1)
        G(J,2,1)=G(J,1,1)+G(J,2,1)
        P(J,2)  =G(J,1,2)-G(J,2,2)
        G(J,2,2)=G(J,1,2)+G(J,2,2)
      END DO

      DO J=1,JH
        G(JH+1-J,1,1)=P(J,1)
        G(JH+1-J,1,2)=P(J,2)
      END DO

      END
************************************************************************
      SUBROUTINE LTLBVZ(JM,V,P,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V(JM/2,2),P(JM/2),Q(JM/2,2)

      JH=JM/2

      DO J=1,JH
        P(J)  =(V(J,1)-V(J,2))*Q(J,2)
        V(J,2)=(V(J,1)+V(J,2))*Q(J,2)
      END DO

      DO J=1,JH
        V(JH+1-J,1)=P(J)
      END DO

      END
************************************************************************
      SUBROUTINE LTLBVW(JM,V,P,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V(JM/2,2,2),P(JM/2,2),Q(JM/2,2)

      JH=JM/2

      DO J=1,JH
        P(J,1)  =(V(J,1,1)-V(J,2,1))*Q(J,2)
        V(J,2,1)=(V(J,1,1)+V(J,2,1))*Q(J,2)
        P(J,2)  =(V(J,1,2)-V(J,2,2))*Q(J,2)
        V(J,2,2)=(V(J,1,2)+V(J,2,2))*Q(J,2)
      END DO

      DO J=1,JH
        V(JH+1-J,1,1)=P(J,1)
        V(JH+1-J,1,2)=P(J,2)
      END DO

      END
************************************************************************
*     FOR FORWARD TRANSFORMATION
************************************************************************
      SUBROUTINE LTLFGZ(JM,G,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(JM/2,2),P(JM/2)

      JH=JM/2

      DO J=1,JH
        P(J)=G(JH+1-J,1)
      END DO

      DO J=1,JH
        G(J,1)=G(J,2)+P(J)
        G(J,2)=G(J,2)-P(J)
      END DO

      END
************************************************************************
      SUBROUTINE LTLFGW(JM,G,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(JM/2,2,2),P(JM/2,2)

      JH=JM/2

      DO J=1,JH
        P(J,1)=G(JH+1-J,1,1)
        P(J,2)=G(JH+1-J,1,2)
      END DO

      DO J=1,JH
        G(J,1,1)=G(J,2,1)+P(J,1)
        G(J,2,1)=G(J,2,1)-P(J,1)
        G(J,1,2)=G(J,2,2)+P(J,2)
        G(J,2,2)=G(J,2,2)-P(J,2)
      END DO

      END
************************************************************************
      SUBROUTINE LTLFVZ(JM,V,P,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V(JM/2,2),P(JM/2),Q(JM/2,2)

      JH=JM/2

      DO J=1,JH
        P(J)=V(JH+1-J,1)
      END DO

      DO J=1,JH
        V(J,1)=(V(J,2)+P(J))*Q(J,2)
        V(J,2)=(V(J,2)-P(J))*Q(J,2)
      END DO

      END
************************************************************************
      SUBROUTINE LTLFVW(JM,V,P,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V(JM/2,2,2),P(JM/2,2),Q(JM/2,2)

      JH=JM/2

      DO J=1,JH
        P(J,1)=V(JH+1-J,1,1)
        P(J,2)=V(JH+1-J,1,2)
      END DO

      DO J=1,JH
        V(J,1,1)=(V(J,2,1)+P(J,1))*Q(J,2)
        V(J,2,1)=(V(J,2,1)-P(J,1))*Q(J,2)
        V(J,1,2)=(V(J,2,2)+P(J,2))*Q(J,2)
        V(J,2,2)=(V(J,2,2)-P(J,2))*Q(J,2)
      END DO

      END
