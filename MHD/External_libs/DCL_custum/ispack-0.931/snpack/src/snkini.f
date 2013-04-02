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
*     DUPLICATE COEFFICIENTS                                    99/02/22
************************************************************************
      SUBROUTINE SNKINI(MM,JM,KM,IP,P,R,IPK,PK,RK)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IPK(KM*((MM+1)/2+MM+1)*2)
      DIMENSION PK(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION RK(KM*((MM+1)/2*2+3)*(MM/2+1))

      CALL SNKCPR(((MM+1)/2+MM+1)*JM,KM,P,PK)
      CALL SNKCPR(((MM+1)/2*2+3)*(MM/2+1),KM,R,RK)
      CALL SNKCPI(((MM+1)/2+MM+1)*2,KM,IP,IPK)

      END
************************************************************************
      SUBROUTINE SNKCPR(N,K,A,AK)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),AK(K,N)

      DO J=1,K
        DO I=1,N
          AK(J,I)=A(I)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNKCPI(N,K,IA,IAK)

      DIMENSION IA(N),IAK(K,N)

      DO J=1,K
        DO I=1,N
          IAK(J,I)=IA(I)
        END DO
      END DO

      END
