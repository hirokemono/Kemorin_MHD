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
*     CALCULATE DIVERGENCE(V*R)                                 95/10/03
************************************************************************
      SUBROUTINE STVRSA(MM,JM,IM,U,V,S,P,Q,R,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(JM*2,0:IM/2-1),V(JM*2,0:IM/2-1)
      DIMENSION S((MM+1)*(MM+1))
      DIMENSION P(JM*IM)
      DIMENSION Q(JM*(MM+1)),R((MM+1)*(MM+1)),IT(5),T(IM*2)

      CALL FTTRUF(JM,IM,U,P,IT,T)
      CALL FTTRUF(JM,IM,V,P,IT,T)

      CALL LTVRSZ(MM,JM,U,S,P,Q,R)

      DO M=1,MM
        CALL LTLMML(MM,M,L)
        CALL LTVRSW(MM,JM,M,U(1,M),V(1,M),S(L),P,Q,R)
      END DO

      END
************************************************************************
      SUBROUTINE LTVRSZ(MM,JM,U,S,P,Q,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(JM/2,2)
      DIMENSION S(0:MM)
      DIMENSION P(JM/2,2),Q(JM/2,2,0:MM),R(0:MM,0:MM)

      JH=JM/2
      M=0

      CALL LTLFVZ(JM,U,P,Q)
      CALL BSSET0(MM+1,S)

      N=M
      DO J=1,JH
        P(J,1)=Q(J,1,0)
        P(J,2)=0
      END DO

      DO N=M+1,MM-1,2
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          S(N)=S(N)+U(J,1)*P(J,1)

          P(J,1)=R(N+1,M)*(Q(J,1,N+1)*P(J,2)-P(J,1))
          P(J,2)=R(M,N+1)*P(J,2)-Q(J,1,N+1)*P(J,1)
          S(N+1)=S(N+1)+U(J,2)*P(J,2)
        END DO
      END DO

      IF(MOD(MM-M,2).EQ.1) THEN
        N=MM
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          S(N)=S(N)+U(J,1)*P(J,1)
        END DO
      END IF

      END
************************************************************************
      SUBROUTINE LTVRSW(MM,JM,M,U,V,S,P,Q,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(JM/2,2,2),V(JM/2,2,2)
      DIMENSION S(M:MM,2)
      DIMENSION P(JM/2,2),Q(JM/2,2,0:MM),R(0:MM,0:MM)

      JH=JM/2

      CALL LTLFVW(JM,U,P,Q)
      CALL LTLFVW(JM,V,P,Q)
      CALL BSSET0((MM-M+1)*2,S)

      N=M
      DO J=1,JH
        P(J,1)=Q(J,2,M)*Q(J,1,0)
        P(J,2)=-P(J,1)*Q(J,1,M)
        S(N,1)=S(N,1)-V(J,1,2)*(M*P(J,1))+U(J,2,1)*P(J,2)
        S(N,2)=S(N,2)+V(J,1,1)*(M*P(J,1))+U(J,2,2)*P(J,2)
      END DO

      DO N=M+1,MM-1,2
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          S(N,1)=S(N,1)-V(J,2,2)*(M*P(J,2))+U(J,1,1)*P(J,1)
          S(N,2)=S(N,2)+V(J,2,1)*(M*P(J,2))+U(J,1,2)*P(J,1)

          P(J,1)=R(N+1,M)*(Q(J,1,N+1)*P(J,2)-P(J,1))
          P(J,2)=R(M,N+1)*P(J,2)-Q(J,1,N+1)*P(J,1)
          S(N+1,1)=S(N+1,1)-V(J,1,2)*(M*P(J,1))+U(J,2,1)*P(J,2)
          S(N+1,2)=S(N+1,2)+V(J,1,1)*(M*P(J,1))+U(J,2,2)*P(J,2)
        END DO
      END DO

      IF(MOD(MM-M,2).EQ.1) THEN
        N=MM
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          S(N,1)=S(N,1)-V(J,2,2)*(M*P(J,2))+U(J,1,1)*P(J,1)
          S(N,2)=S(N,2)+V(J,2,1)*(M*P(J,2))+U(J,1,2)*P(J,1)
        END DO
      END IF

      END
