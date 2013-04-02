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
*     CALCULATE JACOBIAN                                        95/10/03
************************************************************************
      SUBROUTINE STAJBA(MM,JM,IM,A,B,C,P,Q,R,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1)),B((MM+1)*(MM+1))
      DIMENSION C((MM+1)*(MM+1))
      DIMENSION P(JM*IM,4)
      DIMENSION Q(JM*(MM+1)),R((MM+1)*(MM+1)),IT(5),T(IM*2)

      CALL STBJBA(MM,JM,IM,A,B,P(1,1),P(1,2),P(1,3),P(1,4),Q,R,IT,T)

      DO IJ=1,IM*JM
        P(IJ,1)=P(IJ,1)*P(IJ,3)
        P(IJ,2)=P(IJ,2)*P(IJ,3)
      END DO

      CALL STV2SA(MM,JM,IM,P(1,1),P(1,2),C,P(1,3),Q,R,IT,T)

      END
************************************************************************
      SUBROUTINE STBJBA(MM,JM,IM,A,B,U,V,G,P,Q,R,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1)),B((MM+1)*(MM+1))
      DIMENSION U(JM*2,0:IM/2-1),V(JM*2,0:IM/2-1),G(JM*2,0:IM/2-1)
      DIMENSION P(JM*IM)
      DIMENSION Q(JM*(MM+1)),R((MM+1)*(MM+1)),IT(5),T(IM*2)

      CALL LTBJBZ(MM,JM,A,B,U,G,P,Q,R)

      CALL BSSET0(JM,G(JM+1,0))
      CALL BSSET0(JM,U(JM+1,0))
      CALL BSSET0(JM*2,V)

      DO M=1,MM
        CALL LTLMML(MM,M,L)
        CALL LTBJBW(MM,JM,M,A(L),B(L),U(1,M),V(1,M),G(1,M),P,Q,R)
      END DO

      IF(MM.LT.IM/2-1) THEN
        CALL BSSET0(JM*2*(IM/2-1-MM),U(1,MM+1))
        CALL BSSET0(JM*2*(IM/2-1-MM),V(1,MM+1))
        CALL BSSET0(JM*2*(IM/2-1-MM),G(1,MM+1))
      END IF

      CALL FTTRUB(JM,IM,U,P,IT,T)
      CALL FTTRUB(JM,IM,V,P,IT,T)
      CALL FTTRUB(JM,IM,G,P,IT,T)

      END
************************************************************************
      SUBROUTINE LTBJBZ(MM,JM,A,B,U,G,P,Q,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(0:MM),B(0:MM)
      DIMENSION U(JM/2,2),G(JM/2,2)
      DIMENSION P(JM/2,2),Q(JM/2,2,0:MM),R(0:MM,0:MM)

      JH=JM/2
      M=0

      N=M
      DO J=1,JH
        P(J,1)=1
        P(J,2)=0

        G(J,1)=B(N)
        U(J,2)=0

        G(J,2)=0
        U(J,1)=0
      END DO

      DO N=M+1,MM-1,2
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          G(J,2)=G(J,2)+B(N)*P(J,2)

          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          U(J,1)=U(J,1)-A(N)*P(J,1)

          P(J,1)=R(N+1,M)*(Q(J,1,N+1)*P(J,2)-P(J,1))
          G(J,1)=G(J,1)+B(N+1)*P(J,1)

          P(J,2)=R(M,N+1)*P(J,2)-Q(J,1,N+1)*P(J,1)
          U(J,2)=U(J,2)-A(N+1)*P(J,2)
        END DO
      END DO

      IF(MOD(MM-M,2).EQ.1) THEN
        N=MM
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          G(J,2)=G(J,2)+B(N)*P(J,2)

          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          U(J,1)=U(J,1)-A(N)*P(J,1)
        END DO
      END IF

      CALL LTLBGZ(JM,G,P)
      CALL LTLBVZ(JM,U,P,Q)

      END
************************************************************************
      SUBROUTINE LTBJBW(MM,JM,M,A,B,U,V,G,P,Q,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M:MM,2),B(M:MM,2)
      DIMENSION U(JM/2,2,2),V(JM/2,2,2),G(JM/2,2,2)
      DIMENSION P(JM/2,2),Q(JM/2,2,0:MM),R(0:MM,0:MM)

      JH=JM/2

      N=M
      DO J=1,JH
        P(J,1)=Q(J,2,M)
        G(J,1,1)= B(N,1)  *P(J,1)
        G(J,1,2)= B(N,2)  *P(J,1)
        V(J,1,1)=-(A(N,2)*M)*P(J,1)
        V(J,1,2)= (A(N,1)*M)*P(J,1)

        P(J,2)=-P(J,1)*Q(J,1,M)
        U(J,2,1)=-A(N,1)*P(J,2)
        U(J,2,2)=-A(N,2)*P(J,2)

        G(J,2,1)=0
        G(J,2,2)=0
        V(J,2,1)=0
        V(J,2,2)=0
        U(J,1,1)=0
        U(J,1,2)=0
      END DO

      DO N=M+1,MM-1,2
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          G(J,2,1)=G(J,2,1)   +B(N,1) *P(J,2)
          G(J,2,2)=G(J,2,2)   +B(N,2) *P(J,2)
          V(J,2,1)=V(J,2,1)-(M*A(N,2))*P(J,2)
          V(J,2,2)=V(J,2,2)+(M*A(N,1))*P(J,2)

          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          U(J,1,1)=U(J,1,1)-A(N,1)*P(J,1)
          U(J,1,2)=U(J,1,2)-A(N,2)*P(J,1)

          P(J,1)=R(N+1,M)*(Q(J,1,N+1)*P(J,2)-P(J,1))
          G(J,1,1)=G(J,1,1)   +B(N+1,1) *P(J,1)
          G(J,1,2)=G(J,1,2)   +B(N+1,2) *P(J,1)
          V(J,1,1)=V(J,1,1)-(M*A(N+1,2))*P(J,1)
          V(J,1,2)=V(J,1,2)+(M*A(N+1,1))*P(J,1)

          P(J,2)=R(M,N+1)*P(J,2)-Q(J,1,N+1)*P(J,1)
          U(J,2,1)=U(J,2,1)-A(N+1,1)*P(J,2)
          U(J,2,2)=U(J,2,2)-A(N+1,2)*P(J,2)
        END DO
      END DO

      IF(MOD(MM-M,2).EQ.1) THEN
        N=MM
        DO J=1,JH
          P(J,2)=R(N,M)*(Q(J,1,N)*P(J,1)-P(J,2))
          G(J,2,1)=G(J,2,1)   +B(N,1) *P(J,2)
          G(J,2,2)=G(J,2,2)   +B(N,2) *P(J,2)
          V(J,2,1)=V(J,2,1)-(M*A(N,2))*P(J,2)
          V(J,2,2)=V(J,2,2)+(M*A(N,1))*P(J,2)

          P(J,1)=R(M,N)*P(J,1)-Q(J,1,N)*P(J,2)
          U(J,1,1)=U(J,1,1)-A(N,1)*P(J,1)
          U(J,1,2)=U(J,1,2)-A(N,2)*P(J,1)
        END DO
      END IF

      CALL LTLBGW(JM,G,P)
      CALL LTLBVW(JM,U,P,Q)
      CALL LTLBVW(JM,V,P,Q)

      END
