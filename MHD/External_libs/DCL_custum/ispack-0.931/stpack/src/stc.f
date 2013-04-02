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
*     SUBROUTINES FOR CONVERTING                                95/10/03
************************************************************************
*     OPERATE LAPLACIAN
************************************************************************
      SUBROUTINE STCLFA(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION B((MM+1)*(MM+1))

      DO N=0,MM
        B(N+1)=-N*(N+1)
      END DO

      CALL STCLLA(MM,A,B)

      END
************************************************************************
      SUBROUTINE LTCLFZ(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(0:MM)
      DIMENSION B(0:MM)

      DO N=0,MM
        B(N)=-N*(N+1)*A(N)
      END DO

      END
************************************************************************
      SUBROUTINE LTCLFW(MM,M,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M:MM,2)
      DIMENSION B(M:MM,2)

      DO N=M,MM
        B(N,1)=-N*(N+1)*A(N,1)
        B(N,2)=-N*(N+1)*A(N,2)
      END DO

      END
************************************************************************
*     OPERATE INVERSE LAPLACIAN
************************************************************************
      SUBROUTINE STCLBA(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION B((MM+1)*(MM+1))

      B(1)=1
      DO N=1,MM
        B(N+1)=-1D0/(N*(N+1))
      END DO

      CALL STCLLA(MM,A,B)

      END
************************************************************************
      SUBROUTINE LTCLBZ(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(0:MM)
      DIMENSION B(0:MM)

      B(0)=A(0)
      DO N=1,MM
        B(N)=-A(N)/(N*(N+1))
      END DO

      END
************************************************************************
      SUBROUTINE LTCLBW(MM,M,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M:MM,2)
      DIMENSION B(M:MM,2)

      DO N=M,MM
        B(N,1)=-A(N,1)/(N*(N+1))
        B(N,2)=-A(N,2)/(N*(N+1))
      END DO

      END
************************************************************************
*     LOWER ROUTINES
************************************************************************
      SUBROUTINE STCLLA(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION B((MM+1)*(MM+1))

      DO M=1,MM
        CALL LTLMML(MM,M,L)
        CALL STCLLW(MM,M,A(L),B(L),B)
      END DO

      CALL STCLLZ(MM,A,B)

      END
************************************************************************
      SUBROUTINE STCLLW(MM,M,A,B,W)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M:MM,2)
      DIMENSION B(M:MM,2)
      DIMENSION W(0:MM)

      DO N=M,MM
        B(N,1)=W(N)*A(N,1)
        B(N,2)=W(N)*A(N,2)
      END DO

      END
************************************************************************
      SUBROUTINE STCLLZ(MM,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(0:MM)
      DIMENSION B(0:MM)

      DO N=0,MM
        B(N)=B(N)*A(N)
      END DO

      END
