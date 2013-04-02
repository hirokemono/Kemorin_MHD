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
*     LOWER ROUTINES FOR SMPACK (DIFFERENCIAL)                  98/01/06
************************************************************************
*     X-DIFFERENCIAL
************************************************************************
      SUBROUTINE SMDX2A(MM,KM,A,B,U,V,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ML((MM+1)*(MM+1))
      DIMENSION A(KM,(MM+1)*(MM+1)),B(KM,(MM+1)*(MM+1))
      DIMENSION U(KM,(MM+1)*(MM+1)),V(KM,(MM+1)*(MM+1))

      LM=(MM+1)*(MM+1)

      DO K=1,KM
        DO L=1,LM
          U(K,L)=-ML(L)*A(K,LM-L+1)
          V(K,L)=-ML(L)*B(K,LM-L+1)
        END DO
      END DO
        
      END
************************************************************************
      SUBROUTINE SMDX1A(MM,KM,A,U,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ML((MM+1)*(MM+1))
      DIMENSION A(KM,(MM+1)*(MM+1))
      DIMENSION U(KM,(MM+1)*(MM+1))

      LM=(MM+1)*(MM+1)

      DO K=1,KM
        DO L=1,LM
          U(K,L)=-ML(L)*A(K,LM-L+1)
        END DO
      END DO
        
      END
************************************************************************
*     Y-DIFFERENCIAL
************************************************************************
      SUBROUTINE SMDY2B(MM,KM,A,B,U,V,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION R(KM*MM,2:MM,2)
      DIMENSION A(KM*(MM+1),MM+1),B(KM*(MM+1),MM+1)
      DIMENSION U(KM*(MM+1),MM+1),V(KM*(MM+1),MM+1)

      DO N=2,MM
        DO I=1,KM*MM
          U(I,N+1)=U(I,N+1)-R(I,N,1)*B(KM+I,N)
          V(I,N+1)=V(I,N+1)+R(I,N,1)*A(KM+I,N)
          U(KM+I,N-1)=U(KM+I,N-1)-R(I,N,2)*B(I,N)
          V(KM+I,N-1)=V(KM+I,N-1)+R(I,N,2)*A(I,N)
        END DO
      END DO
        
      END
************************************************************************
      SUBROUTINE SMDY2F(MM,KM,U,V,A,B,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION R(KM*MM,2:MM,2)
      DIMENSION U(KM*(MM+1),MM+1),V(KM*(MM+1),MM+1)
      DIMENSION A(KM*(MM+1),MM+1),B(KM*(MM+1),MM+1)

      DO N=2,MM
        DO I=1,KM*MM
          A(KM+I,N)=A(KM+I,N)-R(I,N,1)*V(I,N+1)
          B(KM+I,N)=B(KM+I,N)+R(I,N,1)*U(I,N+1)
        END DO
      END DO
        
      DO N=2,MM
        DO I=1,KM*MM
          A(I,N)=A(I,N)-R(I,N,2)*V(KM+I,N-1)
          B(I,N)=B(I,N)+R(I,N,2)*U(KM+I,N-1)
        END DO
      END DO
        
      END
************************************************************************
      SUBROUTINE SMDY1B(MM,KM,A,V,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION R(KM*MM,2:MM,2)
      DIMENSION A(KM*(MM+1),MM+1)
      DIMENSION V(KM*(MM+1),MM+1)

      DO N=2,MM
        DO I=1,KM*MM
          V(I,N+1)=V(I,N+1)+R(I,N,1)*A(KM+I,N)
          V(KM+I,N-1)=V(KM+I,N-1)+R(I,N,2)*A(I,N)
        END DO
      END DO
        
      END
************************************************************************
      SUBROUTINE SMDY1F(MM,KM,V,A,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION R(KM*MM,2:MM,2)
      DIMENSION V(KM*(MM+1),MM+1)
      DIMENSION A(KM*(MM+1),MM+1)

      DO N=2,MM
        DO I=1,KM*MM
          A(KM+I,N)=A(KM+I,N)-R(I,N,1)*V(I,N+1)
        END DO
      END DO
        
      DO N=2,MM
        DO I=1,KM*MM
          A(I,N)=A(I,N)-R(I,N,2)*V(KM+I,N-1)
        END DO
      END DO
        
      END
