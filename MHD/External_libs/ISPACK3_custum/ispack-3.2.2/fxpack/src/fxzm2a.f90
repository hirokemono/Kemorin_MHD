!***********************************************************************
! ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
! Copyright (C) 1998--2024 Keiichi Ishioka <ishioka@gfd-dennou.org>
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
! 02110-1301 USA.
!***********************************************************************
!-----------------------------------------------------------------------
!     short DFT (factor 2, backword)
!-----------------------------------------------------------------------
SUBROUTINE FXZM2A(M,K,L,X,T)

  IMPLICIT NONE
  INTEGER(8) :: M,I,J,K,L,IV
  REAL(8) :: X(M,2,L/2,0:1,0:K-1),T(2,0:*)
  REAL(8) :: X1R,X1I

  DO J=0,K-1
     DO I=1,L/2                  
        DO IV=1,M
           X1R=X(IV,1,I,0,J)-T(1,J)*X(IV,1,I,1,J)
           X1I=X(IV,2,I,0,J)-T(2,J)*X(IV,1,I,1,J)
           X(IV,1,I,1,J)=X1R+T(2,J)*X(IV,2,I,1,J)
           X(IV,2,I,1,J)=X1I-T(1,J)*X(IV,2,I,1,J)
           X(IV,1,I,0,J)=2D0*X(IV,1,I,0,J)-X(IV,1,I,1,J)
           X(IV,2,I,0,J)=2D0*X(IV,2,I,0,J)-X(IV,2,I,1,J)               
        END DO
     END DO
  END DO

END SUBROUTINE FXZM2A
