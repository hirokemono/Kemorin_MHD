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
!     short DFT (factor 2, backword, j=0)
!-----------------------------------------------------------------------
SUBROUTINE FXZM2B(M,L,X)

  IMPLICIT NONE
  INTEGER(8) :: M,I,L,IV
  REAL(8) :: X(M,2,L/2,0:1)

  DO I=1,L/2                  
     DO IV=1,M
        X(IV,1,I,1)=X(IV,1,I,0)-X(IV,1,I,1)
        X(IV,2,I,1)=X(IV,2,I,0)-X(IV,2,I,1)
        X(IV,1,I,0)=2D0*X(IV,1,I,0)-X(IV,1,I,1)
        X(IV,2,I,0)=2D0*X(IV,2,I,0)-X(IV,2,I,1)               
     END DO
  END DO

END SUBROUTINE FXZM2B
