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
!     short DFT (factor 4, forword, j=0) (only taking conjugate)
!-----------------------------------------------------------------------
SUBROUTINE FXZM4F(M,L,X) 

  IMPLICIT NONE
  INTEGER(8) :: M,I,L,IV
  REAL(8) :: X(M,2,L/4,0:3)
  REAL(8) :: X0R,X0I,X1R,X1I,X2R,X2I,X3R,X3I

  DO I=1,L/4
     DO IV=1,M
        X2R=X(IV,1,I,0)-X(IV,1,I,2)
        X2I=-X(IV,2,I,0)+X(IV,2,I,2)
        X0R=X(IV,1,I,0)+X(IV,1,I,2)
        X0I=-X(IV,2,I,0)-X(IV,2,I,2)
        X3R=X(IV,1,I,1)-X(IV,1,I,3)
        X3I=-X(IV,2,I,1)+X(IV,2,I,3)
        X1R=X(IV,1,I,1)+X(IV,1,I,3)
        X1I=-X(IV,2,I,1)-X(IV,2,I,3)
        X(IV,1,I,2)=X0R-X1R
        X(IV,2,I,2)=X0I-X1I
        X(IV,1,I,0)=2D0*X0R-X(IV,1,I,2)
        X(IV,2,I,0)=2D0*X0I-X(IV,2,I,2)       
        X(IV,1,I,1)=X2R-X3I
        X(IV,2,I,1)=X2I+X3R
        X(IV,1,I,3)=2D0*X2R-X(IV,1,I,1)
        X(IV,2,I,3)=2D0*X2I-X(IV,2,I,1)       
     END DO
  END DO

END SUBROUTINE FXZM4F
