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
!     short DFT (factor 3, forword, j=0) (only taking conjugate)
!-----------------------------------------------------------------------
SUBROUTINE FXZQ3F(L,X)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: M=4  
  INTEGER(8) :: I,L,IV
  REAL(8) :: X(M,2,L/3,0:2)
  REAL(8) :: X0R,X0I,X1R,X1I,X2R,X2I
  REAL(8),PARAMETER :: C1=-0.5D0
  REAL(8),PARAMETER :: C2=0.86602540378443864676D0

  DO I=1,L/3       
     DO IV=1,M
        X1R=X(IV,1,I,1)-X(IV,1,I,2)
        X1I=-X(IV,2,I,1)+X(IV,2,I,2)
        X0R=X(IV,1,I,1)+X(IV,1,I,2)
        X0I=-X(IV,2,I,1)-X(IV,2,I,2)
        X2R=X(IV,1,I,0)+C1*X0R
        X2I=-X(IV,2,I,0)+C1*X0I
        X(IV,1,I,0)=X0R+X(IV,1,I,0)
        X(IV,2,I,0)=X0I-X(IV,2,I,0)
        X(IV,1,I,2)=X2R+C2*X1I
        X(IV,2,I,2)=X2I-C2*X1R
        X(IV,1,I,1)=2D0*X2R-X(IV,1,I,2)
        X(IV,2,I,1)=2D0*X2I-X(IV,2,I,2)
     END DO
  END DO

END SUBROUTINE FXZQ3F
