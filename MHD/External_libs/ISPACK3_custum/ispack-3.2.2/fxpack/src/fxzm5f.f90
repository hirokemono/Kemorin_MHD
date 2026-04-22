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
!     short DFT (factor 5, forword, j=0) (only taking conjugate)
!-----------------------------------------------------------------------
SUBROUTINE FXZM5F(M,L,X)

  IMPLICIT NONE
  INTEGER(8) :: M,I,L,IV
  REAL(8) :: X(M,2,L/5,0:4)
  REAL(8) :: X0R,X0I,X1R,X1I,X2R,X2I,X3R,X3I,X4R,X4I
  REAL(8),PARAMETER :: C1=0.25D0
  REAL(8),PARAMETER :: C2=0.5590169943749474241D0
  REAL(8),PARAMETER :: C3=0.6180339887498948482D0
  REAL(8),PARAMETER :: C4=-0.9510565162951535721D0

  DO I=1,L/5
     DO IV=1,M
        X0R=X(IV,1,I,1)-X(IV,1,I,4)
        X0I=-X(IV,2,I,1)+X(IV,2,I,4)
        X1R=X(IV,1,I,1)+X(IV,1,I,4)
        X1I=-X(IV,2,I,1)-X(IV,2,I,4)
        X3R=X(IV,1,I,2)-X(IV,1,I,3)
        X3I=-X(IV,2,I,2)+X(IV,2,I,3)
        X4R=X(IV,1,I,2)+X(IV,1,I,3)
        X4I=-X(IV,2,I,2)-X(IV,2,I,3)
        X2R=X0R+C3*X3R
        X2I=X0I+C3*X3I
        X3R=C3*X0R-X3R
        X3I=C3*X0I-X3I
        X0R=X1R+X4R
        X0I=X1I+X4I
        X1R=X1R-X4R
        X1I=X1I-X4I
        X4R=X(IV,1,I,0)-C1*X0R
        X4I=-X(IV,2,I,0)-C1*X0I
        X1R=X4R-C2*X1R
        X1I=X4I-C2*X1I
        X4R=2D0*X4R-X1R
        X4I=2D0*X4I-X1I
        X(IV,1,I,0)=X(IV,1,I,0)+X0R
        X(IV,2,I,0)=-X(IV,2,I,0)+X0I
        X(IV,1,I,3)=X1R-C4*X3I
        X(IV,2,I,3)=X1I+C4*X3R
        X(IV,1,I,2)=2D0*X1R-X(IV,1,I,3)
        X(IV,2,I,2)=2D0*X1I-X(IV,2,I,3)
        X(IV,1,I,4)=X4R-C4*X2I
        X(IV,2,I,4)=X4I+C4*X2R
        X(IV,1,I,1)=2D0*X4R-X(IV,1,I,4)
        X(IV,2,I,1)=2D0*X4I-X(IV,2,I,4)
     END DO
  END DO

END SUBROUTINE FXZM5F
