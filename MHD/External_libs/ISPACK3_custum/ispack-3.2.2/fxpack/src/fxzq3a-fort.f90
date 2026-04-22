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
!     short DFT (factor 3, backword)
!-----------------------------------------------------------------------
SUBROUTINE FXZQ3A(K,L,X,T) 

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: M=4  
  INTEGER(8) :: I,J,K,L,IV,IJ
  REAL(8) :: X(M,2,L/3,0:2,0:K-1),T(2,0:*)
  REAL(8) :: X0R,X0I,X1R,X1I,X2R,X2I
  REAL(8) :: T1R,T1I,T2R,T2I  
  REAL(8),PARAMETER :: C1=-0.5D0
  REAL(8),PARAMETER :: C2=0.86602540378443864676D0

  IJ=0
  DO J=0,K-1
     T1R=T(1,IJ)
     T1I=T(2,IJ)            
     T2R=T(1,IJ+1)
     T2I=T(2,IJ+1)            
     DO I=1,L/3       
        DO IV=1,M
           X0R=T1R*X(IV,1,I,1,J)-T1I*X(IV,2,I,1,J)
           X0I=T1R*X(IV,2,I,1,J)+T1I*X(IV,1,I,1,J)
           X1R=X0R-T2R*X(IV,1,I,2,J)+T2I*X(IV,2,I,2,J)
           X1I=X0I-T2R*X(IV,2,I,2,J)-T2I*X(IV,1,I,2,J)
           X0R=2D0*X0R-X1R
           X0I=2D0*X0I-X1I
           X2R=X(IV,1,I,0,J)+C1*X0R
           X2I=X(IV,2,I,0,J)+C1*X0I
           X(IV,1,I,0,J)=X0R+X(IV,1,I,0,J)
           X(IV,2,I,0,J)=X0I+X(IV,2,I,0,J)
           X(IV,1,I,2,J)=X2R+C2*X1I
           X(IV,2,I,2,J)=X2I-C2*X1R
           X(IV,1,I,1,J)=2D0*X2R-X(IV,1,I,2,J)
           X(IV,2,I,1,J)=2D0*X2I-X(IV,2,I,2,J)
        END DO
     END DO
     IJ=IJ+2
  END DO

END SUBROUTINE FXZQ3A
