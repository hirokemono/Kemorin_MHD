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
!     short DFT (factor 5, backword)
!-----------------------------------------------------------------------
SUBROUTINE FXZM5A(M,K,L,X,T) 

  IMPLICIT NONE
  INTEGER(8) :: M,I,J,K,L,IV,IJ
  REAL(8) :: X(M,2,L/5,0:4,0:K-1),T(2,0:*)
  REAL(8) :: X0R,X0I,X1R,X1I,X2R,X2I,X3R,X3I,X4R,X4I
  REAL(8) :: T1R,T1I,T2R,T2I,T3R,T3I,T4R,T4I
  REAL(8),PARAMETER :: C1=0.25D0
  REAL(8),PARAMETER :: C2=0.5590169943749474241D0
  REAL(8),PARAMETER :: C3=0.6180339887498948482D0
  REAL(8),PARAMETER :: C4=-0.9510565162951535721D0
    
  IJ=0
  DO J=0,K-1
     T1R=T(1,IJ)
     T1I=T(2,IJ)            
     T2R=T(1,IJ+1)
     T2I=T(2,IJ+1)            
     T3R=T(1,IJ+2)
     T3I=T(2,IJ+2)            
     T4R=T(1,IJ+3)
     T4I=T(2,IJ+3)
     DO I=1,L/5
        DO IV=1,M
           X1R=T1R*X(IV,1,I,1,J)-T1I*X(IV,2,I,1,J)
           X1I=T1R*X(IV,2,I,1,J)+T1I*X(IV,1,I,1,J)
           X2R=T2R*X(IV,1,I,2,J)-T2I*X(IV,2,I,2,J)
           X2I=T2R*X(IV,2,I,2,J)+T2I*X(IV,1,I,2,J)
           X0R=X1R-T4R*X(IV,1,I,4,J)+T4I*X(IV,2,I,4,J)
           X0I=X1I-T4R*X(IV,2,I,4,J)-T4I*X(IV,1,I,4,J)
           X1R=2D0*X1R-X0R
           X1I=2D0*X1I-X0I
           X3R=X2R-T3R*X(IV,1,I,3,J)+T3I*X(IV,2,I,3,J)
           X3I=X2I-T3R*X(IV,2,I,3,J)-T3I*X(IV,1,I,3,J)
           X4R=2D0*X2R-X3R
           X4I=2D0*X2I-X3I

           X2R=X0R+C3*X3R
           X2I=X0I+C3*X3I
           X3R=C3*X0R-X3R
           X3I=C3*X0I-X3I

           X0R=X1R+X4R
           X0I=X1I+X4I
           X1R=X1R-X4R
           X1I=X1I-X4I
           X4R=X(IV,1,I,0,J)-C1*X0R
           X4I=X(IV,2,I,0,J)-C1*X0I
           X1R=X4R-C2*X1R
           X1I=X4I-C2*X1I
           X4R=2D0*X4R-X1R
           X4I=2D0*X4I-X1I
           X(IV,1,I,0,J)=X(IV,1,I,0,J)+X0R
           X(IV,2,I,0,J)=X(IV,2,I,0,J)+X0I
           X(IV,1,I,3,J)=X1R-C4*X3I
           X(IV,2,I,3,J)=X1I+C4*X3R
           X(IV,1,I,2,J)=2D0*X1R-X(IV,1,I,3,J)
           X(IV,2,I,2,J)=2D0*X1I-X(IV,2,I,3,J)
           X(IV,1,I,4,J)=X4R-C4*X2I
           X(IV,2,I,4,J)=X4I+C4*X2R
           X(IV,1,I,1,J)=2D0*X4R-X(IV,1,I,4,J)
           X(IV,2,I,1,J)=2D0*X4I-X(IV,2,I,4,J)
        END DO
     END DO
     IJ=IJ+4
  END DO

END SUBROUTINE FXZM5A
