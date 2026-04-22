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
!     short DFT (factor 4, backword)
!-----------------------------------------------------------------------
SUBROUTINE FXZM4A(M,K,L,X,T) 

  IMPLICIT NONE
  INTEGER(8) :: M,I,J,K,L,IV,IJ
  REAL(8) :: X(M,2,L/4,0:3,0:K-1),T(2,0:*)
  REAL(8) :: X0R,X0I,X1R,X1I,X2R,X2I,X3R,X3I
  REAL(8) :: T1R,T1I,T2R,T2I
  
  IJ=0
  DO J=0,K-1
     T1R=T(1,IJ)
     T1I=T(2,IJ)            
     T2R=T(1,IJ+1)
     T2I=T(2,IJ+1)            
     DO I=1,L/4         
        DO IV=1,M
           X2R=(X(IV,1,I,0,J)-T2R*X(IV,1,I,2,J))+T2I*X(IV,2,I,2,J)
           X2I=(X(IV,2,I,0,J)-T2I*X(IV,1,I,2,J))-T2R*X(IV,2,I,2,J)
           X0R=2D0*X(IV,1,I,0,J)-X2R
           X0I=2D0*X(IV,2,I,0,J)-X2I               
           X3R=(X(IV,1,I,1,J)-T2R*X(IV,1,I,3,J))+T2I*X(IV,2,I,3,J)
           X3I=(X(IV,2,I,1,J)-T2I*X(IV,1,I,3,J))-T2R*X(IV,2,I,3,J)
           X1R=2D0*X(IV,1,I,1,J)-X3R
           X1I=2D0*X(IV,2,I,1,J)-X3I               
           X(IV,1,I,2,J)=(X0R-T1R*X1R)+T1I*X1I
           X(IV,2,I,2,J)=(X0I-T1I*X1R)-T1R*X1I
           X(IV,1,I,0,J)=2D0*X0R-X(IV,1,I,2,J)
           X(IV,2,I,0,J)=2D0*X0I-X(IV,2,I,2,J)       
           X(IV,1,I,1,J)=(X2R-T1R*X3I)-T1I*X3R
           X(IV,2,I,1,J)=(X2I+T1R*X3R)-T1I*X3I
           X(IV,1,I,3,J)=2D0*X2R-X(IV,1,I,1,J)
           X(IV,2,I,3,J)=2D0*X2I-X(IV,2,I,1,J)       
        END DO
     END DO
     IJ=IJ+3
  END DO

END SUBROUTINE FXZM4A
