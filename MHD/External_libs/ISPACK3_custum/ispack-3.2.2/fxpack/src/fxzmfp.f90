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
! permutation for forward FFT
!-----------------------------------------------------------------------
SUBROUTINE FXZMFP(M,N,X,IT)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: IMM=-9223372036854775808_8  
  INTEGER(8) :: M,N,IT(N),ISJ,J,I,ISJ2
  REAL(8) :: X(M,2,0:N-1),F
  REAL(8),DIMENSION(:,:),ALLOCATABLE :: Y  

  F=1D0/N

  ALLOCATE(Y(M,2))  

  DO I=1,M
     X(I,1,0)= F*X(I,1,0)
     X(I,2,0)=-F*X(I,2,0)
  END DO
  DO I=1,M
     X(I,1,N-1)= F*X(I,1,N-1)
     X(I,2,N-1)=-F*X(I,2,N-1)
  END DO

  J=1
  DO WHILE(J.LE.N-2)
     ISJ=IT(J)
     IF(ISJ.LT.0) THEN
        DO I=1,M        
           X(I,1,ISJ-IMM)= F*X(I,1,ISJ-IMM)
           X(I,2,ISJ-IMM)=-F*X(I,2,ISJ-IMM)           
        END DO
        J=J+1
        CYCLE
     END IF
     DO I=1,M
        Y(I,1)=X(I,1,ISJ)
        Y(I,2)=X(I,2,ISJ)        
     END DO
     DO
        J=J+1        
        ISJ2=IT(J)
        IF(ISJ2.LT.0) THEN
           DO I=1,M
              X(I,1,ISJ)= F*X(I,1,ISJ2-IMM)
              X(I,2,ISJ)=-F*X(I,2,ISJ2-IMM)                            
           END DO
           DO I=1,M
              X(I,1,ISJ2-IMM)= F*Y(I,1)
              X(I,2,ISJ2-IMM)=-F*Y(I,2)                            
           END DO
           J=J+1
           EXIT
        ELSE
           DO I=1,M
              X(I,1,ISJ)= F*X(I,1,ISJ2)
              X(I,2,ISJ)=-F*X(I,2,ISJ2)              
           END DO
           ISJ=ISJ2
        END IF
     END DO
  END DO
  
END SUBROUTINE FXZMFP
