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
! permutation for backward FFT
!-----------------------------------------------------------------------
SUBROUTINE FXZOBP(N,X,IT)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: M=8
  INTEGER(8),PARAMETER :: IMM=-9223372036854775808_8  
  INTEGER(8) :: N,IT(N),ISJ,J,I,ISJ2
  REAL(8) :: X(M*2,0:N-1),Y(M*2)

  J=1
  DO WHILE(J.LE.N-2)
     ISJ=IT(J)
     IF(ISJ.LT.0) THEN
        ! このときはそのまま
        J=J+1
        CYCLE
     END IF
     DO I=1,M*2
        Y(I)=X(I,ISJ)
     END DO
     DO
        J=J+1        
        ISJ2=IT(J)
        IF(ISJ2.LT.0) THEN
           DO I=1,M*2
              X(I,ISJ)=X(I,ISJ2-IMM)              
           END DO
           DO I=1,M*2
              X(I,ISJ2-IMM)=Y(I)              
           END DO
           J=J+1
           EXIT
        ELSE
           DO I=1,M*2
              X(I,ISJ)=X(I,ISJ2)
           END DO
           ISJ=ISJ2
        END IF
     END DO
  END DO
  
END SUBROUTINE FXZOBP
