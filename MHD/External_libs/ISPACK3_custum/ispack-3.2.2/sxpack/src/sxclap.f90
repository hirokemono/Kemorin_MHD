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
SUBROUTINE SXCLAP(MM,NT,S,SL,D,IFLAG)

  IMPLICIT NONE
  INTEGER(8) :: MM,NT,L,IFLAG
  REAL(8) :: S(NT+1+MM*(2*NT-MM+1)),SL(NT+1+MM*(2*NT-MM+1))
  REAL(8) :: D(NT+1+MM*(2*NT-MM+1),2)
  INTEGER(8) :: NPMAX

  CALL MXGOMP(NPMAX)  

  !$omp parallel do num_threads(NPMAX)
  DO L=1,NT+1+MM*(2*NT-MM+1)
     SL(L)=D(L,IFLAG)*S(L)
  END DO
  !$omp end parallel do

END SUBROUTINE SXCLAP
