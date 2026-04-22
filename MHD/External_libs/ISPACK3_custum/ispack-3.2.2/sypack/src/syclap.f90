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
!***********************************************************************
!     Operate Laplacian
!***********************************************************************
SUBROUTINE SYCLAP(MM,NT,S,SL,D,IFLAG,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,IFLAG,ND,L,NT,ICOM
  REAL(8) :: S(*) ! S((MM/NP+1)*(2*(NT+1)-MM/NP*NP))
  REAL(8) :: SL(*) ! SL((MM/NP+1)*(2*(NT+1)-MM/NP*NP)) 
  REAL(8) :: D(*) ! D((MM/NP+1)*(2*(NT+1)-MM/NP*NP)*2)
  INTEGER :: NP,IP,IERR,ICOM4
  INTEGER(8) :: NTHMAX

  CALL MXGOMP(NTHMAX)  

  ICOM4=ICOM
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  ND=(MM/NP+1)*(2*(NT+1)-MM/NP*NP)

  !$omp parallel do num_threads(NTHMAX)
  DO L=1,ND
     SL(L)=D(L+(IFLAG-1)*ND)*S(L)
  END DO
  !$omp end parallel do

END SUBROUTINE SYCLAP
