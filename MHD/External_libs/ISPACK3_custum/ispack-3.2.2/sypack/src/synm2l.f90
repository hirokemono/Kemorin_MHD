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
SUBROUTINE SYNM2L(NN,N,M,IP,L,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: NN,N,M,L,K,MA,IP,ICOM
  INTEGER :: NP,IERR,ICOM4

  ICOM4=ICOM
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)

  MA=ABS(M)
  K=MA/NP
  IF(MOD(K,2).EQ.0) THEN
     IP=MA-K*NP
  ELSE
     IP=(K+1)*NP-MA-1
  END IF

  IF(M.EQ.0) THEN
     L=N+1
  ELSE IF(M.GT.0) THEN
     L=K*(2*(NN+1)-(K-1)*NP)+1+2*(N-M)
  ELSE
     L=K*(2*(NN+1)-(K-1)*NP)+1+2*(N+M)+1
  END IF

END SUBROUTINE SYNM2L
