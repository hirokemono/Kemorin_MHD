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
SUBROUTINE SYSS2S(MM,NN,SALL,S,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,NN,M,K,NS,LA,L,ICOM
  REAL(8) :: S(*),SALL(*)
  INTEGER(8) :: IPDEST
  REAL(8),DIMENSION(:),ALLOCATABLE :: SBUF(:)
  INTEGER :: IP,NP,IERR,NB,ICOM4

  ICOM4=ICOM
!  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)
!  CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  NB=(MM/NP+1)*(2*(NN+1)-MM/NP*NP)

  IF(IP.EQ.0) THEN
     ALLOCATE(SBUF(1_8*NB*NP))
     SBUF=0     
     SBUF(1:NN+1)=SALL(1:NN+1)
     DO M=1,MM
        K=M/NP
        NS=K*(2*(NN+1)-(K-1)*NP)+1
        CALL SYNM2L(NN,M,M,IPDEST,L,ICOM)
        CALL SXNM2L(NN,M,M,LA)
        SBUF(NS+NB*IPDEST:NS+NB*IPDEST+2*(NN+1-M)-1)=SALL(LA:LA+2*(NN+1-M)-1)
     END DO
  ELSE
     ALLOCATE(SBUF(1))
  END IF

  !  CALL MPI_SCATTER(SBUF,NB,MPI_REAL8,S,NB,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
  CALL MPI_SCATTER(SBUF,NB,MPI_REAL8,S,NB,MPI_REAL8,0,ICOM4,IERR)  

  DEALLOCATE(SBUF)

END SUBROUTINE SYSS2S
