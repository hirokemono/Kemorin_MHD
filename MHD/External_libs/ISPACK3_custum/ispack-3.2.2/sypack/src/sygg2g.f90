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
SUBROUTINE SYGG2G(IM,JM,JV,G,GALL,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: IM,JM,JV,JI,I,J,K,ICOM
  REAL(8) ::  G(*),GALL(*)
  REAL(8),DIMENSION(:),ALLOCATABLE :: GBUF(:)
  INTEGER :: IP,NP,IERR,NB,ICOM4

  ICOM4=ICOM
!  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)
!  CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  NB=IM*JV*((JM/JV-1)/NP+1)

  IF(IP.EQ.0) THEN
     ALLOCATE(GBUF(1_8*NB*NP))
  ELSE
     ALLOCATE(GBUF(1))
  END IF

  !  CALL MPI_GATHER(G,NB,MPI_REAL8,GBUF,NB,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
  CALL MPI_GATHER(G,NB,MPI_REAL8,GBUF,NB,MPI_REAL8,0,ICOM4,IERR)  

  IF(IP.EQ.0) THEN
     JI=((JM/JV-1)/NP+1)*JV
     DO J=1,JM
        K=(J-1)/JI
        DO I=0,IM-1
           GALL(I+IM*(J-1)+1)=GBUF(1+I+IM*(J-K*JI-1)+K*NB)
        END DO
     END DO
  END IF

  DEALLOCATE(GBUF)  

END SUBROUTINE SYGG2G
