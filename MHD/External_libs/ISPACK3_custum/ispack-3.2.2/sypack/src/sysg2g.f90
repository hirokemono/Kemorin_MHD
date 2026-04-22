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
SUBROUTINE SYSG2G(IM,JM,JV,GALL,G,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: IM,JM,JV,JI,I,J,K,ICOM
  REAL(8) ::  G(*),GALL(*)
  REAL(8),ALLOCATABLE :: GBUF(:)
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

  IF(IP.EQ.0) THEN
     JI=((JM/JV-1)/NP+1)*JV
     DO J=1,JM
        K=(J-1)/JI
        DO I=0,IM-1
           GBUF(1+I+IM*(J-K*JI-1)+K*NB)=GALL(I+IM*(J-1)+1)
        END DO
     END DO
  END IF

  !  CALL MPI_SCATTER(GBUF,NB,MPI_REAL8,G,NB,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
  CALL MPI_SCATTER(GBUF,NB,MPI_REAL8,G,NB,MPI_REAL8,0,ICOM4,IERR)  

  DEALLOCATE(GBUF)    

END SUBROUTINE SYSG2G
