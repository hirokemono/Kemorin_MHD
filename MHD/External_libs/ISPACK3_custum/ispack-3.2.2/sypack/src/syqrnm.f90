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
!     そのプロセスが扱う Mの範囲を返す(MPI用).
!     MCM: そのプロセスが扱う M の個数(扱わない場合は 0)
!     MC: そのプロセスで扱う Mが格納される 配列 MC(1:MCM)
!-----------------------------------------------------------------------
SUBROUTINE SYQRNM(MM,MCM,MC,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,MCM,N1,M1,K,ICOM
  INTEGER(8) :: MC(*) ! MC(MM/NP+1) 
  INTEGER :: NP,IP,IERR,ICOM4

  ICOM4=ICOM
!  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)
!  CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  N1=(MM+1)/NP
  M1=N1*NP
  MCM=N1
  IF(MOD(N1,2).EQ.0) THEN
     IF(IP.LE.MM-M1) THEN
        MCM=MCM+1
     END IF
  ELSE
     IF(IP.GE.NP-MM+M1-1) THEN
        MCM=MCM+1
     END IF
  END IF

  DO K=0,MCM-1
     MC(K+1)=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
  END DO

END SUBROUTINE SYQRNM
