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
!     そのプロセスが扱う Jの範囲を返す(MPI用).
!     J1: 扱う Jの先頭, J2: 扱う Jの末尾. 
!     そのプロセスで扱う緯度範囲が無い場合は, J1=0, J2=-1 とする.
!-----------------------------------------------------------------------
SUBROUTINE SYQRNJ(JM,JV,J1,J2,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: JM,JV,J1,J2,JI,ICOM
  INTEGER :: NP,IP,IERR,ICOM4

  ICOM4=ICOM
!  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)
!  CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  JI=((JM/JV-1)/NP+1)*JV
  IF(JI*IP.LT.JM) THEN
     J1=1+JI*IP
     J2=MIN(JM,JI*(IP+1))
  ELSE ! そのプロセスでは全く緯度円を担当しない場合
     J1=0
     J2=-1
  END IF

END SUBROUTINE SYQRNJ
