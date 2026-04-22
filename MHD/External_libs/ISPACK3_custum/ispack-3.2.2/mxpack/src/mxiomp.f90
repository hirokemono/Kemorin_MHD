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
SUBROUTINE MXIOMP

  IMPLICIT NONE
  LOGICAL,SAVE :: LFIRST = .TRUE.
  INTEGER(8),SAVE :: NTHS, NTHMAX
  INTEGER(8) :: NTH
  !$    INTEGER :: omp_get_max_threads

  !--------------
  ENTRY MXGOMP(NTH)

    IF(LFIRST) THEN
       NTHMAX=1
       !$    NTHMAX=omp_get_max_threads()
       NTHS=NTHMAX
       LFIRST = .FALSE.
    END IF

    NTH=NTHS

  RETURN
  !--------------
  ENTRY MXSOMP(NTH)

    IF(LFIRST) THEN
       NTHMAX=1
       !$    NTHMAX=omp_get_max_threads()
       NTHS=NTHMAX
       LFIRST = .FALSE.
    END IF

    IF(NTH.LE.NTHMAX) THEN
       NTHS=NTH
    ELSE
       NTHS=NTHMAX
    END IF

  RETURN
  !--------------
END SUBROUTINE MXIOMP
