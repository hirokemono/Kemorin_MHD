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
SUBROUTINE LXCSZY(NT,S,SG,C)

  IMPLICIT NONE
  INTEGER(8) :: NT,N
  REAL(8) :: S(0:NT),SG(0:NT+1)
  REAL(8) :: C(2*NT+1)

  IF(NT.GE.1) THEN
     SG(0)=C(NT+1+1)*S(1)
  ELSE
     SG(0)=0
  END IF
  DO N=1,NT-1
     SG(N)=C(N)*S(N-1)+C(NT+1+N+1)*S(N+1)
  END DO
  IF(NT.GE.1) THEN
     SG(NT)=C(NT)*S(NT-1)
  END IF
  SG(NT+1)=C(NT+1)*S(NT)

END SUBROUTINE LXCSZY
