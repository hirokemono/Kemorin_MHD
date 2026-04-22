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
SUBROUTINE LXOGZP(JR,JM,ID,P,Q,G)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JV=8
  INTEGER(8) :: JR,JM,J,IR,ID,JD,JVD
  REAL(8) :: P(JM/2,2),Q(JV,5,JR)
  REAL(8) :: G(*)

  DO IR=1,JR
     JD=IR+JR*(ID-1)
     JVD=JV*(JD-1)
     DO J=1,JV
        Q(J,5,IR)=(G(J+JM+JVD*2)+G(JV+1-J+JM-JD*JV*2))*P(J+JVD,2)
        Q(J,4,IR)=(G(J+JM+JVD*2)-G(JV+1-J+JM-JD*JV*2))*P(J+JVD,1)*P(J+JVD,2)
     END DO
  END DO

END SUBROUTINE LXOGZP
