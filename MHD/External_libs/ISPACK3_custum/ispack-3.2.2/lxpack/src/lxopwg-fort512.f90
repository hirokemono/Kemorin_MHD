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
SUBROUTINE LXOPWG(JR,JM,ID,P,Q,G)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JV=8
  INTEGER(8) :: JR,JM,J,IR,ID,JD,JVD
  REAL(8) :: P(JM/2),Q(JV,7,JR)
  REAL(8) :: G(*),GQ1,GQ2  

  DO IR=1,JR
     DO J=1,JV
        GQ1=Q(J,4,IR)*P(J+JV*(IR-1)+JV*JR*(ID-1))
        GQ2=Q(J,6,IR)*P(J+JV*(IR-1)+JV*JR*(ID-1))
        G(J   +2*JV*(IR-1)  +JM+2*JV*JR*(ID-1))= GQ1+Q(J,5,IR)
        G(J+JV+2*JV*(IR-1)  +JM+2*JV*JR*(ID-1))= GQ2+Q(J,7,IR)
        G(JV+1-J   -2*JV*IR +JM-2*JV*JR*(ID-1))=-GQ1+Q(J,5,IR)
        G(JV+1-J+JV-2*JV*IR +JM-2*JV*JR*(ID-1))=-GQ2+Q(J,7,IR)
     END DO
  END DO

END SUBROUTINE LXOPWG
