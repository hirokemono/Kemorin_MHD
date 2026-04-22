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
SUBROUTINE LXLPWV(JV,JR,JM,ID,P,Q,G1,G2)

  IMPLICIT NONE
  INTEGER(8) :: JV,JR,JM,J,IR,ID,JD,JVD
  REAL(8) :: P(JM/2),Q(JV,11,JR)
  REAL(8) :: G1(*),G2(*),GQ1,GQ2  

  DO IR=1,JR
     JD=IR+JR*(ID-1)
     JVD=JV*(JD-1)
     DO J=1,JV
        GQ1=Q(J,4,IR)*P(J+JVD)
        GQ2=Q(J,6,IR)*P(J+JVD)
        G1(J   +JM+JVD*2)= GQ1+Q(J,5,IR)
        G1(J+JV+JM+JVD*2)= GQ2+Q(J,7,IR)
        G1(JV+1-J   +JM-JD*JV*2)=-GQ1+Q(J,5,IR)
        G1(JV+1-J+JV+JM-JD*JV*2)=-GQ2+Q(J,7,IR)
        GQ1=Q(J,8,IR)*P(J+JVD)
        GQ2=Q(J,10,IR)*P(J+JVD)
        G2(J   +JM+JVD*2)= GQ1+Q(J,9,IR)
        G2(J+JV+JM+JVD*2)= GQ2+Q(J,11,IR)
        G2(JV+1-J   +JM-JD*JV*2)=-GQ1+Q(J,9,IR)
        G2(JV+1-J+JV+JM-JD*JV*2)=-GQ2+Q(J,11,IR)
     END DO
  END DO

END SUBROUTINE LXLPWV
