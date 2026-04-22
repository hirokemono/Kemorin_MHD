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
SUBROUTINE SXLTB1(JV,JR2,MM,IM,A,B)

  INTEGER(8) :: JV,JR2,MM,IM,M,J
  REAL(8) ::  A(*),B(JV*2,0:*)

  DO M=0,MM
     DO J=1,JV*2
        B(J,M)=A(J+M*JV*2*JR2)
     END DO
  END DO

  DO M=MM+1,IM/2-1
     DO J=1,JV*2
        B(J,M)=0
     END DO
  END DO

END SUBROUTINE SXLTB1
