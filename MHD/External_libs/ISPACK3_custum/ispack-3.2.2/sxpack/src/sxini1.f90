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
SUBROUTINE SXINI1(MM,NM,IM,IT,T,R)

  IMPLICIT NONE
  INTEGER(8) :: MM,NM,IM,M,IE
  INTEGER(8) :: IT(IM/2)  
  REAL(8) :: T(IM*3/2)
  REAL(8) :: R(((MM+1)*(2*NM-MM-1)+1)/4*3+(2*NM-MM)*(MM+1)/2+MM+1)
  INTEGER(8) :: NPMAX  

  CALL FXRINI(IM,IT,T)    
  CALL LXINIR(NM,0_8,R)

  CALL MXGOMP(NPMAX)  

!$omp parallel private(IE) num_threads(NPMAX)
!$omp do schedule(dynamic) 
  DO M=1,MM
     IE=(M*(2*NM-M)+1)/4*3+M*(2*NM-M+1)/2+M+1
     CALL LXINIR(NM,M,R(IE))       
  END DO
!$omp end do
!$omp end parallel  

END SUBROUTINE SXINI1
