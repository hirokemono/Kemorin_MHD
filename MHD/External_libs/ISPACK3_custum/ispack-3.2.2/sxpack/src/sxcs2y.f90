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
SUBROUTINE SXCS2Y(MM,NT,S,SG,C)

  IMPLICIT NONE
  INTEGER(8) :: MM,NT,M,NS,NSG,NC
  REAL(8) :: S(NT+1+MM*(2*NT-MM+1)),SG(NT+2+MM*(2*NT-MM+3))  
  REAL(8) :: C((2*NT-MM+1)*(MM+1))  
  INTEGER(8) :: NPMAX

  CALL MXGOMP(NPMAX)  

  !$omp parallel private(NS,NSG,NC) num_threads(NPMAX)
  !$omp do schedule(dynamic)
  DO M=0,MM
     IF(M.EQ.0) THEN
        CALL LXCSZY(NT,S,SG,C)              
     ELSE
        NS=NT+1+(NT+(NT-(M-2)))*(M-1)+1
        NSG=NT+2+(NT+1+(NT+1-(M-2)))*(M-1)+1
        NC=(2*NT-M+2)*M+1
        CALL LXCSWY(NT,M,S(NS),SG(NSG),C(NC))
     END IF
  END DO
  !$omp end do
  !$omp end parallel

END SUBROUTINE SXCS2Y
