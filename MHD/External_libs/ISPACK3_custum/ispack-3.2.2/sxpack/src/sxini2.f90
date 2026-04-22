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
SUBROUTINE SXINI2(MM,NM,JM,IG,P,R,JC)

  IMPLICIT NONE
  INTEGER(8) :: MM,NM,JM,IG,M,IE,IJ
  REAL(8) :: P(JM/2,2*MM+5)
  REAL(8) :: R(((MM+1)*(2*NM-MM-1)+1)/4*3+(2*NM-MM)*(MM+1)/2+MM+1)
  INTEGER(8) :: JC(MM*(2*NM-MM-1)/16+MM)
  INTEGER(8) :: NPMAX  

  CALL LXINIG(JM,P,IG)

  CALL MXGOMP(NPMAX)  

!$omp parallel private(IE,IJ) num_threads(NPMAX)
!$omp do schedule(dynamic)
  DO M=1,MM
     IE=(M*(2*NM-M)+1)/4*3+M*(2*NM-M+1)/2+M+1
     IJ=(M-1)*(2*NM-M)/16+M
     CALL LXINIW(NM,JM,M,P,P(1,4+2*M),R(IE),JC(IJ))
  END DO
!$omp end do
!$omp end parallel  
  
END SUBROUTINE SXINI2

