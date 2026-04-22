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
SUBROUTINE LXTSZG(NM,NN,JM,S,G,P,R,IPOW)

  USE ISO_C_BINDING  
  IMPLICIT NONE
  INTEGER(8) :: NM,NN,JM,JV,JR,IPOW
  REAL(8) ::  S(NN+1),G(JM),P(JM/2,5),R(*)
  TYPE(C_PTR) :: WP,WSP
  REAL(8),DIMENSION(:),POINTER:: WORK,WS
  
  CALL LXGPRM(JM,JV,JR)  

  CALL MXALLC(WP,JV*5*JR)
  CALL C_F_POINTER(WP,WORK,[JV*5*JR])
  CALL MXALLC(WSP,NN+1)
  CALL C_F_POINTER(WSP,WS,[NN+1])
  CALL LXSSZG(NM,NN,JM,JV,JR,S,G,P,R,WORK,WS,IPOW,1_8)

  CALL MXFREE(WP)
  CALL MXFREE(WSP)        
  
END SUBROUTINE LXTSZG
