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
SUBROUTINE LXTSWV(NM,NN,JM,M,S1,S2,G1,G2,P,PM,R,JC,IPOW)

  USE ISO_C_BINDING  
  IMPLICIT NONE
  INTEGER(8) :: NM,NN,JM,M,JV,JR,IPOW
  INTEGER(8) :: JC(*)  
  REAL(8) ::  S1(2*(NN-M+1)),S2(2*(NN-M+1)),G1(2,JM),G2(2,JM)
  REAL(8) :: P(JM/2,5),PM(JM/2,2),R(*)
  TYPE(C_PTR) :: WP,WSP
  REAL(8),DIMENSION(:),POINTER:: WORK,WS
  
  CALL LXGPRM(JM,JV,JR)  

  CALL MXALLC(WP,JV*11*JR)
  CALL C_F_POINTER(WP,WORK,[JV*11*JR])
  CALL MXALLC(WSP,4*(NN-M+1))
  CALL C_F_POINTER(WSP,WS,[4*(NN-M+1)])
  CALL LXSSWV(NM,NN,JM,JV,JR,M,S1,S2,G1,G2,P,PM,R,JC,WORK,WS,IPOW,1_8)  

  CALL MXFREE(WP)
  CALL MXFREE(WSP)        
  
END SUBROUTINE LXTSWV
