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
SUBROUTINE SXTS2V(MM,NM,NN,IM,JM,S1,S2,G1,G2,IT,T,P,R,JC,W,IPOW)

  USE ISO_C_BINDING  
  IMPLICIT NONE
  INTEGER(8) :: MM,NM,NN,IM,JM,JV,JR,M,IE,IJ,JD,NS,IPOW
  REAL(8) ::  S1((2*NN+1-MM)*MM+NN+1),G1(JM*IM)
  REAL(8) ::  S2((2*NN+1-MM)*MM+NN+1),G2(JM*IM)  
  REAL(8) :: T(IM*3/2),P(JM/2,2*MM+5),R(*)
  INTEGER(8) :: IT(IM/2),JC(*)
  REAL(8) :: W(JM*IM,2)
  INTEGER(8) :: NP  
  TYPE(C_PTR) :: WP
  REAL(8),DIMENSION(:),POINTER:: WORK
  TYPE(C_PTR) :: WSP
  REAL(8),DIMENSION(:),POINTER:: WS(:)

  CALL SXGPRM(JM,JV,JR)
  IF(MOD(JR,2).EQ.0) JR=JR/2

  CALL MXGOMP(NP)

!$omp parallel private(NS,WP,WSP,IJ,IE,WORK,WS) num_threads(NP)
  CALL MXALLC(WP,JV*11*JR)
  CALL C_F_POINTER(WP,WORK,[JV*11*JR])
  CALL MXALLC(WSP,4*(NM+1))
  CALL C_F_POINTER(WSP,WS,[4*(NM+1)])
  !$omp do schedule(dynamic)
  DO M=0,MM
     IF(M.EQ.0) THEN
        CALL LXSSZV(NM,NN,JM,JV,JR,S1,S2,W(1,1),W(1,2),P,R,WORK,WS,IPOW,0_8)
     ELSE
        NS=1+NN+1+(M-1)*(NN+NN+2-M)
        IE=(M*(2*NM-M)+1)/4*3+M*(2*NM-M+1)/2+M+1
        IJ=(M-1)*(2*NM-M)/16+M
        CALL LXSSWV(NM,NN,JM,JV,JR,M,S1(NS),S2(NS), &
             & W(1+M*2*JM,1),W(1+M*2*JM,2), P,P(1,4+M*2), &
             & R(IE),JC(IJ),WORK,WS,IPOW,0_8)
     END IF
  END DO
  !$omp end do
  CALL MXFREE(WP)
  CALL MXFREE(WSP)
!$omp end parallel

!$omp parallel private(WP,WORK) num_threads(NP)
  CALL MXALLC(WP,JV*IM)
  CALL C_F_POINTER(WP,WORK,[JV*IM])
  !$omp do schedule(dynamic)
  DO JD=1,JM/JV
     IF(JV.EQ.4) THEN     
        CALL SXQTB1(JM/JV,MM,IM,W(1+2*JV*(JD-1),1),WORK)
     ELSE IF(JV.EQ.8) THEN     
        CALL SXOTB1(JM/JV,MM,IM,W(1+2*JV*(JD-1),1),WORK)
     ELSE
        CALL SXLTB1(JV,JM/JV,MM,IM,W(1+2*JV*(JD-1),1),WORK)
     END IF
     CALL FXRTBA(JV,IM,WORK,IT,T)
     IF(JV.EQ.4.AND.MOD(IM,4).EQ.0) THEN     
        CALL SXQTB2(IM,WORK,G1(1+JV*IM*(JD-1)))
     ELSE IF(JV.EQ.8.AND.MOD(IM,8).EQ.0) THEN     
        CALL SXOTB2(IM,WORK,G1(1+JV*IM*(JD-1)))
     ELSE
        CALL SXLTB2(JV,IM,WORK,G1(1+JV*IM*(JD-1)))
     END IF
  END DO
  !$omp end do
  CALL MXFREE(WP)    
!$omp end parallel

!$omp parallel private(WP,WORK) num_threads(NP)
  CALL MXALLC(WP,JV*IM)
  CALL C_F_POINTER(WP,WORK,[JV*IM])
  !$omp do schedule(dynamic)
  DO JD=1,JM/JV
     IF(JV.EQ.4) THEN     
        CALL SXQTB1(JM/JV,MM,IM,W(1+2*JV*(JD-1),2),WORK)
     ELSE IF(JV.EQ.8) THEN     
        CALL SXOTB1(JM/JV,MM,IM,W(1+2*JV*(JD-1),2),WORK)
     ELSE
        CALL SXLTB1(JV,JM/JV,MM,IM,W(1+2*JV*(JD-1),2),WORK)
     END IF
     CALL FXRTBA(JV,IM,WORK,IT,T)
     IF(JV.EQ.4.AND.MOD(IM,4).EQ.0) THEN     
        CALL SXQTB2(IM,WORK,G2(1+JV*IM*(JD-1)))
     ELSE IF(JV.EQ.8.AND.MOD(IM,8).EQ.0) THEN     
        CALL SXOTB2(IM,WORK,G2(1+JV*IM*(JD-1)))
     ELSE
        CALL SXLTB2(JV,IM,WORK,G2(1+JV*IM*(JD-1)))
     END IF
  END DO
  !$omp end do
  CALL MXFREE(WP)    
!$omp end parallel

END SUBROUTINE SXTS2V
