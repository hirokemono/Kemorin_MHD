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
SUBROUTINE SYL2NM(MM,NN,L,N,M,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,NN,N,M,L,K,LS,LM,ICOM
  REAL(8) :: DK
  INTEGER :: NP,IP,IERR,ICOM4

  ICOM4=ICOM
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  LM=(MM/NP+1)*(2*(NN+1)-MM/NP*NP)

  IF(L.GT.LM) THEN
     N=-1
     M=0
     RETURN
  END IF

  DK=(NN+1D0)/NP+0.5D0
  DK=DK-SQRT(DK*DK-1D0*(L-0.5D0)/NP)
  K=DK

  M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)

  IF(M.GT.MM) THEN
     N=-1
     M=0
     RETURN
  END IF

  LS=K*(2*(NN+1)-(K-1)*NP)+1

  IF(M.EQ.0) THEN
     N=L-1
  ELSE
     N=M+(L-LS)/2
     IF(MOD(L-LS,2).EQ.1) THEN
        M=-M
     END IF
  END IF

  IF(N.GT.NN) THEN
     N=-1
     M=0
  END IF

END SUBROUTINE SYL2NM
