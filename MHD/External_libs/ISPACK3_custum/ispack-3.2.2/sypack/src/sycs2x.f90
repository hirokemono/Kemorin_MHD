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
!***********************************************************************
!     REPACKING SPECTRA FOR x-grad                            
!***********************************************************************
SUBROUTINE SYCS2X(MM,NT,S,SX,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,M,N,L,N1,M1,MN,K,NS,NT,ICOM
  REAL(8) :: S(*) ! S((MM/NP+1)*(2*(NT+1)-MM/NP*NP))
  REAL(8) :: SX(*) ! SX((MM/NP+1)*(2*(NT+1)-MM/NP*NP))
  INTEGER :: NP,IP,IERR,ICOM4
  INTEGER(8) :: NTHMAX

  CALL MXGOMP(NTHMAX)  

  ICOM4=ICOM
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  N1=(MM+1)/NP
  M1=N1*NP
  MN=N1
  IF(MOD(N1,2).EQ.0) THEN
     IF(IP.LE.MM-M1) THEN
        MN=MN+1
     END IF
  ELSE
     IF(IP.GE.NP-MM+M1-1) THEN
        MN=MN+1
     END IF
  END IF

  !$omp parallel do num_threads(NTHMAX)  
  DO L=1,(MM/NP+1)*(2*(NT+1)-MM/NP*NP)
     SX(L)=0
  END DO
  !$omp end parallel do     

  !$omp parallel private(NS,M,N,L) num_threads(NTHMAX)  
  !$omp do schedule(dynamic)
  DO K=0,MN-1
     M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
     NS=K*(2*(MM+1)-(K-1)*NP)+1
     IF(M.EQ.0) THEN
        DO N=0,NT
           SX(N+1)=0
        END DO
     ELSE
        DO N=M,NT
           L=NS+2*(N-M)
           SX(L)=-M*S(L+1)
           SX(L+1)=M*S(L)
        END DO
     END IF
  END DO
  !$omp end do
  !$omp end parallel

END SUBROUTINE SYCS2X
