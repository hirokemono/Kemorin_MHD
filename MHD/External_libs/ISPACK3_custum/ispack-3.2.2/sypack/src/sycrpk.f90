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
SUBROUTINE SYCRPK(MM,NT1,NT2,S1,S2,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,M1,N1,MN,NS,NSG,M,N,K,NT1,NT2,L,ICOM
  REAL(8) :: S1(*) ! S1((MM/NP+1)*(2*(NT1+1)-MM/NP*NP))
  REAL(8) :: S2(*) ! S2((MM/NP+1)*(2*(NT2+1)-MM/NP*NP))
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
  DO L=1,(MM/NP+1)*(2*(NT2+1)-MM/NP*NP)
     S2(L)=0
  END DO
  !$omp end parallel do     

  IF(NT2.GT.NT1) THEN
     !$omp parallel private(NS,NSG,N,M,L) num_threads(NTHMAX)
     !$omp do schedule(dynamic)
     DO K=0,MN-1
        M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
        IF(M.EQ.0) THEN
           DO N=0,NT1
              S2(N+1)=S1(N+1)
           END DO
           DO N=NT1+1,NT2
              S2(N+1)=0
           END DO
        ELSE
           NS=K*(2*(NT1+1)-(K-1)*NP)+1
           NSG=K*(2*(NT2+1)-(K-1)*NP)+1
           DO L=0,2*(NT1-M+1)-1
              S2(NSG+L)=S1(NS+L)
           END DO
           DO L=2*(NT1-M+1),2*(NT2-M+1)-1
              S2(NSG+L)=0
           END DO
        END IF
     END DO
     !$omp end do
     !$omp end parallel
  ELSE IF(NT2.LT.NT1) THEN
     !$omp parallel private(NS,NSG,N,M,L) num_threads(NTHMAX)  
     !$omp do schedule(dynamic)
     DO K=0,MN-1
        M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
        IF(M.EQ.0) THEN
           DO N=0,NT2
              S2(N+1)=S1(N+1)
           END DO
        ELSE
           NS=K*(2*(NT1+1)-(K-1)*NP)+1
           NSG=K*(2*(NT2+1)-(K-1)*NP)+1
           DO L=0,2*(NT2-M+1)-1
              S2(NSG+L)=S1(NS+L)
           END DO
        END IF
     END DO
     !$omp end do
     !$omp end parallel
  ELSE
     !$omp parallel do num_threads(NTHMAX)  
     DO L=1,(MM/NP+1)*(2*(NT2+1)-MM/NP*NP)
        S2(L)=S1(L)
     END DO
     !$omp end parallel do     
  END IF

END SUBROUTINE SYCRPK
