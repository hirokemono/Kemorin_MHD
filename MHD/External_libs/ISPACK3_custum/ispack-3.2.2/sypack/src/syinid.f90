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
SUBROUTINE SYINID(MM,NT,D,ICOM)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,M1,N1,MN,NS,M,N,K,ND,NT,ICOM
  REAL(8) :: D(*) ! D((MM/NP+1)*(2*(NT+1)-MM/NP*NP)*2)
  INTEGER :: NP,IP,IERR,ICOM4
  INTEGER(8) :: NTHMAX

  CALL MXGOMP(NTHMAX)  

  ICOM4=ICOM
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  ND=(MM/NP+1)*(2*(NT+1)-MM/NP*NP)

  !  D(1:ND*2)=0
  !$omp parallel do num_threads(NTHMAX)  
  DO K=1,ND*2
     D(K)=0
  END DO
  !$omp end parallel do  

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

  !$omp parallel private(NS,M,N) num_threads(NTHMAX)
  !$omp do schedule(dynamic)
  DO K=0,MN-1
     M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
     NS=K*(2*(NT+1)-(K-1)*NP)+1
     IF(M.EQ.0) THEN
        D(1)=0
        D(1+ND)=1
        DO N=1,NT
           D(NS+N-M)=-1D0*N*(N+1)
           D(NS+N-M+ND)=1/D(NS+N-M)
        END DO
     ELSE
        DO N=M,NT
           D(NS+2*(N-M))=-1D0*N*(N+1)
           D(NS+2*(N-M)+1)=D(NS+2*(N-M))
           D(NS+2*(N-M)+ND)=1/D(NS+2*(N-M))
           D(NS+2*(N-M)+1+ND)=D(NS+2*(N-M)+ND)
        END DO
     END IF
  END DO
  !$omp end do
  !$omp end parallel

END SUBROUTINE SYINID
