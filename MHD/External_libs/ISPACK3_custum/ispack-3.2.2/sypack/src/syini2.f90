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
SUBROUTINE SYINI2(MM,NM,JM,IG,P,R,JC,ICOM)

  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER :: NP,IP,IERR,ICOM4
  INTEGER(8) :: MM,NM,JM,IG,M,IE,IJ,K,ICOM
  INTEGER(8) :: N1,M1,MN  
  REAL(8) :: P(JM/2,*) ! P(JM/2,5+2*(MM/NP+1))
  REAL(8) :: R(*) ! R(5*(MM/NP+1)*(2*NM-MM/NP*NP)/4+MM/NP+1)
  INTEGER(8) :: JC(*) ! JC((MM/NP+1)*(2*NM-MM/NP*NP)/16+MM/NP+1)
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

  CALL LXINIG(JM,P,IG)

  !$omp parallel private(K,M,IE,IJ) num_threads(NTHMAX)
  !$omp do schedule(dynamic)
  DO K=0,MN-1
     IE=5*K*(2*NM-NP*(K-1))/4+K+1 ! サイズを変えたのでこうなる
     IJ=K*(2*NM-NP*(K-1))/16+K+1
     M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
     IF(M.NE.0) THEN     
        CALL LXINIW(NM,JM,M,P,P(1,5+2*K+1),R(IE),JC(IJ))
     END IF
  END DO
!$omp end do
!$omp end parallel  
  
END SUBROUTINE SYINI2

