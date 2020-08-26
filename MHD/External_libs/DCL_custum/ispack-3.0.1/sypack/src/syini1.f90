!***********************************************************************
! ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
! Copyright (C) 1998--2019 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
SUBROUTINE SYINI1(MM,NM,IM,IT,T,R,ICOM)

  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER :: NP,IP,IERR,ICOM4
  INTEGER(8) :: MM,NM,IM,M,IE,K,ICOM
  INTEGER(8) :: N1,M1,MN
  INTEGER(8) :: IT(IM/2)  
  REAL(8) :: T(IM*3/2)
  REAL(8) :: R(*) ! R(5*(MM/NP+1)*(2*NM-MM/NP*NP)/4+MM/NP+1)
  INTEGER(8) :: NTHMAX

  CALL MXGOMP(NTHMAX)  

  CALL FXRINI(IM,IT,T)      

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

  CALL MXGOMP(NTHMAX)  

  !$omp parallel private(K,M,IE) num_threads(NTHMAX)
  !$omp do schedule(dynamic)
  DO K=0,MN-1
     IE=5*K*(2*NM-NP*(K-1))/4+K+1 ! ���������Ѥ����ΤǤ����ʤ�
     M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
     IF(M.EQ.0) THEN
        CALL LXINIR(NM,0_8,R)          
     ELSE
        CALL LXINIR(NM,M,R(IE))                  
     END IF
  END DO
  !$omp end do
  !$omp end parallel  

END SUBROUTINE SYINI1
