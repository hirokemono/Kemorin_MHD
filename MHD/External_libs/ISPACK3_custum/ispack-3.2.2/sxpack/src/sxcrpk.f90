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
SUBROUTINE SXCRPK(MM,NT1,NT2,S1,S2)

  IMPLICIT NONE
  INTEGER(8) :: MM,NT1,NT2,M,N,NS1,NS2,L
  REAL(8) :: S1((2*NT1+1-MM)*MM+NT1+1),S2((2*NT2+1-MM)*MM+NT2+1)
  INTEGER(8) :: NPMAX

  CALL MXGOMP(NPMAX)  

  IF(NT2.GT.NT1) THEN
     !$omp parallel private(NS1,NS2,N,L) num_threads(NPMAX)
     !$omp do schedule(dynamic)
     DO M=0,MM
        IF(M.EQ.0) THEN
           DO N=0,NT1
              S2(N+1)=S1(N+1)
           END DO
           DO N=NT1+1,NT2
              S2(N+1)=0
           END DO
        ELSE
           NS1=(2*NT1+2-M)*(M-1)+NT1+1+1
           NS2=(2*NT2+2-M)*(M-1)+NT2+1+1           
           DO L=0,2*(NT1-M+1)-1
              S2(NS2+L)=S1(NS1+L)
           END DO
           DO L=2*(NT1-M+1),2*(NT2-M+1)-1
              S2(NS2+L)=0
           END DO
        END IF
     END DO
     !$omp end do
     !$omp end parallel
  ELSE IF(NT2.LT.NT1) THEN
     !$omp parallel private(NS1,NS2,N,L) num_threads(NPMAX)
     !$omp do schedule(dynamic)
     DO M=0,MM
        IF(M.EQ.0) THEN
           DO N=0,NT2
              S2(N+1)=S1(N+1)
           END DO
        ELSE
           NS1=(2*NT1+2-M)*(M-1)+NT1+1+1
           NS2=(2*NT2+2-M)*(M-1)+NT2+1+1           
           DO L=0,2*(NT2-M+1)-1
              S2(NS2+L)=S1(NS1+L)
           END DO
        END IF
     END DO
     !$omp end do
     !$omp end parallel
  ELSE
     S2=S1
  END IF

END SUBROUTINE SXCRPK
