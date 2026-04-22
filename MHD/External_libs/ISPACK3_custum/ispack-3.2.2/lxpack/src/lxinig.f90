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
SUBROUTINE LXINIG(JM,P,IG)

  IMPLICIT NONE
  INTEGER(8) :: JM,JH,J,N,IFLAG,IG
  REAL(8) :: P(JM/2,5)
  REAL(16) :: QEPS,QPI,QP0,QP1,QDP,QDZ,QZ
  INTEGER(8) :: NPMAX    

  JH=JM/2

  QPI=4*ATAN(1Q0)

  CALL MXGOMP(NPMAX)    

  IF(IG.EQ.1) THEN ! Gaussian Grid
     
     QEPS=1Q-30

     !$omp parallel private(QZ,N,IFLAG,QP0,QP1,QDP,QDZ) num_threads(NPMAX)
     !$omp do schedule(dynamic)
     DO J=1,JH
        QZ=SIN(QPI*(2*J-1)/(2*JM+1))
        IFLAG=0
        DO 
           QP0=0
           QP1=1
           DO N=1,JM-1,2
              QP0=((2*N-1)*QZ*QP1-(N-1)*QP0)/N
              QP1=((2*N+1)*QZ*QP0-N*QP1)/(N+1)
           END DO
           QDP=JM*(QP0-QZ*QP1)/(1-QZ*QZ)
           QDZ=QP1/QDP
           QZ=QZ-QDZ
           IF(IFLAG.EQ.1) EXIT
           IF(ABS(QDZ/QZ).LE.QEPS) IFLAG=1
        END DO
        P(J,1)=QZ
        P(J,2)=1/(QDP*QDP)/(1-QZ*QZ)
        P(J,3)=SQRT(1-QZ*QZ)
        P(J,4)=1/SQRT(1-QZ*QZ)
        P(J,5)=QZ*QZ
     END DO
     !$omp end do
     !$omp end parallel

  ELSE IF(IG.EQ.2) THEN ! Clenshaw-Curtis
     
     !$omp parallel private(QZ,N,QP0,QP1) num_threads(NPMAX)
     !$omp do schedule(dynamic)
     DO J=1,JH
        QP0=QPI/2*(2*J-1)/(2*JH+1)
        QZ=SIN(QP0)
        P(J,1)=QZ
        QP1=0
        DO N=1,2*JH-1,2
           QP1=QP1+SIN(N*(QPI/2-QP0))/N
        END DO
        P(J,2)=2*QP1*COS(QP0)/(2*JH+1)
        P(J,3)=SQRT(1-QZ*QZ)
        P(J,4)=1/SQRT(1-QZ*QZ)
        P(J,5)=QZ*QZ
     END DO
     !$omp end do
     !$omp end parallel

  ELSE IF(IG.EQ.3) THEN ! Clenshaw-Curtis including equator
     
     !$omp parallel private(QZ,N,QP0,QP1) num_threads(NPMAX)
     !$omp do schedule(dynamic)
     DO J=1,JH
        QP0=QPI/2*(J-1)/JH
        QZ=SIN(QP0)
        P(J,1)=QZ
        QP1=0
        DO N=1,2*JH-1,2
           QP1=QP1+SIN(N*(QPI/2-QP0))/N
        END DO
        P(J,2)=2*QP1*COS(QP0)/(2*JH)
        P(J,3)=SQRT(1-QZ*QZ)
        P(J,4)=1/SQRT(1-QZ*QZ)
        P(J,5)=QZ*QZ
     END DO
     !$omp end do
     !$omp end parallel
     
     P(1,2)=P(1,2)/2

  END IF

END SUBROUTINE LXINIG
