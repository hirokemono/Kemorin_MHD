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
SUBROUTINE LXINIR(NM,M,R)

  IMPLICIT NONE
  INTEGER(8) :: NM,N,M,IE,IA,IC,L
  REAL(16) :: QR,QR2
  REAL(8) :: R((NM-M)/2*3+NM-M+1)

  IF(NM.GE.M+1) THEN
     R=0
     IE=0
     IA=IE+(NM-M)/2*2
     IC=IA+(NM-M+1)/2
     QR=SQRT(2Q0*M+3)
     DO L=1,(NM-M)/2
        N=M+2*L-1
        R(IA+L)=QR
        QR2=(-1)**(L-1)*QR*QR
        R(IC+2*L-1)=QR2
        R(IC+2*L)=-QR2*(QE(N+1,M)**2+QE(N,M)**2)
        QR=QR*QE(N+1,M)
        R(IE+2*L-1)=QR
        QR=(-1)**(L-1)/QR
        R(IE+2*L)=QR
        QR=QR/QE(N+2,M)
     END DO
     IF(MOD(NM-M-1,2).EQ.0) THEN
        L=(NM-M+1)/2
        R(IA+L)=QR
     END IF
  ELSE
     R=0        
  END IF
  
CONTAINS
  
  FUNCTION QE(N,M)
    INTEGER(8) :: N,M
    REAL(16) :: QE
    QE=SQRT(1Q0*(N-M)*(N+M)/((2Q0*N-1)*(2*N+1)))      
  END FUNCTION QE
  
END SUBROUTINE LXINIR
