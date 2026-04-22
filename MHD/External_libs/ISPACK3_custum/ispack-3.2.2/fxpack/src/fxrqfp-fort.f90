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
SUBROUTINE FXRQFP(N,X,T)

  INTEGER(8),PARAMETER :: M=4
  INTEGER(8) :: N,IV
  REAL(8) :: T(N/2)
  REAL(8) :: X(M,2,0:N/2-1)
  REAL(8) :: F,TMP,ADDR,SUBR,ADDI,SUBI,TMPR,TMPI

  F=1D0/N

  DO IV=1,M
     TMP=X(IV,1,0)*F
     X(IV,1,0)=TMP+X(IV,2,0)*F
     X(IV,2,0)=TMP-X(IV,2,0)*F
  END DO
  DO I=1,(N-2)/4
     DO IV=1,M  
        ADDR=(X(IV,1,N/2-I)+X(IV,1,I))*0.5D0*F
        SUBR=(X(IV,1,N/2-I)-X(IV,1,I))*0.5D0*F
        ADDI=(X(IV,2,N/2-I)+X(IV,2,I))*0.5D0*F
        SUBI=(X(IV,2,N/2-I)-X(IV,2,I))*0.5D0*F
        TMPR=ADDR-SUBR*T(2*I)+ADDI*T(2*I-1)
        TMPI=SUBI-ADDI*T(2*I)-SUBR*T(2*I-1)
        X(IV,1,I)=TMPR
        X(IV,2,I)=TMPI
        X(IV,1,N/2-I)=2*ADDR-TMPR
        X(IV,2,N/2-I)=TMPI-2*SUBI
     END DO
  END DO
  IF(MOD(N,4).EQ.0) THEN
     DO IV=1,M
        X(IV,1,N/4)=X(IV,1,N/4)*F
        X(IV,2,N/4)=-X(IV,2,N/4)*F
     END DO
  END IF

END SUBROUTINE FXRQFP
