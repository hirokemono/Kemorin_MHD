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
SUBROUTINE FXRMBP(M,N,X,T)

  INTEGER(8) :: N,IV,M
  REAL(8) :: T(N/2)
  REAL(8) :: X(M,2,0:N/2-1)
  REAL(8) :: TMP,TMPP1,TMPP2,TMPM1,TMPM2

  DO IV=1,M
     TMP=X(IV,1,0)
     X(IV,1,0)=X(IV,1,0)+X(IV,2,0)
     X(IV,2,0)=TMP-X(IV,2,0)
  END DO
  DO I=1,(N-2)/4
     DO IV=1,M
        TMPP1=X(IV,1,I)+X(IV,1,N/2-I)
        TMPP2=X(IV,2,I)+X(IV,2,N/2-I)
        TMPM1=X(IV,1,I)-X(IV,1,N/2-I)
        TMPM2=X(IV,2,I)-X(IV,2,N/2-I)
        X(IV,1,I)=TMPP1-TMPM1*T(2*I)-TMPP2*T(2*I-1)
        X(IV,2,I)=TMPM2+TMPM1*T(2*I-1)-TMPP2*T(2*I)
        X(IV,1,N/2-I)=2*TMPP1-X(IV,1,I)
        X(IV,2,N/2-I)=X(IV,2,I)-2*TMPM2
     END DO
  END DO
  IF(MOD(N,4).EQ.0) THEN
      DO IV=1,M
        X(IV,1,N/4)= 2*X(IV,1,N/4)
        X(IV,2,N/4)=-2*X(IV,2,N/4)
      END DO
  END IF

END SUBROUTINE FXRMBP
