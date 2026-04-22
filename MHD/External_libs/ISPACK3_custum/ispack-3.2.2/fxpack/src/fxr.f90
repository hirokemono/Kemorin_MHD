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
SUBROUTINE FXRTBA(M,N,X,IT,T)

  INTEGER(8) :: M,N,IT(N/2)
  REAL(8) :: T(N+N/2)  
  REAL(8) :: X(M,2,0:N/2-1)

  CALL FXRTBP(M,N,X,T(N+1))  
  CALL FXZTBA(M,N/2,X,IT,T)

END SUBROUTINE FXRTBA
!------------------------------------------------------------------------  
SUBROUTINE FXRTFA(M,N,X,IT,T)

  INTEGER(8) :: M,N,IT(N/2)
  REAL(8) :: T(N+N/2)  
  REAL(8) :: X(M,2,0:N/2-1)

  CALL FXZTBA(M,N/2,X,IT,T)
  CALL FXRTFP(M,N,X,T(N+1))

END SUBROUTINE FXRTFA
!------------------------------------------------------------------------  
SUBROUTINE FXRTBP(M,N,X,T)

  INTEGER(8) :: M,N
  REAL(8) :: X(*),T(*)

  IF(M.EQ.4) THEN
     CALL FXRQBP(N,X,T)
  ELSE IF(M.EQ.8) THEN
     CALL FXROBP(N,X,T)
  ELSE
     CALL FXRMBP(M,N,X,T)
  END IF

END SUBROUTINE FXRTBP
!------------------------------------------------------------------------  
SUBROUTINE FXRTFP(M,N,X,T)

  INTEGER(8) :: M,N
  REAL(8) :: X(*),T(*)

  IF(M.EQ.4) THEN
     CALL FXRQFP(N,X,T)
  ELSE IF(M.EQ.8) THEN
     CALL FXROFP(N,X,T)
  ELSE
     CALL FXRMFP(M,N,X,T)
  END IF

END SUBROUTINE FXRTFP
!------------------------------------------------------------------------
SUBROUTINE FXRINI(N,IT,T)

  INTEGER(8) :: N,IT(N/2),I
  REAL(8) :: T(N+N/2)
  REAL(16) :: PI

  PI=4*ATAN(1Q0)
  T=0

  IF(MOD(N,2).NE.0) THEN
     PRINT*, '*** error in FXRINI: MOD(N,2).ne.0'
     STOP
  END IF

  CALL FXZINI(N/2,IT,T)

  DO I=1,(N-2)/4
     T(N+2*I-1)=COS(2*PI*I/N)
     T(N+2*I)  =SIN(2*PI*I/N)        
  END DO

END SUBROUTINE FXRINI
