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
SUBROUTINE LXCSWY(NT,M,S,SG,C)

  IMPLICIT NONE
  INTEGER(8) :: NT,M,N
  REAL(8) :: S(2,M:NT),SG(2,M:NT+1)
  REAL(8) :: C(2*(NT-M)+1)  

  N=M
  IF(N.NE.NT) THEN
     SG(1,N)=C(NT-M+1+1)*S(1,N+1)
     SG(2,N)=C(NT-M+1+1)*S(2,N+1)         
  END IF
  DO N=M+1,NT-1
     SG(1,N)=C(N-M)*S(1,N-1)+C(NT-M+1+1+N-M)*S(1,N+1)
     SG(2,N)=C(N-M)*S(2,N-1)+C(NT-M+1+1+N-M)*S(2,N+1)
  END DO
  IF(M.NE.NT) THEN
     SG(1,NT)=C(NT-M)*S(1,NT-1)
     SG(2,NT)=C(NT-M)*S(2,NT-1)         
  ELSE
     SG(1,NT)=0
     SG(2,NT)=0         
  END IF
  SG(1,NT+1)=C(NT+1-M)*S(1,NT)
  SG(2,NT+1)=C(NT+1-M)*S(2,NT)      

END SUBROUTINE LXCSWY
