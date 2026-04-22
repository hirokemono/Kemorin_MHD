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
SUBROUTINE LXTSZP(NN,S,GNP,GSP)

  IMPLICIT NONE
  INTEGER(8) :: NN,N
  REAL(8) ::  S(0:NN),G1,G2,GNP,GSP

  G1=0
  DO N=0,NN,2
     G1=G1+S(N)*SQRT(2D0*N+1)
  END DO

  G2=0
  DO N=1,NN,2
     G2=G2+S(N)*SQRT(2D0*N+1)
  END DO

  GNP=G1+G2
  GSP=G1-G2

END SUBROUTINE LXTSZP
