!************************************************************************
!* ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
!* Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
!*
!* This library is free software; you can redistribute it and/or
!* modify it under the terms of the GNU Lesser General Public
!* License as published by the Free Software Foundation; either
!* version 2.1 of the License, or (at your option) any later version.
!*
!* This library is distributed in the hope that it will be useful,
!* but WITHOUT ANY WARRANTY; without even the implied warranty of
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!* Lesser General Public License for more details.
!* 
!* You should have received a copy of the GNU Lesser General Public
!* License along with this library; if not, write to the Free Software
!* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!* 02110-1301 USA.
!************************************************************************
!
!      module ispack_0931
!
!************************************************************************
!
!      SUBROUTINE FTTZUI(N,IT,T)
!      SUBROUTINE FTTZUF(M,N,X,Y,IT,T)
!      SUBROUTINE FTTZUB(M,N,X,Y,IT,T)
!
!      SUBROUTINE FTTRUI(N,IT,T)
!      SUBROUTINE FTTRUF(M,N,X,Y,IT,T)
!      SUBROUTINE FTTRUB(M,N,X,Y,IT,T)
!
!************************************************************************!*     COSINE TRANSFORM (TRAPEZOIDAL)   2000/09/19 K.Ishioka
!************************************************************************!      SUBROUTINE FTTCTI(N,IT,T)
!      SUBROUTINE FTTCTF(M,N,X,Y,IT,T)
!      SUBROUTINE FTTCTB(M,N,X,Y,IT,T)
!
!************************************************************************!*     SINE TRANSFORM (TRAPEZOIDAL)     2000/09/19 K.Ishioka
!************************************************************************!      SUBROUTINE FTTSTI(N,IT,T)
!      SUBROUTINE FTTSTF(M,N,X,Y,IT,T)
!      SUBROUTINE FTTSTB(M,N,X,Y,IT,T)
!
!************************************************************************!*     COSINE TRANSFORM (MID-POINT)     2000/09/19 K.Ishioka
!************************************************************************!      SUBROUTINE FTTCMI(N,IT,T)
!      SUBROUTINE FTTCMF(M,N,X,Y,IT,T)
!      SUBROUTINE FTTCMB(M,N,X,Y,IT,T)
!
!************************************************************************!*     SINE TRANSFORM (MID-POINT)       2000/09/19 K.Ishioka
!************************************************************************!      SUBROUTINE FTTSMI(N,IT,T)
!      SUBROUTINE FTTSMF(M,N,X,Y,IT,T)
!      SUBROUTINE FTTSMB(M,N,X,Y,IT,T)
!
      module ispack_0931
!
      IMPLICIT REAL*8(A-H,O-Z)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      SUBROUTINE FTTZUI(N,IT,T)
!
      use ftz0
!
      DIMENSION T(0:N-1,2),IT(5)

      CALL FTTZUI0(N,IT,T)

      END SUBROUTINE FTTZUI
!************************************************************************
      SUBROUTINE FTTZUF(M,N,X,Y,IT,T)
!
      use ftz0
!
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      CALL FTTZUF0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTZUF
!************************************************************************
      SUBROUTINE FTTZUB(M,N,X,Y,IT,T)
!
      use ftz0
!
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      CALL FTTZUB0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTZUB
!************************************************************************
! -----------------------------------------------------------------------
!************************************************************************
      SUBROUTINE FTTRUI(N,IT,T)
!
      use ftr0
!
      DIMENSION T(0:N/2-1,4),IT(5)

      CALL FTTRUI0(N,IT,T)

      END SUBROUTINE FTTRUI
!************************************************************************
      SUBROUTINE FTTRUF(M,N,X,Y,IT,T)
!
      use ftr0
!
      DIMENSION X(M,2,0:N/2-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      CALL FTTRUF0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTRUF
!************************************************************************
      SUBROUTINE FTTRUB(M,N,X,Y,IT,T)
!
      use ftr0
!
      DIMENSION X(M,2,0:N/2-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      CALL FTTRUB0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTRUB
!
!************************************************************************
! -----------------------------------------------------------------------
!************************************************************************
!*
      SUBROUTINE FTTCTI(N,IT,T)
!
      use ftct
!
      DIMENSION T(0:N/2-1,6),IT(5)

      CALL FTTCTI0(N,IT,T)
 
      END SUBROUTINE FTTCTI
!************************************************************************
      SUBROUTINE FTTCTF(M,N,X,Y,IT,T)
!
      use ftct
!
      DIMENSION IT(5),T(0:N/2-1,6)
      DIMENSION X(M,0:N),Y(M,0:N-1)

      CALL FTTCTF0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTCTF
!************************************************************************
      SUBROUTINE FTTCTB(M,N,X,Y,IT,T)
!
      use ftct
!
      DIMENSION IT(5),T(0:N/2-1,6)
      DIMENSION X(M,0:N),Y(M,0:N-1)

      call FTTCTB0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTCTB
!************************************************************************
! -----------------------------------------------------------------------
!************************************************************************
!
      SUBROUTINE FTTSTI(N,IT,T)
!
      use ftst
!
      DIMENSION T(0:N/2-1,5),IT(5)

      CALL FTTSTI0(N,IT,T)
 
      END SUBROUTINE FTTSTI
!************************************************************************
      SUBROUTINE FTTSTF(M,N,X,Y,IT,T)
!
      use ftst
!
      DIMENSION IT(5),T(0:N/2-1,5)
      DIMENSION X(M,N),Y(M,0:N-1)

      CALL FTTSTF0(M,N,X,Y,IT,T)
 
      END SUBROUTINE FTTSTF
!************************************************************************
      SUBROUTINE FTTSTB(M,N,X,Y,IT,T)
!
      use ftst
!
      DIMENSION IT(5),T(0:N/2-1,5)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)

      CALL FTTSTB0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTSTB
!************************************************************************
! -----------------------------------------------------------------------
!************************************************************************
      SUBROUTINE FTTCMI(N,IT,T)
!
      use ftcm
!
      DIMENSION T(0:N/2-1,12),IT(5)

      CALL FTTCMI0(N,IT,T)
 
      END SUBROUTINE FTTCMI
!************************************************************************
      SUBROUTINE FTTCMF(M,N,X,Y,IT,T)
!
      use ftcm
!
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)
 
      CALL FTTCMF0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTCMF
!************************************************************************
      SUBROUTINE FTTCMB(M,N,X,Y,IT,T)
!
      use ftcm
!
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)
 
      CALL FTTCMB0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTCMB
!
!************************************************************************
! -----------------------------------------------------------------------
!************************************************************************
      SUBROUTINE FTTSMI(N,IT,T)
!
      use ftsm
!
      DIMENSION T(0:N/2-1,12),IT(5)

      CALL FTTSMI0(N,IT,T)
 
      END SUBROUTINE FTTSMI
!************************************************************************
      SUBROUTINE FTTSMF(M,N,X,Y,IT,T)
!
      use ftsm
!
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)
 
      CALL FTTSMF0(M,N,X,Y,IT,T)

      END SUBROUTINE FTTSMF
!************************************************************************
      SUBROUTINE FTTSMB(M,N,X,Y,IT,T)
!
      use ftsm
!
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)

      CALL FTTSMB0(M,N,X,Y,IT,T)
      
      END SUBROUTINE FTTSMB
!************************************************************************
! -----------------------------------------------------------------------
!
      end module ispack_0931
