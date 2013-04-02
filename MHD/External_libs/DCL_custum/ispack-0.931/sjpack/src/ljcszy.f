************************************************************************
* ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
* Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
************************************************************************
************************************************************************
*     REPACKING SPECTRA TRANSFORM for y grad (zonal component)
*                                                            20010/01/28
*-----------------------------------------------------------------------      
      SUBROUTINE LJCSZY(MM,S,SG,C)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(0:MM),SG(0:MM+1)
      DIMENSION C(*)

      SG(0)=C(MM+1+1)*S(1)
      DO N=1,MM-1
        SG(N)=C(N)*S(N-1)+C(MM+1+N+1)*S(N+1)
      END DO
      SG(MM)=C(MM)*S(MM-1)
      SG(MM+1)=C(MM+1)*S(MM)

      END
