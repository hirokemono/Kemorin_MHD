************************************************************************
* FTTJ:  An FFT library
* Copyright (C) 2008 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
*     initialize table for rfft512 (for fortran) (backward)
*-----------------------------------------------------------------------
      SUBROUTINE FJRTB9(ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(ZI=(0,1))
      DIMENSION ZT(0:383)

      PI=4*ATAN(1D0)

      DO J=0,15
        DO I=0,15
          ZT(I+16*J)=EXP(ZI*2*PI*I*J/256)
        END DO
      END DO

      DO K=0,127
        ZT(256+K)=ZI*EXP(ZI*2*PI*K/512)
      END DO

      END
