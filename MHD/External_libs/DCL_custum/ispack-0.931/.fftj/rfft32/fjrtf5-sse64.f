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
*     initialize table for rfft32 (for sse64) (forward)
*-----------------------------------------------------------------------
      SUBROUTINE FJRTF5(XT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      DIMENSION XT(2,2,0:23)

      PI=4*ATAN(1D0)

      DO K=0,7
        XT(1,1,16+K)=0.5D0*(1-SIN(2*PI*K/32))
        XT(2,1,16+K)=0.5D0*(1-SIN(2*PI*K/32))
        XT(1,2,16+K)= 0.5D0*COS(2*PI*K/32)
        XT(2,2,16+K)=-0.5D0*COS(2*PI*K/32)
      END DO

      END
