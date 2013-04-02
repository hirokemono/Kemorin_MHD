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
*     initialize table for rfft128 (for sse64) (forward)
*-----------------------------------------------------------------------
      SUBROUTINE FJRTF7(XT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      DIMENSION XT(2,2,0:95)

      PI=4*ATAN(1D0)

      DO J=0,7
        DO I=0,7
          XT(1,1,I+8*J)=COS(2*PI*I*J/64)
          XT(2,1,I+8*J)=COS(2*PI*I*J/64)
          XT(1,2,I+8*J)=-SIN(2*PI*I*J/64)
          XT(2,2,I+8*J)=SIN(2*PI*I*J/64)        
        END DO
      END DO

      DO K=0,31
        XT(1,1,64+K)=0.5D0*(1-SIN(2*PI*K/128))
        XT(2,1,64+K)=0.5D0*(1-SIN(2*PI*K/128))
        XT(1,2,64+K)= 0.5D0*COS(2*PI*K/128)
        XT(2,2,64+K)=-0.5D0*COS(2*PI*K/128)
      END DO

      END
