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
*     initialize table for rfft1024 (for sse32) (backward)
*-----------------------------------------------------------------------
      SUBROUTINE FJRTBA(XT)
      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(ZI=(0,1))
      DIMENSION XT(2,2,0:767)
      
      PI=4*ATAN(1D0)

      DO J=0,15
        DO I=0,31
          XT(1,1,I+32*J)=COS(2*PI*I*J/512)
          XT(2,1,I+32*J)=COS(2*PI*I*J/512)
          XT(1,2,I+32*J)=-SIN(2*PI*I*J/512)
          XT(2,2,I+32*J)=SIN(2*PI*I*J/512)        
        END DO
      END DO

      DO K=0,255
        XT(1,1,512+K)=-SIN(2*PI*K/1024)
        XT(2,1,512+K)=-SIN(2*PI*K/1024)
        XT(1,2,512+K)=-COS(2*PI*K/1024)
        XT(2,2,512+K)= COS(2*PI*K/1024)
      END DO

      END
