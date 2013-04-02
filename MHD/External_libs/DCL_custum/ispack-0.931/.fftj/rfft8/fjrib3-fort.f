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
* rfft 8 in-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJRIB3(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.7071067811865475244D0) ! COS(2*PI/8)
      PARAMETER(ZT2=(-0.7071067811865475244D0,0.7071067811865475244D0))
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:3),ZD(*),ZDD(*)
      DIMENSION ZT(*)
      
*-----------------------------------------------------------------------
* rfft のための前処理
*-----------------------------------------------------------------------

      Z(0)=(1+ZI)*CONJG(Z(0))
      
      ZTMP1=(Z(1)-CONJG(Z(3)))*ZT2
      ZTMP0=Z(1)+CONJG(Z(3))
      Z(1)=ZTMP0+ZTMP1
      Z(3)=CONJG(ZTMP0-ZTMP1)

      Z(2)=2*CONJG(Z(2))

*-----------------------------------------------------------------------
* fft 4 in-place backward
*-----------------------------------------------------------------------
      Z0=Z(0)+Z(2)
      Z2=Z(0)-Z(2)
      Z1=Z(1)+Z(3)
      Z3=ZI*(Z(1)-Z(3))

      Z(0)=Z0+Z1
      Z(2)=Z0-Z1
      Z(1)=Z2+Z3
      Z(3)=Z2-Z3

      END
