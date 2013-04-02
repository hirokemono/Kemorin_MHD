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
* rfft 128 in-place forward
*-----------------------------------------------------------------------
      SUBROUTINE FJRIF7(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.7071067811865475244D0) ! COS(2*PI/8)
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:63),ZD(0:63),ZDD(0:63)
      DIMENSION ZT(0:95)

*-----------------------------------------------------------------------
* fft 64 in-place backward (8x8 type)
*-----------------------------------------------------------------------
      I=0

      Z0=Z(I+8*1)
      Z1=Z(I+8*5)
      Z2=Z(I+8*3)
      Z3=Z(I+8*7)
      
      Z7=Z0
      Z0=Z0-Z1
      Z1=Z1+Z7

      Z7=Z2
      Z2=Z2-Z3
      Z3=Z3+Z7

      Z7=Z1
      Z1=Z1-Z3
      Z3=Z3+Z7

      Z7=Z0
      Z0=Z0-Z2
      Z2=Z2+Z7

      Z1=Z1*ZI
      Z2=Z2*ZI*C1P8
      Z0=Z0*C1P8

      Z4=Z(I+8*2)
      Z5=Z(I+8*6)

      Z7=Z4
      Z4=Z4-Z5
      Z5=Z5+Z7

      Z4=Z4*ZI

      Z7=Z2
      Z2=Z2-Z4
      Z4=Z4+Z7

      ZD(2+8*I)=Z3
      
      Z6=Z(I+8*0)
      Z7=Z(I+8*4)
      Z3=Z6
      Z6=Z6-Z7
      Z7=Z7+Z3

      Z3=Z6
      Z6=Z6-Z0
      Z0=Z0+Z3

      Z3=Z6
      Z6=Z6-Z2
      Z2=Z2+Z3
      ZD(3+8*I)=Z2
      ZD(5+8*I)=Z6

      Z2=Z0
      Z0=Z0-Z4
      Z4=Z4+Z2
      ZD(7+8*I)=Z0
      ZD(1+8*I)=Z4

      Z2=Z7
      Z7=Z7-Z5
      Z5=Z5+Z2

      Z3=ZD(2+8*I)

      Z2=Z5
      Z5=Z5-Z3
      Z3=Z3+Z2
      ZD(4+8*I)=Z5
      ZD(0+8*I)=Z3

      Z2=Z7
      Z7=Z7-Z1
      Z1=Z1+Z2
      ZD(2+8*I)=Z1
      ZD(6+8*I)=Z7

      DO I=1,7
        Z0=Z(I+8*1)
        Z1=Z(I+8*5)
        Z2=Z(I+8*3)
        Z3=Z(I+8*7)
      
        Z7=Z0
        Z0=Z0-Z1
        Z1=Z1+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        Z7=Z1
        Z1=Z1-Z3
        Z3=Z3+Z7

        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z1=Z1*ZI
        Z2=Z2*ZI*C1P8
        Z0=Z0*C1P8

        Z4=Z(I+8*2)
        Z5=Z(I+8*6)

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7

        Z4=Z4*ZI

        Z7=Z2
        Z2=Z2-Z4
        Z4=Z4+Z7

        ZD(2+8*I)=Z3

        Z6=Z(I+8*0)
        Z7=Z(I+8*4)
        Z3=Z6
        Z6=Z6-Z7
        Z7=Z7+Z3

        Z3=Z6
        Z6=Z6-Z0
        Z0=Z0+Z3

        Z3=Z6
        Z6=Z6-Z2
        Z2=Z2+Z3
        ZD(3+8*I)=Z2*ZT(3+8*I)
        ZD(5+8*I)=Z6*ZT(5+8*I)

        Z2=Z0
        Z0=Z0-Z4
        Z4=Z4+Z2
        ZD(7+8*I)=Z0*ZT(7+8*I)
        ZD(1+8*I)=Z4*ZT(1+8*I)

        Z2=Z7
        Z7=Z7-Z5
        Z5=Z5+Z2

        Z3=ZD(2+8*I)

        Z2=Z5
        Z5=Z5-Z3
        Z3=Z3+Z2
        ZD(4+8*I)=Z5*ZT(4+8*I)
        ZD(0+8*I)=Z3

        Z2=Z7
        Z7=Z7-Z1
        Z1=Z1+Z2
        ZD(2+8*I)=Z1*ZT(2+8*I)
        ZD(6+8*I)=Z7*ZT(6+8*I)
      END DO
*-----------------------------------------------------------------------
      DO I=0,7
        Z0=ZD(I+8*1)
        Z1=ZD(I+8*5)
        Z2=ZD(I+8*3)
        Z3=ZD(I+8*7)
      
        Z7=Z0
        Z0=Z0-Z1
        Z1=Z1+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        Z7=Z1
        Z1=Z1-Z3
        Z3=Z3+Z7

        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z1=Z1*ZI
        Z2=Z2*ZI*C1P8
        Z0=Z0*C1P8

        Z4=ZD(I+8*2)
        Z5=ZD(I+8*6)

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7

        Z4=Z4*ZI

        Z7=Z2
        Z2=Z2-Z4
        Z4=Z4+Z7

        Z(I+8*2)=Z3

        Z6=ZD(I+8*0)
        Z7=ZD(I+8*4)
        Z3=Z6
        Z6=Z6-Z7
        Z7=Z7+Z3

        Z3=Z6
        Z6=Z6-Z0
        Z0=Z0+Z3

        Z3=Z6
        Z6=Z6-Z2
        Z2=Z2+Z3
        Z(I+8*3)=Z2
        Z(I+8*5)=Z6

        Z2=Z0
        Z0=Z0-Z4
        Z4=Z4+Z2
        Z(I+8*7)=Z0
        Z(I+8*1)=Z4

        Z2=Z7
        Z7=Z7-Z5
        Z5=Z5+Z2

        Z3=Z(I+8*2)

        Z2=Z5
        Z5=Z5-Z3
        Z3=Z3+Z2
        Z(I+8*4)=Z5
        Z(I+8*0)=Z3

        Z2=Z7
        Z7=Z7-Z1
        Z1=Z1+Z2
        Z(I+8*2)=Z1
        Z(I+8*6)=Z7
      END DO

*-----------------------------------------------------------------------
* rfft のための後処理
*-----------------------------------------------------------------------

      Z(0)=(1+ZI)*CONJG(Z(0))
      DO K=1,31
        ZTMP=(Z(64-K)-CONJG(Z(K)))*ZT(64+K)
        Z(K)=CONJG(Z(K))+ZTMP
        Z(64-K)=CONJG(Z(64-K)-ZTMP)
      END DO
      Z(32)=CONJG(Z(32))

      END
