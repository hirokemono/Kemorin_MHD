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
* rfft 1024 out-of-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJROBA(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.7071067811865475244D0) ! COS(2*PI/8)
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:511),ZD(0:7,0:7,0:7),ZDD(0:511)
      DIMENSION ZT(0:767)

*-----------------------------------------------------------------------
* rfft のための前処理
*-----------------------------------------------------------------------

      ZDD(0)=(1+ZI)*CONJG(Z(0))
      DO K=1,255
        ZTMP1=(Z(K)-CONJG(Z(512-K)))*ZT(512+K)
        ZTMP0=Z(K)+CONJG(Z(512-K))
        ZDD(K)=ZTMP0+ZTMP1
        ZDD(512-K)=CONJG(ZTMP0-ZTMP1)
      END DO
      ZDD(256)=2*CONJG(Z(256))

*-----------------------------------------------------------------------
* fft 512 out-of-place backward (8x8x8 type)
*-----------------------------------------------------------------------

      DO I=0,7
        DO J=0,7
          Z0=ZDD(I+8*J+64*1)
          Z1=ZDD(I+8*J+64*5)
          Z2=ZDD(I+8*J+64*3)
          Z3=ZDD(I+8*J+64*7)
          
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

          Z4=ZDD(I+8*J+64*2)
          Z5=ZDD(I+8*J+64*6)

          Z7=Z4
          Z4=Z4-Z5
          Z5=Z5+Z7

          Z4=Z4*ZI

          Z7=Z2
          Z2=Z2-Z4
          Z4=Z4+Z7

          ZD(2,J,I)=Z3

          Z6=ZDD(I+8*J+64*0)
          Z7=ZDD(I+8*J+64*4)
          Z3=Z6
          Z6=Z6-Z7
          Z7=Z7+Z3

          Z3=Z6
          Z6=Z6-Z0
          Z0=Z0+Z3

          Z3=Z6
          Z6=Z6-Z2
          Z2=Z2+Z3
          ZD(3,J,I)=Z2
          ZD(5,J,I)=Z6

          Z2=Z0
          Z0=Z0-Z4
          Z4=Z4+Z2
          ZD(7,J,I)=Z0
          ZD(1,J,I)=Z4

          Z2=Z7
          Z7=Z7-Z5
          Z5=Z5+Z2

          Z3=ZD(2,J,I)

          Z2=Z5
          Z5=Z5-Z3
          Z3=Z3+Z2
          ZD(4,J,I)=Z5
          ZD(0,J,I)=Z3

          Z2=Z7
          Z7=Z7-Z1
          Z1=Z1+Z2
          ZD(2,J,I)=Z1
          ZD(6,J,I)=Z7
        END DO
      END DO
*-----------------------------------------------------------------------
      DO J=0,7
        
        I=0
        
          Z0=ZD(I,1,J)
          Z1=ZD(I,5,J)
          Z2=ZD(I,3,J)
          Z3=ZD(I,7,J)
          
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

          Z4=ZD(I,2,J)
          Z5=ZD(I,6,J)

          Z7=Z4
          Z4=Z4-Z5
          Z5=Z5+Z7

          Z4=Z4*ZI

          Z7=Z2
          Z2=Z2-Z4
          Z4=Z4+Z7

          ZD(I,2,J)=Z3

          Z6=ZD(I,0,J)
          Z7=ZD(I,4,J)
          Z3=Z6
          Z6=Z6-Z7
          Z7=Z7+Z3

          Z3=Z6
          Z6=Z6-Z0
          Z0=Z0+Z3

          Z3=Z6
          Z6=Z6-Z2
          Z2=Z2+Z3
          ZD(I,3,J)=Z2
          ZD(I,5,J)=Z6

          Z2=Z0
          Z0=Z0-Z4
          Z4=Z4+Z2
          ZD(I,7,J)=Z0
          ZD(I,1,J)=Z4

          Z2=Z7
          Z7=Z7-Z5
          Z5=Z5+Z2

          Z3=ZD(I,2,J)

          Z2=Z5
          Z5=Z5-Z3
          Z3=Z3+Z2
          ZD(I,4,J)=Z5
          ZD(I,0,J)=Z3

          Z2=Z7
          Z7=Z7-Z1
          Z1=Z1+Z2
          ZD(I,2,J)=Z1
          ZD(I,6,J)=Z7

        DO I=1,7
          Z0=ZD(I,1,J)*ZT(1+I*8)
          Z1=ZD(I,5,J)*ZT(5+I*8)
          Z2=ZD(I,3,J)*ZT(3+I*8)
          Z3=ZD(I,7,J)*ZT(7+I*8)
          
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

          Z4=ZD(I,2,J)*ZT(2+I*8)
          Z5=ZD(I,6,J)*ZT(6+I*8)

          Z7=Z4
          Z4=Z4-Z5
          Z5=Z5+Z7

          Z4=Z4*ZI

          Z7=Z2
          Z2=Z2-Z4
          Z4=Z4+Z7

          ZD(I,2,J)=Z3

          Z6=ZD(I,0,J)
          Z7=ZD(I,4,J)*ZT(4+I*8)
          Z3=Z6
          Z6=Z6-Z7
          Z7=Z7+Z3

          Z3=Z6
          Z6=Z6-Z0
          Z0=Z0+Z3

          Z3=Z6
          Z6=Z6-Z2
          Z2=Z2+Z3
          ZD(I,3,J)=Z2
          ZD(I,5,J)=Z6

          Z2=Z0
          Z0=Z0-Z4
          Z4=Z4+Z2
          ZD(I,7,J)=Z0
          ZD(I,1,J)=Z4

          Z2=Z7
          Z7=Z7-Z5
          Z5=Z5+Z2

          Z3=ZD(I,2,J)

          Z2=Z5
          Z5=Z5-Z3
          Z3=Z3+Z2
          ZD(I,4,J)=Z5
          ZD(I,0,J)=Z3

          Z2=Z7
          Z7=Z7-Z1
          Z1=Z1+Z2
          ZD(I,2,J)=Z1
          ZD(I,6,J)=Z7
        END DO        
      END DO
*-----------------------------------------------------------------------
      DO J=0,7
        DO I=0,7
          Z0=ZD(I,J,1)*ZT(1+7*(I+8*J)+63)
          Z1=ZD(I,J,5)*ZT(5+7*(I+8*J)+63)
          Z2=ZD(I,J,3)*ZT(3+7*(I+8*J)+63)
          Z3=ZD(I,J,7)*ZT(7+7*(I+8*J)+63)
          
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

          Z4=ZD(I,J,2)*ZT(2+7*(I+8*J)+63)
          Z5=ZD(I,J,6)*ZT(6+7*(I+8*J)+63)

          Z7=Z4
          Z4=Z4-Z5
          Z5=Z5+Z7

          Z4=Z4*ZI

          Z7=Z2
          Z2=Z2-Z4
          Z4=Z4+Z7

          ZD(I,J,2)=Z3

          Z6=ZD(I,J,0)
          Z7=ZD(I,J,4)*ZT(4+7*(I+8*J)+63)
          Z3=Z6
          Z6=Z6-Z7
          Z7=Z7+Z3

          Z3=Z6
          Z6=Z6-Z0
          Z0=Z0+Z3

          Z3=Z6
          Z6=Z6-Z2
          Z2=Z2+Z3
          ZD(I,J,3)=Z2
          ZD(I,J,5)=Z6

          Z2=Z0
          Z0=Z0-Z4
          Z4=Z4+Z2
          ZD(I,J,7)=Z0
          ZD(I,J,1)=Z4

          Z2=Z7
          Z7=Z7-Z5
          Z5=Z5+Z2

          Z3=ZD(I,J,2)

          Z2=Z5
          Z5=Z5-Z3
          Z3=Z3+Z2
          ZD(I,J,4)=Z5
          ZD(I,J,0)=Z3

          Z2=Z7
          Z7=Z7-Z1
          Z1=Z1+Z2
          ZD(I,J,2)=Z1
          ZD(I,J,6)=Z7
        END DO
      END DO      

      END
