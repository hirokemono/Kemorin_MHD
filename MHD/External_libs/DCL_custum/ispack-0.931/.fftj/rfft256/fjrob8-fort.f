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
* rfft 256 out-of-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJROB8(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.92387953251128675613D0) ! COS(1*PI/8)      
      PARAMETER(C2P8=0.70710678118654752441D0) ! COS(2*PI/8)
      PARAMETER(C3P8=0.38268343236508977172D0) ! COS(3*PI/8)
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:127),ZD(0:15,0:7),ZDD(0:127)
      DIMENSION ZT(0:191)

*-----------------------------------------------------------------------
* rfft のための前処理
*-----------------------------------------------------------------------

      ZDD(0)=(1+ZI)*CONJG(Z(0))
      DO K=1,63
        ZTMP1=(Z(K)-CONJG(Z(128-K)))*ZT(128+K)
        ZTMP0=Z(K)+CONJG(Z(128-K))
        ZDD(K)=ZTMP0+ZTMP1
        ZDD(128-K)=CONJG(ZTMP0-ZTMP1)
      END DO
      ZDD(64)=2*CONJG(Z(64))

*-----------------------------------------------------------------------

      DO I=0,7
        Z0=ZDD(I+0*8)
        Z1=ZDD(I+4*8)
        Z2=ZDD(I+8*8)
        Z3=ZDD(I+12*8)

        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z7=Z1
        Z1=Z1-Z3
        Z3=Z3+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        ZD(8,I)=Z1 
        ZD(4,I)=Z3 
        ZD(12,I)=Z2

*-----------------

        Z2=ZDD(I+2*8)
        Z1=ZDD(I+6*8)
        Z4=ZDD(I+10*8)
        Z5=ZDD(I+14*8)

        Z7=Z2
        Z2=Z2-Z4
        Z4=Z4+Z7

        Z7=Z1
        Z1=Z1-Z5
        Z5=Z5+Z7

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7
        
        Z7=Z2
        Z2=Z2-Z1
        Z1=Z1+Z7
        
        Z2=Z2*C2P8
        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        ZD(6,I)=Z4 
        ZD(0,I)=Z5 

*-----------------Z1, Z4 使用中

        Z5=ZDD(I+13*8)
        ZD(13,I)=Z0
        Z4=ZDD(I+9*8)
        ZD(9,I)=Z2 
        Z0=ZDD(I+1*8)
        Z3=ZDD(I+5*8)

        Z7=Z0
        Z0=Z0-Z4
        Z4=Z4+Z7

        Z7=Z3
        Z3=Z3-Z5
        Z5=Z5+Z7

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7

        ZD(5,I)=Z5 

        Z5=ZDD(I+7*8)
        Z6=ZDD(I+15*8)

        Z7=Z5
        Z5=Z5-Z6
        Z6=Z6+Z7
        
        Z7=Z0
        Z0=Z0-Z5
        Z5=Z5+Z7

        ZD(1,I)=Z0 
        
        Z0=ZDD(I+3*8)
        Z2=ZDD(I+11*8)      
        
        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z7=Z0
        Z0=Z0-Z3
        Z3=Z3+Z7

        ZD(3,I)=Z0 

        Z7=Z2
        Z2=Z2-Z6
        Z6=Z6+Z7

        ZD(11,I)=Z6
        

*---------Z6, Z7, Z0 空き

        Z0=ZD(6,I)
        Z0=Z0*ZI
        Z6=ZD(12,I)

        Z7=Z4
        Z4=Z4-Z2
        Z2=Z2+Z7 

        Z4=Z4*C2P8
        Z2=Z2*(C2P8*ZI)

        Z7=Z6
        Z6=Z6-Z4 
        Z4=Z4+Z7

        Z7=Z2
        Z2=Z2-Z0 
        Z0=Z0+Z7 

        Z7=Z6
        Z6=Z6-Z2
        Z2=Z2+Z7

        Z7=Z4
        Z4=Z4-Z0
        Z0=Z0+Z7

        ZD(6,I)=Z2
        ZD(10,I)=Z6
        ZD(14,I)=Z4
        ZD(2,I)=Z0
        
*---- Z0, Z2, Z4, Z6, Z7 空き      


        Z2=Z5
        Z5=Z5*C3P8
        Z2=Z2*C1P8

        Z0=Z3
        Z3=Z3*C1P8
        Z0=Z0*C3P8      

        Z5=Z5+Z3
        Z2=Z2-Z0
        Z5=Z5*ZI
        Z2=Z2*ZI 

*--   Z0, Z3, Z4, Z6, Z7 空き      

        Z0=ZD(1,I)
        Z3=ZD(3,I)
        Z4=Z0
        Z6=Z3
        Z0=Z0*C1P8
        Z4=Z4*C3P8
        Z3=Z3*C3P8
        Z6=Z6*C1P8
        Z0=Z0+Z3
        Z4=Z4-Z6

*--   Z3, Z6, Z7 空き

        Z3=ZD(8,I)
        Z1=Z1*C2P8
        Z7=Z3
        Z3=Z3-Z1
        Z1=Z1+Z7
        Z3=Z3*ZI
        Z1=Z1*ZI

        Z6=ZD(9,I)
        Z7=Z6
        Z6=Z6-Z0
        Z0=Z0+Z7

        Z7=Z5
        Z5=Z5-Z1
        Z1=Z1+Z7

        Z7=Z6
        Z6=Z6-Z5
        Z5=Z5+Z7
        ZD(9,I)=Z6
        ZD(7,I)=Z5

        Z7=Z0
        Z0=Z0-Z1
        Z1=Z1+Z7
        ZD(1,I)=Z1
        ZD(15,I)=Z0

!--   Z0,Z1,Z5,Z6,Z7 空き

        Z5=ZD(13,I)
        Z7=Z5
        Z5=Z5-Z4
        Z4=Z4+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        Z7=Z4
        Z4=Z4-Z2
        Z2=Z2+Z7
        ZD(13,I)=Z4
        ZD(3,I)=Z2

        Z7=Z5
        Z5=Z5-Z3
        Z3=Z3+Z7

        Z6=ZD(11,I) 
        ZD(11,I)=Z5
        Z5=ZD(5,I) 
        ZD(5,I)=Z3

!     Z0,Z1,Z2,Z3,Z4,Z7 空き

        Z7=Z5
        Z5=Z5-Z6
        Z6=Z6+Z7 

        Z5=Z5*ZI 

        Z0=ZD(4,I)  
        Z1=ZD(0,I)  

        Z7=Z0
        Z0=Z0-Z1 
        Z1=Z1+Z7 

        Z7=Z1
        Z1=Z1-Z6
        Z6=Z6+Z7
        ZD(0,I)=Z6
        ZD(8,I)=Z1

        Z7=Z0
        Z0=Z0-Z5
        Z5=Z5+Z7
        ZD(4,I)=Z5
        ZD(12,I)=Z0
      END DO
*-----------------------------------------------------------------------
      I=0

        Z0=ZD(I,1)
        Z1=ZD(I,5)
        Z2=ZD(I,3)
        Z3=ZD(I,7)
      
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
        Z2=Z2*ZI*C2P8
        Z0=Z0*C2P8

        Z4=ZD(I,2)
        Z5=ZD(I,6)

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7

        Z4=Z4*ZI

        Z7=Z2
        Z2=Z2-Z4
        Z4=Z4+Z7

        ZD(I,2)=Z3

        Z6=ZD(I,0)
        Z7=ZD(I,4)
        Z3=Z6
        Z6=Z6-Z7
        Z7=Z7+Z3

        Z3=Z6
        Z6=Z6-Z0
        Z0=Z0+Z3

        Z3=Z6
        Z6=Z6-Z2
        Z2=Z2+Z3
        ZD(I,3)=Z2
        ZD(I,5)=Z6

        Z2=Z0
        Z0=Z0-Z4
        Z4=Z4+Z2
        ZD(I,7)=Z0
        ZD(I,1)=Z4

        Z2=Z7
        Z7=Z7-Z5
        Z5=Z5+Z2

        Z3=ZD(I,2)

        Z2=Z5
        Z5=Z5-Z3
        Z3=Z3+Z2
        ZD(I,4)=Z5
        ZD(I,0)=Z3

        Z2=Z7
        Z7=Z7-Z1
        Z1=Z1+Z2
        ZD(I,2)=Z1
        ZD(I,6)=Z7

      DO I=1,15
        Z0=ZD(I,1)*ZT(1+8*I)
        Z1=ZD(I,5)*ZT(5+8*I)
        Z2=ZD(I,3)*ZT(3+8*I)
        Z3=ZD(I,7)*ZT(7+8*I)
      
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
        Z2=Z2*ZI*C2P8
        Z0=Z0*C2P8

        Z4=ZD(I,2)*ZT(2+8*I)
        Z5=ZD(I,6)*ZT(6+8*I)

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7

        Z4=Z4*ZI

        Z7=Z2
        Z2=Z2-Z4
        Z4=Z4+Z7

        ZD(I,2)=Z3

        Z6=ZD(I,0)
        Z7=ZD(I,4)*ZT(4+8*I)
        Z3=Z6
        Z6=Z6-Z7
        Z7=Z7+Z3

        Z3=Z6
        Z6=Z6-Z0
        Z0=Z0+Z3

        Z3=Z6
        Z6=Z6-Z2
        Z2=Z2+Z3
        ZD(I,3)=Z2
        ZD(I,5)=Z6

        Z2=Z0
        Z0=Z0-Z4
        Z4=Z4+Z2
        ZD(I,7)=Z0
        ZD(I,1)=Z4

        Z2=Z7
        Z7=Z7-Z5
        Z5=Z5+Z2

        Z3=ZD(I,2)

        Z2=Z5
        Z5=Z5-Z3
        Z3=Z3+Z2
        ZD(I,4)=Z5
        ZD(I,0)=Z3

        Z2=Z7
        Z7=Z7-Z1
        Z1=Z1+Z2
        ZD(I,2)=Z1
        ZD(I,6)=Z7
      END DO

      END
      
      
