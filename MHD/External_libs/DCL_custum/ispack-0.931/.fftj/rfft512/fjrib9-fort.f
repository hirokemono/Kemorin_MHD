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
* rfft 512 in-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJRIB9(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.92387953251128675613D0) ! COS(1*PI/8)      
      PARAMETER(C2P8=0.70710678118654752441D0) ! COS(2*PI/8)
      PARAMETER(C3P8=0.38268343236508977172D0) ! COS(3*PI/8)
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:255),ZD(0:15,0:15),ZDD(0:255)
      DIMENSION ZT(0:383)

*-----------------------------------------------------------------------
* rfft のための前処理
*-----------------------------------------------------------------------

      Z(0)=(1+ZI)*CONJG(Z(0))
      DO K=1,127
        ZTMP1=(Z(K)-CONJG(Z(256-K)))*ZT(256+K)
        ZTMP0=Z(K)+CONJG(Z(256-K))
        Z(K)=ZTMP0+ZTMP1
        Z(256-K)=CONJG(ZTMP0-ZTMP1)
      END DO
      Z(128)=2*CONJG(Z(128))

*-----------------------------------------------------------------------
* fft 256 in-place backward (16x16 type)
*-----------------------------------------------------------------------

      DO I=0,15
        Z0=Z(I+16*0)
        Z1=Z(I+16*4)
        Z2=Z(I+16*8)
        Z3=Z(I+16*12)

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

        Z2=Z(I+16*2)
        Z1=Z(I+16*6)
        Z4=Z(I+16*10)
        Z5=Z(I+16*14)

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

        Z5=Z(I+16*13)
        ZD(13,I)=Z0
        Z4=Z(I+16*9)
        ZD(9,I)=Z2 
        Z0=Z(I+16*1)
        Z3=Z(I+16*5)

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

        Z5=Z(I+16*7)
        Z6=Z(I+16*15)

        Z7=Z5
        Z5=Z5-Z6
        Z6=Z6+Z7
        
        Z7=Z0
        Z0=Z0-Z5
        Z5=Z5+Z7

        ZD(1,I)=Z0 
        
        Z0=Z(I+16*3)
        Z2=Z(I+16*11)      
        
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
      
        Z0=ZD(I,0)
        Z1=ZD(I,4)
        Z2=ZD(I,8)
        Z3=ZD(I,12)

        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z7=Z1
        Z1=Z1-Z3
        Z3=Z3+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        Z(I+16*8)=Z1 
        Z(I+16*4)=Z3 
        Z(I+16*12)=Z2

*-----------------

        Z2=ZD(I,2)
        Z1=ZD(I,6)
        Z4=ZD(I,10)
        Z5=ZD(I,14)

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

        Z(I+16*6)=Z4 
        Z(I+16*0)=Z5 

*-----------------Z1, Z4 使用中

        Z5=ZD(I,13)
        Z(I+16*13)=Z0
        Z4=ZD(I,9)
        Z(I+16*9)=Z2 
        Z0=ZD(I,1)
        Z3=ZD(I,5)

        Z7=Z0
        Z0=Z0-Z4
        Z4=Z4+Z7

        Z7=Z3
        Z3=Z3-Z5
        Z5=Z5+Z7

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7

        Z(I+16*5)=Z5 

        Z5=ZD(I,7)
        Z6=ZD(I,15)

        Z7=Z5
        Z5=Z5-Z6
        Z6=Z6+Z7
        
        Z7=Z0
        Z0=Z0-Z5
        Z5=Z5+Z7

        Z(I+16*1)=Z0 
        
        Z0=ZD(I,3)
        Z2=ZD(I,11)
        
        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z7=Z0
        Z0=Z0-Z3
        Z3=Z3+Z7

        Z(I+16*3)=Z0 

        Z7=Z2
        Z2=Z2-Z6
        Z6=Z6+Z7

        Z(I+16*11)=Z6
        

*---------Z6, Z7, Z0 空き

        Z0=Z(I+16*6)
        Z0=Z0*ZI
        Z6=Z(I+16*12)

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

        Z(I+16*6)=Z2
        Z(I+16*10)=Z6
        Z(I+16*14)=Z4
        Z(I+16*2)=Z0
        
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

        Z0=Z(I+16*1)
        Z3=Z(I+16*3)
        Z4=Z0
        Z6=Z3
        Z0=Z0*C1P8
        Z4=Z4*C3P8
        Z3=Z3*C3P8
        Z6=Z6*C1P8
        Z0=Z0+Z3
        Z4=Z4-Z6

*--   Z3, Z6, Z7 空き

        Z3=Z(I+16*8)
        Z1=Z1*C2P8
        Z7=Z3
        Z3=Z3-Z1
        Z1=Z1+Z7
        Z3=Z3*ZI
        Z1=Z1*ZI

        Z6=Z(I+16*9)
        Z7=Z6
        Z6=Z6-Z0
        Z0=Z0+Z7

        Z7=Z5
        Z5=Z5-Z1
        Z1=Z1+Z7

        Z7=Z6
        Z6=Z6-Z5
        Z5=Z5+Z7
        Z(I+16*9)=Z6
        Z(I+16*7)=Z5

        Z7=Z0
        Z0=Z0-Z1
        Z1=Z1+Z7
        Z(I+16*1)=Z1
        Z(I+16*15)=Z0

!--   Z0,Z1,Z5,Z6,Z7 空き

        Z5=Z(I+16*13)
        Z7=Z5
        Z5=Z5-Z4
        Z4=Z4+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        Z7=Z4
        Z4=Z4-Z2
        Z2=Z2+Z7
        Z(I+16*13)=Z4
        Z(I+16*3)=Z2

        Z7=Z5
        Z5=Z5-Z3
        Z3=Z3+Z7

        Z6=Z(I+16*11) 
        Z(I+16*11)=Z5
        Z5=Z(I+16*5) 
        Z(I+16*5)=Z3

!     Z0,Z1,Z2,Z3,Z4,Z7 空き

        Z7=Z5
        Z5=Z5-Z6
        Z6=Z6+Z7 

        Z5=Z5*ZI 

        Z0=Z(I+16*4)  
        Z1=Z(I+16*0)  

        Z7=Z0
        Z0=Z0-Z1 
        Z1=Z1+Z7 

        Z7=Z1
        Z1=Z1-Z6
        Z6=Z6+Z7
        Z(I+16*0)=Z6
        Z(I+16*8)=Z1

        Z7=Z0
        Z0=Z0-Z5
        Z5=Z5+Z7
        Z(I+16*4)=Z5
        Z(I+16*12)=Z0

      DO I=1,15
        Z0=ZD(I,0)*ZT(0+16*I)
        Z1=ZD(I,4)*ZT(4+16*I)
        Z2=ZD(I,8)*ZT(8+16*I)
        Z3=ZD(I,12)*ZT(12+16*I)

        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z7=Z1
        Z1=Z1-Z3
        Z3=Z3+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        Z(I+16*8)=Z1 
        Z(I+16*4)=Z3 
        Z(I+16*12)=Z2

*-----------------

        Z2=ZD(I,2)*ZT(2+16*I)
        Z1=ZD(I,6)*ZT(6+16*I)
        Z4=ZD(I,10)*ZT(10+16*I)
        Z5=ZD(I,14)*ZT(14+16*I)

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

        Z(I+16*6)=Z4 
        Z(I+16*0)=Z5 

*-----------------Z1, Z4 使用中

        Z5=ZD(I,13)*ZT(13+16*I)
        Z(I+16*13)=Z0
        Z4=ZD(I,9)*ZT(9+16*I)
        Z(I+16*9)=Z2 
        Z0=ZD(I,1)*ZT(1+16*I)
        Z3=ZD(I,5)*ZT(5+16*I)

        Z7=Z0
        Z0=Z0-Z4
        Z4=Z4+Z7

        Z7=Z3
        Z3=Z3-Z5
        Z5=Z5+Z7

        Z7=Z4
        Z4=Z4-Z5
        Z5=Z5+Z7

        Z(I+16*5)=Z5 

        Z5=ZD(I,7)*ZT(7+16*I)
        Z6=ZD(I,15)*ZT(15+16*I)

        Z7=Z5
        Z5=Z5-Z6
        Z6=Z6+Z7
        
        Z7=Z0
        Z0=Z0-Z5
        Z5=Z5+Z7

        Z(I+16*1)=Z0 
        
        Z0=ZD(I,3)*ZT(3+16*I)
        Z2=ZD(I,11)*ZT(11+16*I)
        
        Z7=Z0
        Z0=Z0-Z2
        Z2=Z2+Z7

        Z7=Z0
        Z0=Z0-Z3
        Z3=Z3+Z7

        Z(I+16*3)=Z0 

        Z7=Z2
        Z2=Z2-Z6
        Z6=Z6+Z7

        Z(I+16*11)=Z6
        

*---------Z6, Z7, Z0 空き

        Z0=Z(I+16*6)
        Z0=Z0*ZI
        Z6=Z(I+16*12)

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

        Z(I+16*6)=Z2
        Z(I+16*10)=Z6
        Z(I+16*14)=Z4
        Z(I+16*2)=Z0
        
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

        Z0=Z(I+16*1)
        Z3=Z(I+16*3)
        Z4=Z0
        Z6=Z3
        Z0=Z0*C1P8
        Z4=Z4*C3P8
        Z3=Z3*C3P8
        Z6=Z6*C1P8
        Z0=Z0+Z3
        Z4=Z4-Z6

*--   Z3, Z6, Z7 空き

        Z3=Z(I+16*8)
        Z1=Z1*C2P8
        Z7=Z3
        Z3=Z3-Z1
        Z1=Z1+Z7
        Z3=Z3*ZI
        Z1=Z1*ZI

        Z6=Z(I+16*9)
        Z7=Z6
        Z6=Z6-Z0
        Z0=Z0+Z7

        Z7=Z5
        Z5=Z5-Z1
        Z1=Z1+Z7

        Z7=Z6
        Z6=Z6-Z5
        Z5=Z5+Z7
        Z(I+16*9)=Z6
        Z(I+16*7)=Z5

        Z7=Z0
        Z0=Z0-Z1
        Z1=Z1+Z7
        Z(I+16*1)=Z1
        Z(I+16*15)=Z0

!--   Z0,Z1,Z5,Z6,Z7 空き

        Z5=Z(I+16*13)
        Z7=Z5
        Z5=Z5-Z4
        Z4=Z4+Z7

        Z7=Z2
        Z2=Z2-Z3
        Z3=Z3+Z7

        Z7=Z4
        Z4=Z4-Z2
        Z2=Z2+Z7
        Z(I+16*13)=Z4
        Z(I+16*3)=Z2

        Z7=Z5
        Z5=Z5-Z3
        Z3=Z3+Z7

        Z6=Z(I+16*11) 
        Z(I+16*11)=Z5
        Z5=Z(I+16*5) 
        Z(I+16*5)=Z3

!     Z0,Z1,Z2,Z3,Z4,Z7 空き

        Z7=Z5
        Z5=Z5-Z6
        Z6=Z6+Z7 

        Z5=Z5*ZI 

        Z0=Z(I+16*4)  
        Z1=Z(I+16*0)  

        Z7=Z0
        Z0=Z0-Z1 
        Z1=Z1+Z7 

        Z7=Z1
        Z1=Z1-Z6
        Z6=Z6+Z7
        Z(I+16*0)=Z6
        Z(I+16*8)=Z1

        Z7=Z0
        Z0=Z0-Z5
        Z5=Z5+Z7
        Z(I+16*4)=Z5
        Z(I+16*12)=Z0
      END DO

      END
