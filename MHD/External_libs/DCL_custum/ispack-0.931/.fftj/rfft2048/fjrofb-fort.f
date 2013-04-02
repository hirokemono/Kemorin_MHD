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
* rfft 2048 out-of-place forward
*-----------------------------------------------------------------------
      SUBROUTINE FJROFB(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.92387953251128675613D0) ! COS(1*PI/8)      
      PARAMETER(C2P8=0.70710678118654752441D0) ! COS(2*PI/8)
      PARAMETER(C3P8=0.38268343236508977172D0) ! COS(3*PI/8)
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:1023),ZD(0:1023),ZDD(0:1023)
      DIMENSION ZT(0:1535)

*-----------------------------------------------------------------------
* fft 1024 out-of-place backward (16x16x4 type)
*-----------------------------------------------------------------------

      DO J=0,15        
        DO I=0,3
          Z0=Z(I+4*J+64*0)
          Z1=Z(I+4*J+64*4)
          Z2=Z(I+4*J+64*8)
          Z3=Z(I+4*J+64*12)

          Z7=Z0
          Z0=Z0-Z2
          Z2=Z2+Z7

          Z7=Z1
          Z1=Z1-Z3
          Z3=Z3+Z7

          Z7=Z2
          Z2=Z2-Z3
          Z3=Z3+Z7

          ZD(8+16*J+256*I)=Z1 
          ZD(4+16*J+256*I)=Z3 
          ZD(12+16*J+256*I)=Z2

*-----------------

          Z2=Z(I+4*J+64*2)
          Z1=Z(I+4*J+64*6)
          Z4=Z(I+4*J+64*10)
          Z5=Z(I+4*J+64*14)

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

          ZD(6+16*J+256*I)=Z4 
          ZD(0+16*J+256*I)=Z5 

*-----------------Z1, Z4 使用中

          Z5=Z(I+4*J+64*13)
          ZD(13+16*J+256*I)=Z0
          Z4=Z(I+4*J+64*9)
          ZD(9+16*J+256*I)=Z2 
          Z0=Z(I+4*J+64*1)
          Z3=Z(I+4*J+64*5)

          Z7=Z0
          Z0=Z0-Z4
          Z4=Z4+Z7

          Z7=Z3
          Z3=Z3-Z5
          Z5=Z5+Z7

          Z7=Z4
          Z4=Z4-Z5
          Z5=Z5+Z7

          ZD(5+16*J+256*I)=Z5 

          Z5=Z(I+4*J+64*7)
          Z6=Z(I+4*J+64*15)

          Z7=Z5
          Z5=Z5-Z6
          Z6=Z6+Z7
          
          Z7=Z0
          Z0=Z0-Z5
          Z5=Z5+Z7

          ZD(1+16*J+256*I)=Z0 
          
          Z0=Z(I+4*J+64*3)
          Z2=Z(I+4*J+64*11)      
          
          Z7=Z0
          Z0=Z0-Z2
          Z2=Z2+Z7

          Z7=Z0
          Z0=Z0-Z3
          Z3=Z3+Z7

          ZD(3+16*J+256*I)=Z0 

          Z7=Z2
          Z2=Z2-Z6
          Z6=Z6+Z7

          ZD(11+16*J+256*I)=Z6
          

*---------Z6, Z7, Z0 空き

          Z0=ZD(6+16*J+256*I)
          Z0=Z0*ZI
          Z6=ZD(12+16*J+256*I)

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

          ZD(6+16*J+256*I)=Z2*ZT(6+(I+4*J)*15+63)
          ZD(10+16*J+256*I)=Z6*ZT(10+(I+4*J)*15+63)
          ZD(14+16*J+256*I)=Z4*ZT(14+(I+4*J)*15+63)
          ZD(2+16*J+256*I)=Z0*ZT(2+(I+4*J)*15+63)
          
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

          Z0=ZD(1+16*J+256*I)
          Z3=ZD(3+16*J+256*I)
          Z4=Z0
          Z6=Z3
          Z0=Z0*C1P8
          Z4=Z4*C3P8
          Z3=Z3*C3P8
          Z6=Z6*C1P8
          Z0=Z0+Z3
          Z4=Z4-Z6

*--   Z3, Z6, Z7 空き

          Z3=ZD(8+16*J+256*I)
          Z1=Z1*C2P8
          Z7=Z3
          Z3=Z3-Z1
          Z1=Z1+Z7
          Z3=Z3*ZI
          Z1=Z1*ZI

          Z6=ZD(9+16*J+256*I)
          Z7=Z6
          Z6=Z6-Z0
          Z0=Z0+Z7

          Z7=Z5
          Z5=Z5-Z1
          Z1=Z1+Z7

          Z7=Z6
          Z6=Z6-Z5
          Z5=Z5+Z7
          ZD(9+16*J+256*I)=Z6*ZT(9+(I+4*J)*15+63)
          ZD(7+16*J+256*I)=Z5*ZT(7+(I+4*J)*15+63)

          Z7=Z0
          Z0=Z0-Z1
          Z1=Z1+Z7
          ZD(1+16*J+256*I)=Z1*ZT(1+(I+4*J)*15+63)
          ZD(15+16*J+256*I)=Z0*ZT(15+(I+4*J)*15+63)

!--   Z0,Z1,Z5,Z6,Z7 空き

          Z5=ZD(13+16*J+256*I)
          Z7=Z5
          Z5=Z5-Z4
          Z4=Z4+Z7

          Z7=Z2
          Z2=Z2-Z3
          Z3=Z3+Z7

          Z7=Z4
          Z4=Z4-Z2
          Z2=Z2+Z7
          ZD(13+16*J+256*I)=Z4*ZT(13+(I+4*J)*15+63)
          ZD(3+16*J+256*I)=Z2*ZT(3+(I+4*J)*15+63)

          Z7=Z5
          Z5=Z5-Z3
          Z3=Z3+Z7

          Z6=ZD(11+16*J+256*I) 
          ZD(11+16*J+256*I)=Z5*ZT(11+(I+4*J)*15+63)
          Z5=ZD(5+16*J+256*I) 
          ZD(5+16*J+256*I)=Z3*ZT(5+(I+4*J)*15+63)

!     Z0,Z1,Z2,Z3,Z4,Z7 空き

          Z7=Z5
          Z5=Z5-Z6
          Z6=Z6+Z7 

          Z5=Z5*ZI 

          Z0=ZD(4+16*J+256*I)  
          Z1=ZD(0+16*J+256*I)  

          Z7=Z0
          Z0=Z0-Z1 
          Z1=Z1+Z7 

          Z7=Z1
          Z1=Z1-Z6
          Z6=Z6+Z7
          ZD(0+16*J+256*I)=Z6
          ZD(8+16*J+256*I)=Z1*ZT(8+(I+4*J)*15+63)

          Z7=Z0
          Z0=Z0-Z5
          Z5=Z5+Z7
          ZD(4+16*J+256*I)=Z5*ZT(4+(I+4*J)*15+63)
          ZD(12+16*J+256*I)=Z0*ZT(12+(I+4*J)*15+63)
        END DO
      END DO
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
      I=0
        DO J=0,15
          Z0=ZD(J+16*0+256*I)
          Z1=ZD(J+16*4+256*I)
          Z2=ZD(J+16*8+256*I)
          Z3=ZD(J+16*12+256*I)

          Z7=Z0
          Z0=Z0-Z2
          Z2=Z2+Z7

          Z7=Z1
          Z1=Z1-Z3
          Z3=Z3+Z7

          Z7=Z2
          Z2=Z2-Z3
          Z3=Z3+Z7

          ZD(J+16*8+256*I)=Z1 
          ZD(J+16*4+256*I)=Z3 
          ZD(J+16*12+256*I)=Z2

*-----------------

          Z2=ZD(J+16*2+256*I)
          Z1=ZD(J+16*6+256*I)
          Z4=ZD(J+16*10+256*I)
          Z5=ZD(J+16*14+256*I)

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

          ZD(J+16*6+256*I)=Z4 
          ZD(J+16*0+256*I)=Z5 

*-----------------Z1, Z4 使用中

          Z5=ZD(J+16*13+256*I)
          ZD(J+16*13+256*I)=Z0
          Z4=ZD(J+16*9+256*I)
          ZD(J+16*9+256*I)=Z2 
          Z0=ZD(J+16*1+256*I)
          Z3=ZD(J+16*5+256*I)

          Z7=Z0
          Z0=Z0-Z4
          Z4=Z4+Z7

          Z7=Z3
          Z3=Z3-Z5
          Z5=Z5+Z7

          Z7=Z4
          Z4=Z4-Z5
          Z5=Z5+Z7

          ZD(J+16*5+256*I)=Z5 

          Z5=ZD(J+16*7+256*I)
          Z6=ZD(J+16*15+256*I)

          Z7=Z5
          Z5=Z5-Z6
          Z6=Z6+Z7
          
          Z7=Z0
          Z0=Z0-Z5
          Z5=Z5+Z7

          ZD(J+16*1+256*I)=Z0 
          
          Z0=ZD(J+16*3+256*I)
          Z2=ZD(J+16*11+256*I)
          
          Z7=Z0
          Z0=Z0-Z2
          Z2=Z2+Z7

          Z7=Z0
          Z0=Z0-Z3
          Z3=Z3+Z7

          ZD(J+16*3+256*I)=Z0 

          Z7=Z2
          Z2=Z2-Z6
          Z6=Z6+Z7

          ZD(J+16*11+256*I)=Z6
          

*---------Z6, Z7, Z0 空き

          Z0=ZD(J+16*6+256*I)
          Z0=Z0*ZI
          Z6=ZD(J+16*12+256*I)

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

          ZD(J+16*6+256*I)=Z2
          ZD(J+16*10+256*I)=Z6
          ZD(J+16*14+256*I)=Z4
          ZD(J+16*2+256*I)=Z0
          
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

          Z0=ZD(J+16*1+256*I)
          Z3=ZD(J+16*3+256*I)
          Z4=Z0
          Z6=Z3
          Z0=Z0*C1P8
          Z4=Z4*C3P8
          Z3=Z3*C3P8
          Z6=Z6*C1P8
          Z0=Z0+Z3
          Z4=Z4-Z6

*--   Z3, Z6, Z7 空き

          Z3=ZD(J+16*8+256*I)
          Z1=Z1*C2P8
          Z7=Z3
          Z3=Z3-Z1
          Z1=Z1+Z7
          Z3=Z3*ZI
          Z1=Z1*ZI

          Z6=ZD(J+16*9+256*I)
          Z7=Z6
          Z6=Z6-Z0
          Z0=Z0+Z7

          Z7=Z5
          Z5=Z5-Z1
          Z1=Z1+Z7

          Z7=Z6
          Z6=Z6-Z5
          Z5=Z5+Z7
          ZD(J+16*9+256*I)=Z6
          ZD(J+16*7+256*I)=Z5

          Z7=Z0
          Z0=Z0-Z1
          Z1=Z1+Z7
          ZD(J+16*1+256*I)=Z1
          ZD(J+16*15+256*I)=Z0

!--   Z0,Z1,Z5,Z6,Z7 空き

          Z5=ZD(J+16*13+256*I)
          Z7=Z5
          Z5=Z5-Z4
          Z4=Z4+Z7

          Z7=Z2
          Z2=Z2-Z3
          Z3=Z3+Z7

          Z7=Z4
          Z4=Z4-Z2
          Z2=Z2+Z7
          ZD(J+16*13+256*I)=Z4
          ZD(J+16*3+256*I)=Z2

          Z7=Z5
          Z5=Z5-Z3
          Z3=Z3+Z7

          Z6=ZD(J+16*11+256*I) 
          ZD(J+16*11+256*I)=Z5
          Z5=ZD(J+16*5+256*I) 
          ZD(J+16*5+256*I)=Z3

!     Z0,Z1,Z2,Z3,Z4,Z7 空き

          Z7=Z5
          Z5=Z5-Z6
          Z6=Z6+Z7 

          Z5=Z5*ZI 

          Z0=ZD(J+16*4+256*I)  
          Z1=ZD(J+16*0+256*I)  

          Z7=Z0
          Z0=Z0-Z1 
          Z1=Z1+Z7 

          Z7=Z1
          Z1=Z1-Z6
          Z6=Z6+Z7
          ZD(J+16*0+256*I)=Z6
          ZD(J+16*8+256*I)=Z1

          Z7=Z0
          Z0=Z0-Z5
          Z5=Z5+Z7
          ZD(J+16*4+256*I)=Z5
          ZD(J+16*12+256*I)=Z0
        END DO
*-------------------------------
      DO I=1,3
        DO J=0,15
          Z0=ZD(J+16*0+256*I)
          Z1=ZD(J+16*4+256*I)
          Z2=ZD(J+16*8+256*I)
          Z3=ZD(J+16*12+256*I)

          Z7=Z0
          Z0=Z0-Z2
          Z2=Z2+Z7

          Z7=Z1
          Z1=Z1-Z3
          Z3=Z3+Z7

          Z7=Z2
          Z2=Z2-Z3
          Z3=Z3+Z7

          ZD(J+16*8+256*I)=Z1 
          ZD(J+16*4+256*I)=Z3 
          ZD(J+16*12+256*I)=Z2

*-----------------

          Z2=ZD(J+16*2+256*I)
          Z1=ZD(J+16*6+256*I)
          Z4=ZD(J+16*10+256*I)
          Z5=ZD(J+16*14+256*I)

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

          ZD(J+16*6+256*I)=Z4 
          ZD(J+16*0+256*I)=Z5 

*-----------------Z1, Z4 使用中

          Z5=ZD(J+16*13+256*I)
          ZD(J+16*13+256*I)=Z0
          Z4=ZD(J+16*9+256*I)
          ZD(J+16*9+256*I)=Z2 
          Z0=ZD(J+16*1+256*I)
          Z3=ZD(J+16*5+256*I)

          Z7=Z0
          Z0=Z0-Z4
          Z4=Z4+Z7

          Z7=Z3
          Z3=Z3-Z5
          Z5=Z5+Z7

          Z7=Z4
          Z4=Z4-Z5
          Z5=Z5+Z7

          ZD(J+16*5+256*I)=Z5 

          Z5=ZD(J+16*7+256*I)
          Z6=ZD(J+16*15+256*I)

          Z7=Z5
          Z5=Z5-Z6
          Z6=Z6+Z7
          
          Z7=Z0
          Z0=Z0-Z5
          Z5=Z5+Z7

          ZD(J+16*1+256*I)=Z0 
          
          Z0=ZD(J+16*3+256*I)
          Z2=ZD(J+16*11+256*I)
          
          Z7=Z0
          Z0=Z0-Z2
          Z2=Z2+Z7

          Z7=Z0
          Z0=Z0-Z3
          Z3=Z3+Z7

          ZD(J+16*3+256*I)=Z0 

          Z7=Z2
          Z2=Z2-Z6
          Z6=Z6+Z7

          ZD(J+16*11+256*I)=Z6
          

*---------Z6, Z7, Z0 空き

          Z0=ZD(J+16*6+256*I)
          Z0=Z0*ZI
          Z6=ZD(J+16*12+256*I)

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

          ZD(J+16*6+256*I)=Z2*ZT(6+16*I)
          ZD(J+16*10+256*I)=Z6*ZT(10+16*I)
          ZD(J+16*14+256*I)=Z4*ZT(14+16*I)
          ZD(J+16*2+256*I)=Z0*ZT(2+16*I)
          
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

          Z0=ZD(J+16*1+256*I)
          Z3=ZD(J+16*3+256*I)
          Z4=Z0
          Z6=Z3
          Z0=Z0*C1P8
          Z4=Z4*C3P8
          Z3=Z3*C3P8
          Z6=Z6*C1P8
          Z0=Z0+Z3
          Z4=Z4-Z6

*--   Z3, Z6, Z7 空き

          Z3=ZD(J+16*8+256*I)
          Z1=Z1*C2P8
          Z7=Z3
          Z3=Z3-Z1
          Z1=Z1+Z7
          Z3=Z3*ZI
          Z1=Z1*ZI

          Z6=ZD(J+16*9+256*I)
          Z7=Z6
          Z6=Z6-Z0
          Z0=Z0+Z7

          Z7=Z5
          Z5=Z5-Z1
          Z1=Z1+Z7

          Z7=Z6
          Z6=Z6-Z5
          Z5=Z5+Z7
          ZD(J+16*9+256*I)=Z6*ZT(9+16*I)
          ZD(J+16*7+256*I)=Z5*ZT(7+16*I)

          Z7=Z0
          Z0=Z0-Z1
          Z1=Z1+Z7
          ZD(J+16*1+256*I)=Z1*ZT(1+16*I)
          ZD(J+16*15+256*I)=Z0*ZT(15+16*I)

!--   Z0,Z1,Z5,Z6,Z7 空き

          Z5=ZD(J+16*13+256*I)
          Z7=Z5
          Z5=Z5-Z4
          Z4=Z4+Z7

          Z7=Z2
          Z2=Z2-Z3
          Z3=Z3+Z7

          Z7=Z4
          Z4=Z4-Z2
          Z2=Z2+Z7
          ZD(J+16*13+256*I)=Z4*ZT(13+16*I)
          ZD(J+16*3+256*I)=Z2*ZT(3+16*I)

          Z7=Z5
          Z5=Z5-Z3
          Z3=Z3+Z7

          Z6=ZD(J+16*11+256*I) 
          ZD(J+16*11+256*I)=Z5*ZT(11+16*I)
          Z5=ZD(J+16*5+256*I) 
          ZD(J+16*5+256*I)=Z3*ZT(5+16*I)

!     Z0,Z1,Z2,Z3,Z4,Z7 空き

          Z7=Z5
          Z5=Z5-Z6
          Z6=Z6+Z7 

          Z5=Z5*ZI 

          Z0=ZD(J+16*4+256*I)  
          Z1=ZD(J+16*0+256*I)  

          Z7=Z0
          Z0=Z0-Z1 
          Z1=Z1+Z7 

          Z7=Z1
          Z1=Z1-Z6
          Z6=Z6+Z7
          ZD(J+16*0+256*I)=Z6
          ZD(J+16*8+256*I)=Z1*ZT(8+16*I)

          Z7=Z0
          Z0=Z0-Z5
          Z5=Z5+Z7
          ZD(J+16*4+256*I)=Z5*ZT(4+16*I)
          ZD(J+16*12+256*I)=Z0*ZT(12+16*I)
        END DO
      END DO
*-----------------------------------------------------------------------
      DO I=0,255
        Z0=ZD(I+256*0)
        Z1=ZD(I+256*1)
        Z2=ZD(I+256*2)
        Z3=ZD(I+256*3)                

        Z4=Z0
        Z0=Z0-Z2
        Z2=Z2+Z4

        Z4=Z1
        Z1=Z1-Z3
        Z3=Z3+Z4

        Z1=Z1*ZI

        Z4=Z2
        Z2=Z2+Z3
        Z4=Z4-Z3

        Z5=Z0
        Z0=Z0+Z1
        Z5=Z5-Z1

        ZD(I+256*0)=Z2
        ZD(I+256*1)=Z0
        ZD(I+256*2)=Z4
        ZD(I+256*3)=Z5
      END DO

*-----------------------------------------------------------------------
* rfft のための後処理
*-----------------------------------------------------------------------

      ZD(0)=(1+ZI)*CONJG(ZD(0))
      DO K=1,511
        ZTMP=(ZD(1024-K)-CONJG(ZD(K)))*ZT(1024+K)
        ZD(K)=CONJG(ZD(K))+ZTMP
        ZD(1024-K)=CONJG(ZD(1024-K)-ZTMP)
      END DO
      ZD(512)=CONJG(ZD(512))

      END
