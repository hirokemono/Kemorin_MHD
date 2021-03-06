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
* fft 16 out-of-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJCOB4(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.92387953251128675613D0) ! COS(1*PI/8)
      PARAMETER(C2P8=0.70710678118654752441D0) ! COS(2*PI/8)
      PARAMETER(C3P8=0.38268343236508977172D0) ! COS(3*PI/8)
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:15),ZD(0:15),ZDD(0:15)
      DIMENSION ZT(2*16)

      Z0=Z(0)
      Z1=Z(4)
      Z2=Z(8)
      Z3=Z(12)

      Z7=Z0
      Z0=Z0-Z2  ! Z(0)-Z(8)
      Z2=Z2+Z7  ! Z(0)+Z(8)

      Z7=Z1
      Z1=Z1-Z3  ! Z(4)-Z(12)
      Z3=Z3+Z7  ! Z(4)+Z(12)

      Z7=Z2
      Z2=Z2-Z3  ! (Z(0)+Z(8))-(Z(4)+Z(12))
      Z3=Z3+Z7  ! (Z(0)+Z(8))+(Z(4)+Z(12))

      ZD(8)=Z1   ! Z(4)-Z(12)
      ZD(4)=Z3   ! (Z(0)+Z(8))+(Z(4)+Z(12))
      ZD(12)=Z2   ! (Z(0)+Z(8))-(Z(4)+Z(12))      

*-----------------

      Z2=Z(2)
      Z1=Z(6)
      Z4=Z(10)
      Z5=Z(14)

      Z7=Z2
      Z2=Z2-Z4  ! Z(2)-Z(10)
      Z4=Z4+Z7  ! Z(2)+Z(10)

      Z7=Z1
      Z1=Z1-Z5  ! Z(6)-Z(14)
      Z5=Z5+Z7  ! Z(6)+Z(14)

      Z7=Z4
      Z4=Z4-Z5  ! (Z(2)+Z(10))-(Z(6)+Z(14)) 
      Z5=Z5+Z7  ! (Z(2)+Z(10))+(Z(6)+Z(14))
      
      Z7=Z2
      Z2=Z2-Z1  ! (Z(2)-Z(10))-(Z(6)-Z(14))
      Z1=Z1+Z7  ! (Z(2)-Z(10))+(Z(6)-Z(14))  これは退避しない
      
      Z2=Z2*C2P8
      Z7=Z0
      Z0=Z0-Z2 ! Z(0)-Z(8)-C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))
      Z2=Z2+Z7 ! Z(0)-Z(8)+C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))

      ZD(6)=Z4   ! (Z(2)+Z(10))-(Z(6)+Z(14))
      ZD(0)=Z5  ! (Z(2)+Z(10))+(Z(6)+Z(14))

*-----------------  Z1, Z4 使用中

      Z5=Z(13)
      ZD(13)=Z0 ! Z(0)-Z(8)-C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))            
      Z4=Z(9)
      ZD(9)=Z2 ! Z(0)-Z(8)+C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))
      Z0=Z(1)
      Z3=Z(5)

      Z7=Z0
      Z0=Z0-Z4  ! Z(1)-Z(9)
      Z4=Z4+Z7  ! Z(1)+Z(9)

      Z7=Z3
      Z3=Z3-Z5  ! Z(5)-Z(13)
      Z5=Z5+Z7  ! Z(5)+Z(13)

      Z7=Z4
      Z4=Z4-Z5  ! (Z(1)+Z(9))-(Z(5)+Z(13))
      Z5=Z5+Z7  ! (Z(1)+Z(9))+(Z(5)+Z(13))

      ZD(5)=Z5   ! (Z(1)+Z(9))+(Z(5)+Z(13))

      Z5=Z(7)
      Z6=Z(15)

      Z7=Z5
      Z5=Z5-Z6  ! Z(7)-Z(15)
      Z6=Z6+Z7  ! Z(7)+Z(15)
      
      Z7=Z0
      Z0=Z0-Z5  ! (Z(1)-Z(9))-(Z(7)-Z(15))
      Z5=Z5+Z7  ! (Z(1)-Z(9))+(Z(7)-Z(15))

      ZD(1)=Z0   ! (Z(1)-Z(9))-(Z(7)-Z(15))
      
      Z0=Z(3)
      Z2=Z(11)      
      
      Z7=Z0
      Z0=Z0-Z2  ! Z(3)-Z(11)
      Z2=Z2+Z7  ! Z(3)+Z(11)

      Z7=Z0
      Z0=Z0-Z3  ! (Z(3)-Z(11))-(Z(5)-Z(13))
      Z3=Z3+Z7  ! (Z(3)-Z(11))+(Z(5)-Z(13))

      ZD(3)=Z0 ! (Z(3)-Z(11))-(Z(5)-Z(13))

      Z7=Z2
      Z2=Z2-Z6  ! (Z(3)+Z(11))-(Z(7)+Z(15))
      Z6=Z6+Z7  ! (Z(3)+Z(11))+(Z(7)+Z(15))

      ZD(11)=Z6 ! (Z(3)+Z(11))+(Z(7)+Z(15))
      

*--------- Z6, Z7, Z0 空き

      Z0=ZD(6) ! (Z(2)+Z(10))-(Z(6)+Z(14))
      Z0=Z0*ZI  ! i((Z(2)+Z(10))-(Z(6)+Z(14)))
      Z6=ZD(12) ! (Z(0)+Z(8))-(Z(4)+Z(12))      

!      Z4 ! (Z(1)+Z(9))-(Z(5)+Z(13))
!      Z2 ! (Z(3)+Z(11))-(Z(7)+Z(15))      
      Z7=Z4
      Z4=Z4-Z2 ! (Z(1)+Z(9))-(Z(5)+Z(13))-((Z(3)+Z(11))-(Z(7)+Z(15)))
      Z2=Z2+Z7 ! (Z(1)+Z(9))-(Z(5)+Z(13))+((Z(3)+Z(11))-(Z(7)+Z(15)))

      Z4=Z4*C2P8
      Z2=Z2*(C2P8*ZI)

      Z7=Z6
      Z6=Z6-Z4 ! (Z(0)+Z(8))-(Z(4)+Z(12))
      ! -C2P8*(Z(1)+Z(9))-(Z(5)+Z(13))-((Z(3)+Z(11))-(Z(7)+Z(15)))      
      Z4=Z4+Z7 ! (Z(0)+Z(8))-(Z(4)+Z(12))
      ! +C2P8*(Z(1)+Z(9))-(Z(5)+Z(13))-((Z(3)+Z(11))-(Z(7)+Z(15)))

      Z7=Z2
      Z2=Z2-Z0 ! -i((Z(2)+Z(10))-(Z(6)+Z(14)))
      ! + i C2P8*((Z(1)+Z(9))-(Z(5)+Z(13))+((Z(3)+Z(11))-(Z(7)+Z(15))))
      Z0=Z0+Z7 ! i((Z(2)+Z(10))-(Z(6)+Z(14)))
      ! + i C2P8*((Z(1)+Z(9))-(Z(5)+Z(13))+((Z(3)+Z(11))-(Z(7)+Z(15))))

      Z7=Z6
      Z6=Z6-Z2
      Z2=Z2+Z7

      Z7=Z4
      Z4=Z4-Z0
      Z0=Z0+Z7

      ZD(6)=Z2
      ZD(10)=Z6
      ZD(14)=Z4
      ZD(2)=Z0
      
*----  Z0, Z2, Z4, Z6, Z7 空き      

      ! Z5 ! (Z(1)-Z(9))+(Z(7)-Z(15))
      ! Z3 ! (Z(3)-Z(11))+(Z(5)-Z(13))

      Z2=Z5
      Z5=Z5*C3P8
      Z2=Z2*C1P8

      Z0=Z3
      Z3=Z3*C1P8
      Z0=Z0*C3P8      

      Z5=Z5+Z3
      Z2=Z2-Z0
      Z5=Z5*ZI
      ! i((Z(1)-Z(9))+(Z(7)-Z(15))*C3P8+((Z(3)-Z(11))+(Z(5)-Z(13)))*C1P8)
      Z2=Z2*ZI 
      ! i((Z(1)-Z(9))+(Z(7)-Z(15))*C1P8-((Z(3)-Z(11))+(Z(5)-Z(13)))*C3P8)

*--  Z0, Z3, Z4, Z6, Z7 空き      

      !      Z(1) ! (Z(1)-Z(9))-(Z(7)-Z(15))
      !      Z(3) ! (Z(3)-Z(11))-(Z(5)-Z(13))

      Z0=ZD(1)
      Z3=ZD(3)
      Z4=Z0
      Z6=Z3
      Z0=Z0*C1P8
      Z4=Z4*C3P8
      Z3=Z3*C3P8
      Z6=Z6*C1P8
      Z0=Z0+Z3
      ! ((Z(1)-Z(9))-(Z(7)-Z(15))*C1P8+((Z(3)-Z(11))-(Z(5)-Z(13)))*C3P8)
      Z4=Z4-Z6
      ! ((Z(1)-Z(9))-(Z(7)-Z(15))*C3P8-((Z(3)-Z(11))-(Z(5)-Z(13)))*C1P8)

*--  Z3, Z6, Z7 空き

      !  Z(8) ! Z(4)-Z(12)
      !  Z1 ! (Z(2)-Z(10))+(Z(6)-Z(14))
      Z3=ZD(8)
      Z1=Z1*C2P8
      Z7=Z3
      Z3=Z3-Z1 ! 
      Z1=Z1+Z7 ! 
      Z3=Z3*ZI ! i(Z(4)-Z(12)-((Z(2)-Z(10))+(Z(6)-Z(14)))*C2P8)
      Z1=Z1*ZI ! i(Z(4)-Z(12)+((Z(2)-Z(10))+(Z(6)-Z(14)))*C2P8)

      ! Z(9) ! Z(0)-Z(8)+C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))
      Z6=ZD(9)
      ! Z6, Z1, Z0, Z5 で 計算

      Z7=Z6
      Z6=Z6-Z0
      !   Z(0)-Z(8)+C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))
      ! -((Z(1)-Z(9))-(Z(7)-Z(15))*C1P8+((Z(3)-Z(11))-(Z(5)-Z(13)))*C3P8)
      Z0=Z0+Z7
      !   Z(0)-Z(8)+C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))
      ! +((Z(1)-Z(9))-(Z(7)-Z(15))*C1P8+((Z(3)-Z(11))-(Z(5)-Z(13)))*C3P8)

      Z7=Z5
      Z5=Z5-Z1
      ! i((Z(1)-Z(9))+(Z(7)-Z(15))*C3P8+((Z(3)-Z(11))+(Z(5)-Z(13)))*C1P8)
      ! -i(Z(4)-Z(12)+((Z(2)-Z(10))+(Z(6)-Z(14)))*C2P8)
      Z1=Z1+Z7
      ! i((Z(1)-Z(9))+(Z(7)-Z(15))*C3P8+((Z(3)-Z(11))+(Z(5)-Z(13)))*C1P8)
      ! +i(Z(4)-Z(12)+((Z(2)-Z(10))+(Z(6)-Z(14)))*C2P8)

      Z7=Z6
      Z6=Z6-Z5
      Z5=Z5+Z7
      ZD(9)=Z6
      ZD(7)=Z5

      Z7=Z0
      Z0=Z0-Z1
      Z1=Z1+Z7
      ZD(1)=Z1
      ZD(15)=Z0

!-- Z0,Z1,Z5,Z6,Z7 空き

      ! Z3 ! i(Z(4)-Z(12)-((Z(2)-Z(10))+(Z(6)-Z(14)))*C2P8)
      ! Z4
      ! ((Z(1)-Z(9))-(Z(7)-Z(15))*C3P8-((Z(3)-Z(11))-(Z(5)-Z(13)))*C1P8)
      ! Z2
      ! i((Z(1)-Z(9))+(Z(7)-Z(15))*C1P8-((Z(3)-Z(11))+(Z(5)-Z(13)))*C3P8)
      Z5=ZD(13) ! Z(0)-Z(8)-C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))            

      Z7=Z5
      Z5=Z5-Z4
      ! Z(0)-Z(8)-C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))
      ! -((Z(1)-Z(9))-(Z(7)-Z(15))*C3P8-((Z(3)-Z(11))-(Z(5)-Z(13)))*C1P8)
      Z4=Z4+Z7
      ! Z(0)-Z(8)-C2P8*((Z(2)-Z(10))-(Z(6)-Z(14)))
      ! +((Z(1)-Z(9))-(Z(7)-Z(15))*C3P8-((Z(3)-Z(11))-(Z(5)-Z(13)))*C1P8)

      Z7=Z2
      Z2=Z2-Z3
      ! i((Z(1)-Z(9))+(Z(7)-Z(15))*C1P8-((Z(3)-Z(11))+(Z(5)-Z(13)))*C3P8)
      ! -i(Z(4)-Z(12)-((Z(2)-Z(10))+(Z(6)-Z(14)))*C2P8)
      Z3=Z3+Z7
      ! i((Z(1)-Z(9))+(Z(7)-Z(15))*C1P8-((Z(3)-Z(11))+(Z(5)-Z(13)))*C3P8)
      ! +i(Z(4)-Z(12)-((Z(2)-Z(10))+(Z(6)-Z(14)))*C2P8)

      Z7=Z4
      Z4=Z4-Z2
      Z2=Z2+Z7
      ZD(13)=Z4
      ZD(3)=Z2

      Z7=Z5
      Z5=Z5-Z3
      Z3=Z3+Z7

      Z6=ZD(11)  ! 退避 ! (Z(3)+Z(11))+(Z(7)+Z(15))
      ZD(11)=Z5
      Z5=ZD(5)   ! 退避 ! (Z(1)+Z(9))+(Z(5)+Z(13))
      ZD(5)=Z3

! Z0,Z1,Z2,Z3,Z4,Z7 空き

      Z7=Z5
      Z5=Z5-Z6 ! (Z(1)+Z(9))+(Z(5)+Z(13))-((Z(3)+Z(11))+(Z(7)+Z(15)))
      Z6=Z6+Z7 ! (Z(1)+Z(9))+(Z(5)+Z(13))+((Z(3)+Z(11))+(Z(7)+Z(15)))

      Z5=Z5*ZI ! i((Z(1)+Z(9))+(Z(5)+Z(13))-((Z(3)+Z(11))+(Z(7)+Z(15))))

      Z0=ZD(4)  ! (Z(0)+Z(8))+(Z(4)+Z(12))
      Z1=ZD(0)  ! (Z(2)+Z(10))+(Z(6)+Z(14))

      Z7=Z0
      Z0=Z0-Z1 ! (Z(0)+Z(8))+(Z(4)+Z(12))-((Z(2)+Z(10))+(Z(6)+Z(14)))
      Z1=Z1+Z7 ! (Z(0)+Z(8))+(Z(4)+Z(12))+((Z(2)+Z(10))+(Z(6)+Z(14)))

      Z7=Z1
      Z1=Z1-Z6
      Z6=Z6+Z7
      ZD(0)=Z6
      ZD(8)=Z1

      Z7=Z0
      Z0=Z0-Z5
      Z5=Z5+Z7
      ZD(4)=Z5
      ZD(12)=Z0

      END
