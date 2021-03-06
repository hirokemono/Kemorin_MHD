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
* rfft 16 out-of-place forward
*-----------------------------------------------------------------------
      SUBROUTINE FJROF4(Z,ZD,ZDD,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P8=0.7071067811865475244D0) ! COS(2*PI/8)
      PARAMETER(ZT1=(0.3086582838174551142D0,-0.4619397662556433781D0))
      PARAMETER(ZT2=(0.1464466094067262378D0,-0.3535533905932737622D0))
      PARAMETER(ZT3=(0.0380602337443566220D0,-0.1913417161825448859D0))
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:7),ZD(0:7),ZDD(*)
      DIMENSION ZT(*)

*-----------------------------------------------------------------------
* fft 8 in-place backward
*-----------------------------------------------------------------------
      Z0=Z(1)
      Z1=Z(5)
      Z2=Z(3)
      Z3=Z(7)
      
      Z7=Z0
      Z0=Z0-Z1    ! Z(1)-Z(5)
      Z1=Z1+Z7    ! Z(1)+Z(5)

      Z7=Z2
      Z2=Z2-Z3    ! Z(3)-Z(7)
      Z3=Z3+Z7    ! Z(3)+Z(7)

      Z7=Z1
      Z1=Z1-Z3    ! (Z(1)+Z(5))-(Z(3)+Z(7))
      Z3=Z3+Z7    ! (Z(1)+Z(5))+(Z(3)+Z(7))

      Z7=Z0
      Z0=Z0-Z2    ! (Z(1)-Z(5))-(Z(3)-Z(7))
      Z2=Z2+Z7    ! (Z(1)-Z(5))+(Z(3)-Z(7))

      Z1=Z1*ZI      ! i*((Z(1)+Z(5))-(Z(3)+Z(7)))
      Z2=Z2*ZI*C1P8 ! i*C1P8((Z(1)-Z(5))+(Z(3)-Z(7)))
      Z0=Z0*C1P8    ! C1P8*((Z(1)-Z(5))-(Z(3)-Z(7)))

      Z4=Z(2)
      Z5=Z(6)

      Z7=Z4
      Z4=Z4-Z5 ! Z(2)-Z(6)
      Z5=Z5+Z7 ! Z(2)+Z(6)

      Z4=Z4*ZI ! i*(Z(2)-Z(6))

      Z7=Z2
      Z2=Z2-Z4 ! -i*(Z(2)-Z(6))+i*C1P8((Z(1)-Z(5))+(Z(3)-Z(7)))
      Z4=Z4+Z7 !  i*(Z(2)-Z(6))+i*C1P8((Z(1)-Z(5))+(Z(3)-Z(7)))

      ZD(2)=Z3 ! 一時退避(Z3空き)

      Z6=Z(0)
      Z7=Z(4)
      Z3=Z6
      Z6=Z6-Z7 ! Z(0)-Z(4)
      Z7=Z7+Z3 ! Z(0)+Z(4)

      Z3=Z6
      Z6=Z6-Z0 ! (Z(0)-Z(4))-C1P8*((Z(1)-Z(5))-(Z(3)-Z(7)))
      Z0=Z0+Z3 ! (Z(0)-Z(4))+C1P8*((Z(1)-Z(5))-(Z(3)-Z(7)))

      Z3=Z6
      Z6=Z6-Z2
      Z2=Z2+Z3
      ZD(3)=Z2
      ZD(5)=Z6

      Z2=Z0
      Z0=Z0-Z4
      Z4=Z4+Z2
      ZD(7)=Z0
      ZD(1)=Z4


      Z2=Z7
      Z7=Z7-Z5 ! (Z(0)+Z(4))-(Z(2)+Z(6))
      Z5=Z5+Z2 ! (Z(0)+Z(4))+(Z(2)+Z(6))

      Z3=ZD(2) ! Z3の復元

      Z2=Z5
      Z5=Z5-Z3 ! (Z(0)+Z(4))+(Z(2)+Z(6))-((Z(1)+Z(5))+(Z(3)+Z(7)))
      Z3=Z3+Z2 ! (Z(0)+Z(4))+(Z(2)+Z(6))+((Z(1)+Z(5))+(Z(3)+Z(7)))
      ZD(4)=Z5
      ZD(0)=Z3

      Z2=Z7
      Z7=Z7-Z1 ! (Z(0)+Z(4))-(Z(2)+Z(6))-i((Z(1)+Z(5))-(Z(3)+Z(7)))
      Z1=Z1+Z2 ! (Z(0)+Z(4))-(Z(2)+Z(6))+i((Z(1)+Z(5))-(Z(3)+Z(7)))
      ZD(2)=Z1
      ZD(6)=Z7

*-----------------------------------------------------------------------
* rfft のための後処理
*-----------------------------------------------------------------------

      ZD(0)=(1+ZI)*CONJG(ZD(0))

      ZTMP=(ZD(7)-CONJG(ZD(1)))*ZT1
      ZD(1)=CONJG(ZD(1))+ZTMP
      ZD(7)=CONJG(ZD(7)-ZTMP)

      ZTMP=(ZD(6)-CONJG(ZD(2)))*ZT2
      ZD(2)=CONJG(ZD(2))+ZTMP
      ZD(6)=CONJG(ZD(6)-ZTMP)

      ZTMP=(ZD(5)-CONJG(ZD(3)))*ZT3
      ZD(3)=CONJG(ZD(3))+ZTMP
      ZD(5)=CONJG(ZD(5)-ZTMP)

      ZD(4)=CONJG(ZD(4))

      END
