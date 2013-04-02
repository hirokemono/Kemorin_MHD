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
* fft 32 in-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJCIB5(Z,ZT,ZDD,ZE)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(C1P16=0.98078528040323044912D0) ! COS(1*PI/16)
      PARAMETER(S1P16=0.19509032201612826784D0) ! SIN(1*PI/16)
      PARAMETER(C2P16=0.92387953251128675613D0) ! COS(2*PI/16)
      PARAMETER(S2P16=0.38268343236508977172D0) ! SIN(2*PI/16)      
      PARAMETER(C4P16=0.70710678118654752441D0) ! COS(4*PI/16)
      PARAMETER(ZI=(0,1))
      DIMENSION Z(0:31),ZT(0:31),ZDD(0:31)
      DIMENSION ZE(2*32)

      ZA1o=(Z(1)-Z(17))-(Z(15)-Z(31))
      ZB1o=(Z(1)-Z(17))+(Z(15)-Z(31))      
      ZA1e=(Z(1)+Z(17))+(Z(15)+Z(31))
      ZB1e=(Z(1)+Z(17))-(Z(15)+Z(31))      

      ZA4o=(Z(7)-Z(23))-(Z(9)-Z(25))
      ZB4o=(Z(7)-Z(23))+(Z(9)-Z(25))
      ZA4e=(Z(7)+Z(23))+(Z(9)+Z(25))
      ZB4e=(Z(7)+Z(23))-(Z(9)+Z(25))

      ZT( 1)=C1P16*ZA1o+S1P16*ZA4o
      ZT(17)=S1P16*ZA1o-C1P16*ZA4o
      ZT(15)=S1P16*ZB1o+C1P16*ZB4o
      ZT(31)=C1P16*ZB1o-S1P16*ZB4o

      ZT( 7)=ZA1e+ZA4e
      ZT(23)=ZA1e-ZA4e
      ZT( 9)=ZB1e+ZB4e
      ZT(25)=ZB1e-ZB4e

*-------------------------------------------------------

      ZA2o=(Z(3)-Z(19))-(Z(13)-Z(29))
      ZB2o=(Z(3)-Z(19))+(Z(13)-Z(29))      
      ZA2e=(Z(3)+Z(19))+(Z(13)+Z(29))
      ZB2e=(Z(3)+Z(19))-(Z(13)+Z(29))      

      ZA3o=(Z(5)-Z(21))-(Z(11)-Z(27))
      ZB3o=(Z(5)-Z(21))+(Z(11)-Z(27))
      ZA3e=(Z(5)+Z(21))+(Z(11)+Z(27))
      ZB3e=(Z(5)+Z(21))-(Z(11)+Z(27))

      ZT( 3)=C1P16*ZA2o-S1P16*ZA3o
      ZT(19)=S1P16*ZA2o+C1P16*ZA3o
      ZT(13)=S1P16*ZB2o-C1P16*ZB3o
      ZT(29)=C1P16*ZB2o+S1P16*ZB3o

      ZT( 5)=ZA2e+ZA3e
      ZT(21)=ZA2e-ZA3e
      ZT(11)=ZB2e+ZB3e
      ZT(27)=ZB2e-ZB3e

*-------------------------------------------------------      

      ZC0o=Z(0)-Z(16)
      ZD0o=Z(8)-Z(24)
      ZC0e=Z(0)+Z(16)
      ZD0e=Z(8)+Z(24)

      ZC2o=(Z(4)-Z(20))-(Z(12)-Z(28))
      ZD2o=(Z(4)-Z(20))+(Z(12)-Z(28))
      ZC2e=(Z(4)+Z(20))+(Z(12)+Z(28))
      ZD2e=(Z(4)+Z(20))-(Z(12)+Z(28))

      ZT( 0)=ZC0o+C4P16*ZC2o
      ZT(16)=ZC0o-C4P16*ZC2o
      ZT( 8)=ZD0o+C4P16*ZD2o
      ZT(24)=ZD0o-C4P16*ZD2o

      ZT( 4)=(ZC0e+ZD0e)+ZC2e
      ZT(20)=(ZC0e+ZD0e)-ZC2e
      ZT(12)=ZC0e-ZD0e
      ZT(28)=ZD2e

*-------------------------------------------------------
      
      ZC1o=(Z(2)-Z(18))-(Z(14)-Z(30))
      ZD1o=(Z(2)-Z(18))+(Z(14)-Z(30))
      ZC1e=(Z(2)+Z(18))+(Z(14)+Z(30))
      ZD1e=(Z(2)+Z(18))-(Z(14)+Z(30))

      ZC3o=(Z(6)-Z(22))-(Z(10)-Z(26))
      ZD3o=(Z(6)-Z(22))+(Z(10)-Z(26))
      ZC3e=(Z(6)+Z(22))+(Z(10)+Z(26))
      ZD3e=(Z(6)+Z(22))-(Z(10)+Z(26))

      ZT( 2)=C2P16*ZC1o+S2P16*ZC3o
      ZT(18)=S2P16*ZC1o-C2P16*ZC3o
      ZT(14)=S2P16*ZD1o+C2P16*ZD3o
      ZT(30)=C2P16*ZD1o-S2P16*ZD3o

      ZT( 6)=ZC1e+ZC3e
      ZT(22)=C4P16*(ZC1e-ZC3e)
      ZT(10)=C4P16*(ZD1e+ZD3e)
      ZT(26)=ZD1e-ZD3e

*-------------------------------------------------------
*-------------------------------------------------------      
* K=0

      ZA=ZT(7)+ZT(5)
      ZC=ZT(4)+ZT(6)      

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      Z(0)=ZA
      Z(16)=ZC
      
*-------------------------------------------------------
* K=1      

      ZA=ZT(1)+C4P16*(ZT(3)+ZT(19))
      ZB=ZI*(ZT(15)+C4P16*(ZT(29)-ZT(13)))
      ZC=ZT(0)+ZT(2)
      ZD=ZI*(ZT(8)+ZT(14))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(1)=ZA+ZB
      Z(31)=ZA-ZB
      Z(15)=ZC+ZD
      Z(17)=ZC-ZD

*-------------------------------------------------------      
* K=2

      ZA=C2P16*ZT(23)+S2P16*ZT(21)
      ZB=ZI*(S2P16*ZT(9)+C2P16*ZT(11))
      ZC=ZT(12)+ZT(22)
      ZD=ZI*(ZT(28)+ZT(10))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(2)=ZA+ZB
      Z(30)=ZA-ZB
      Z(14)=ZC+ZD
      Z(18)=ZC-ZD

*-------------------------------------------------------
* K=3

      ZA=C4P16*(ZT(1)+ZT(17))-ZT(19)
      ZB=ZI*(C4P16*(ZT(31)-ZT(15))+ZT(29))
      ZC=ZT(16)+ZT(18)
      ZD=ZI*(-ZT(24)+ZT(30))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(3)=ZA+ZB
      Z(29)=ZA-ZB
      Z(13)=ZC+ZD
      Z(19)=ZC-ZD

*-------------------------------------------------------      
* K=4

      ZA=C4P16*(ZT(7)-ZT(5))
      ZB=ZI*C4P16*(ZT(25)+ZT(27))
      ZC=ZT(20)
      ZD=ZI*ZT(26)

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(4)=ZA+ZB
      Z(28)=ZA-ZB
      Z(12)=ZC+ZD
      Z(20)=ZC-ZD
      
*-------------------------------------------------------
* K=5

      ZA=C4P16*(ZT(1)-ZT(17))-ZT(3)
      ZB=ZI*(C4P16*(ZT(31)+ZT(15))+ZT(13))
      ZC=ZT(16)-ZT(18)
      ZD=ZI*(ZT(24)+ZT(30))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(5)=ZA+ZB
      Z(27)=ZA-ZB
      Z(11)=ZC+ZD
      Z(21)=ZC-ZD

*-------------------------------------------------------      
* K=6

      ZA=S2P16*ZT(23)-C2P16*ZT(21)
      ZB=ZI*(C2P16*ZT(9)-S2P16*ZT(11))
      ZC=ZT(12)-ZT(22)
      ZD=ZI*(-ZT(28)+ZT(10))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(6)=ZA+ZB
      Z(26)=ZA-ZB
      Z(10)=ZC+ZD
      Z(22)=ZC-ZD

*-------------------------------------------------------
* K=7

      ZA=ZT(17)+C4P16*(-ZT(3)+ZT(19))
      ZB=ZI*(ZT(31)+C4P16*(-ZT(29)-ZT(13)))
      ZC=ZT(0)-ZT(2)
      ZD=ZI*(-ZT(8)+ZT(14))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(7)=ZA+ZB
      Z(25)=ZA-ZB
      Z(9)=ZC+ZD
      Z(23)=ZC-ZD

*-------------------------------------------------------
* K=8      

      ZD=ZI*(ZT(25)-ZT(27))
      ZB=ZT(4)-ZT(6)

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      Z(8)=ZB
      Z(24)=ZD

      END
