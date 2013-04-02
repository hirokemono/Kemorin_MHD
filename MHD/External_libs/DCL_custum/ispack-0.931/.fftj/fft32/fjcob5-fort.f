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
* fft 32 out-of-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJCOB5(Z,ZT,ZDD,ZE)

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

      ZDD( 1)=C1P16*ZA1o+S1P16*ZA4o
      ZDD(17)=S1P16*ZA1o-C1P16*ZA4o
      ZDD(15)=S1P16*ZB1o+C1P16*ZB4o
      ZDD(31)=C1P16*ZB1o-S1P16*ZB4o

      ZDD( 7)=ZA1e+ZA4e
      ZDD(23)=ZA1e-ZA4e
      ZDD( 9)=ZB1e+ZB4e
      ZDD(25)=ZB1e-ZB4e

*-------------------------------------------------------

      ZA2o=(Z(3)-Z(19))-(Z(13)-Z(29))
      ZB2o=(Z(3)-Z(19))+(Z(13)-Z(29))      
      ZA2e=(Z(3)+Z(19))+(Z(13)+Z(29))
      ZB2e=(Z(3)+Z(19))-(Z(13)+Z(29))      

      ZA3o=(Z(5)-Z(21))-(Z(11)-Z(27))
      ZB3o=(Z(5)-Z(21))+(Z(11)-Z(27))
      ZA3e=(Z(5)+Z(21))+(Z(11)+Z(27))
      ZB3e=(Z(5)+Z(21))-(Z(11)+Z(27))

      ZDD( 3)=C1P16*ZA2o-S1P16*ZA3o
      ZDD(19)=S1P16*ZA2o+C1P16*ZA3o
      ZDD(13)=S1P16*ZB2o-C1P16*ZB3o
      ZDD(29)=C1P16*ZB2o+S1P16*ZB3o

      ZDD( 5)=ZA2e+ZA3e
      ZDD(21)=ZA2e-ZA3e
      ZDD(11)=ZB2e+ZB3e
      ZDD(27)=ZB2e-ZB3e

*-------------------------------------------------------      

      ZC0o=Z(0)-Z(16)
      ZD0o=Z(8)-Z(24)
      ZC0e=Z(0)+Z(16)
      ZD0e=Z(8)+Z(24)

      ZC2o=(Z(4)-Z(20))-(Z(12)-Z(28))
      ZD2o=(Z(4)-Z(20))+(Z(12)-Z(28))
      ZC2e=(Z(4)+Z(20))+(Z(12)+Z(28))
      ZD2e=(Z(4)+Z(20))-(Z(12)+Z(28))

      ZDD( 0)=ZC0o+C4P16*ZC2o
      ZDD(16)=ZC0o-C4P16*ZC2o
      ZDD( 8)=ZD0o+C4P16*ZD2o
      ZDD(24)=ZD0o-C4P16*ZD2o

      ZDD( 4)=(ZC0e+ZD0e)+ZC2e
      ZDD(20)=(ZC0e+ZD0e)-ZC2e
      ZDD(12)=ZC0e-ZD0e
      ZDD(28)=ZD2e

*-------------------------------------------------------
      
      ZC1o=(Z(2)-Z(18))-(Z(14)-Z(30))
      ZD1o=(Z(2)-Z(18))+(Z(14)-Z(30))
      ZC1e=(Z(2)+Z(18))+(Z(14)+Z(30))
      ZD1e=(Z(2)+Z(18))-(Z(14)+Z(30))

      ZC3o=(Z(6)-Z(22))-(Z(10)-Z(26))
      ZD3o=(Z(6)-Z(22))+(Z(10)-Z(26))
      ZC3e=(Z(6)+Z(22))+(Z(10)+Z(26))
      ZD3e=(Z(6)+Z(22))-(Z(10)+Z(26))

      ZDD( 2)=C2P16*ZC1o+S2P16*ZC3o
      ZDD(18)=S2P16*ZC1o-C2P16*ZC3o
      ZDD(14)=S2P16*ZD1o+C2P16*ZD3o
      ZDD(30)=C2P16*ZD1o-S2P16*ZD3o

      ZDD( 6)=ZC1e+ZC3e
      ZDD(22)=C4P16*(ZC1e-ZC3e)
      ZDD(10)=C4P16*(ZD1e+ZD3e)
      ZDD(26)=ZD1e-ZD3e

*-------------------------------------------------------
*-------------------------------------------------------      
* K=0

      ZA=ZDD(7)+ZDD(5)
      ZC=ZDD(4)+ZDD(6)      

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZT(0)=ZA
      ZT(16)=ZC
      
*-------------------------------------------------------
* K=1      

      ZA=ZDD(1)+C4P16*(ZDD(3)+ZDD(19))
      ZB=ZI*(ZDD(15)+C4P16*(ZDD(29)-ZDD(13)))
      ZC=ZDD(0)+ZDD(2)
      ZD=ZI*(ZDD(8)+ZDD(14))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(1)=ZA+ZB
      ZT(31)=ZA-ZB
      ZT(15)=ZC+ZD
      ZT(17)=ZC-ZD

*-------------------------------------------------------      
* K=2

      ZA=C2P16*ZDD(23)+S2P16*ZDD(21)
      ZB=ZI*(S2P16*ZDD(9)+C2P16*ZDD(11))
      ZC=ZDD(12)+ZDD(22)
      ZD=ZI*(ZDD(28)+ZDD(10))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(2)=ZA+ZB
      ZT(30)=ZA-ZB
      ZT(14)=ZC+ZD
      ZT(18)=ZC-ZD

*-------------------------------------------------------
* K=3

      ZA=C4P16*(ZDD(1)+ZDD(17))-ZDD(19)
      ZB=ZI*(C4P16*(ZDD(31)-ZDD(15))+ZDD(29))
      ZC=ZDD(16)+ZDD(18)
      ZD=ZI*(-ZDD(24)+ZDD(30))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(3)=ZA+ZB
      ZT(29)=ZA-ZB
      ZT(13)=ZC+ZD
      ZT(19)=ZC-ZD

*-------------------------------------------------------      
* K=4

      ZA=C4P16*(ZDD(7)-ZDD(5))
      ZB=ZI*C4P16*(ZDD(25)+ZDD(27))
      ZC=ZDD(20)
      ZD=ZI*ZDD(26)

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(4)=ZA+ZB
      ZT(28)=ZA-ZB
      ZT(12)=ZC+ZD
      ZT(20)=ZC-ZD
      
*-------------------------------------------------------
* K=5

      ZA=C4P16*(ZDD(1)-ZDD(17))-ZDD(3)
      ZB=ZI*(C4P16*(ZDD(31)+ZDD(15))+ZDD(13))
      ZC=ZDD(16)-ZDD(18)
      ZD=ZI*(ZDD(24)+ZDD(30))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(5)=ZA+ZB
      ZT(27)=ZA-ZB
      ZT(11)=ZC+ZD
      ZT(21)=ZC-ZD

*-------------------------------------------------------      
* K=6

      ZA=S2P16*ZDD(23)-C2P16*ZDD(21)
      ZB=ZI*(C2P16*ZDD(9)-S2P16*ZDD(11))
      ZC=ZDD(12)-ZDD(22)
      ZD=ZI*(-ZDD(28)+ZDD(10))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(6)=ZA+ZB
      ZT(26)=ZA-ZB
      ZT(10)=ZC+ZD
      ZT(22)=ZC-ZD

*-------------------------------------------------------
* K=7

      ZA=ZDD(17)+C4P16*(-ZDD(3)+ZDD(19))
      ZB=ZI*(ZDD(31)+C4P16*(-ZDD(29)-ZDD(13)))
      ZC=ZDD(0)-ZDD(2)
      ZD=ZI*(-ZDD(8)+ZDD(14))

      ZAD=ZA
      ZA=ZA+ZC
      ZC=ZC-ZAD

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(7)=ZA+ZB
      ZT(25)=ZA-ZB
      ZT(9)=ZC+ZD
      ZT(23)=ZC-ZD

*-------------------------------------------------------
* K=8      

      ZD=ZI*(ZDD(25)-ZDD(27))
      ZB=ZDD(4)-ZDD(6)

      ZBD=ZB
      ZB=ZB+ZD
      ZD=ZBD-ZD

      ZT(8)=ZB
      ZT(24)=ZD

      END
