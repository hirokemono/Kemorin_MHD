########################################################################
# FTTJ:  An FFT library
# Copyright (C) 2008 Keiichi Ishioka <ishioka@gfd-dennou.org>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA.
########################################################################
# fft 32 out-of-place forward
.text
.globl fjcof5_
fjcof5_:
       movl    4(%esp), %eax
       movl    8(%esp), %ecx  # : ZD  のベースアドレス       
       movl   12(%esp), %edx  # : ZDD のベースアドレス              
#-------------------------------------------------------------

       movaps  16(%eax), %xmm0  # Z(1)
       movaps 272(%eax), %xmm1  # Z(17)       
       movaps 240(%eax), %xmm2  # Z(15)       
       movaps 496(%eax), %xmm3  # Z(31)
       
       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0  # Z(1)-Z(17) -> xmm0
       addpd %xmm7,%xmm1  # Z(1)+Z(17) -> xmm1
		     
       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2  # Z(15)-Z(31) -> xmm2
       addpd %xmm7,%xmm3  # Z(15)+Z(31) -> xmm3
		     
       movaps %xmm0,%xmm7
       subpd %xmm2,%xmm0  # ZA1o -> xmm0
       addpd %xmm7,%xmm2  # ZB1o -> xmm2
		     
       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1  # ZB1e -> xmm1
       addpd %xmm7,%xmm3  # ZA1e -> xmm3
       
       movaps  %xmm0, (%edx)   # ZA1o -> ZD(1) # ZD(0)
		     
       movaps 112(%eax), %xmm4  # Z(7)
       movaps 368(%eax), %xmm5  # Z(23)
       movaps 144(%eax), %xmm6  # Z(9)
       movaps 400(%eax), %xmm7  # Z(25)
       
       movaps %xmm4,%xmm0
       subpd %xmm5,%xmm4  # Z(7)-Z(23) -> xmm4
       addpd %xmm0,%xmm5  # Z(7)+Z(23) -> xmm5

       movaps %xmm6,%xmm0
       subpd %xmm7,%xmm6  # Z(9)-Z(25) -> xmm6
       addpd %xmm0,%xmm7  # Z(9)+Z(25) -> xmm7

       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4  # ZA4o -> xmm4
       addpd %xmm0,%xmm6  # ZB4o -> xmm6

       movaps %xmm5,%xmm0
       subpd %xmm7,%xmm5  # ZB4e -> xmm5
       addpd %xmm0,%xmm7  # ZA4e -> xmm7

       movaps %xmm3,%xmm0
       subpd %xmm7,%xmm3  
       addpd %xmm0,%xmm7  
       movaps  %xmm3, 112(%edx)   # ZA1e-ZA4e -> ZD(23) # ZD(7)
       movaps  %xmm7,  64(%edx)   # ZA1e+ZA4e -> ZD(7) # ZD(4)

       movaps %xmm1,%xmm0
       subpd %xmm5,%xmm1
       addpd %xmm0,%xmm5
       movaps  %xmm1,  80(%edx)   # ZB1e-ZB4e -> ZD(25) # ZD(5)
       movaps  %xmm5,  96(%edx)   # ZB1e+ZB4e -> ZD(9) # ZD(6)
       
       movaps .C1, %xmm1
       movaps .S1, %xmm3
       
       movaps %xmm2,%xmm5 # ZB1o
       movaps %xmm6,%xmm7 # ZB4o
       
       mulpd %xmm3,%xmm2 # S1P16*ZB1o
       mulpd %xmm1,%xmm5 # C1P16*ZB1o
      
       mulpd %xmm3,%xmm6 # S1P16*ZB4o
       mulpd %xmm1,%xmm7 # C1P16*ZB4o
       
       addpd %xmm7,%xmm2 # S1P16*ZB1o+C1P16*ZB4o
       subpd %xmm6,%xmm5 # C1P16*ZB1o-S1P16*ZB4o      
       
       movaps  %xmm2, 32(%edx)   # ZD(15) # ZD(2)
       movaps  %xmm5, 16(%edx)   # ZD(31) # ZD(1)
       
       movaps  (%edx), %xmm0    # ZD(1) # ZD(0) -> ZA1o 
       movaps %xmm4,%xmm7 # ZA4o
       movaps %xmm0,%xmm5 # ZA1o
       
       mulpd %xmm3,%xmm4 # S1P16*ZA4o
       mulpd %xmm1,%xmm7 # C1P16*ZA4o
       
       mulpd %xmm3,%xmm0 # S1P16*ZA1o
       mulpd %xmm1,%xmm5 # C1P16*ZA1o
      
       subpd %xmm7,%xmm0 # S1P16*ZA1o-C1P16*ZA4o
       addpd %xmm4,%xmm5 # C1P16*ZA1o+S1P16*ZA4o

       movaps  %xmm0,48(%edx)   # ZD(17) # ZD(3)
       movaps  %xmm5, (%edx)   # ZD(1) # ZD(0)

#---------------------------------------
       movaps  48(%eax), %xmm0  # Z(3)
       movaps 304(%eax), %xmm1  # Z(19)       
       movaps 208(%eax), %xmm2  # Z(13)       
       movaps 464(%eax), %xmm3  # Z(29)
       
       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0  # Z(3)-Z(19) -> xmm0
       addpd %xmm7,%xmm1  # Z(3)+Z(19) -> xmm1
		     
       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2  # Z(13)-Z(29) -> xmm2
       addpd %xmm7,%xmm3  # Z(13)+Z(29) -> xmm3
		     
       movaps %xmm0,%xmm7
       subpd %xmm2,%xmm0  # ZA2o -> xmm0
       addpd %xmm7,%xmm2  # ZB2o -> xmm2
		     
       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1  # ZB2e -> xmm1
       addpd %xmm7,%xmm3  # ZA2e -> xmm3
       
       movaps  %xmm0, 128(%edx)   # ZA2o -> ZD(3) # ZD(8)
		     
       movaps  80(%eax), %xmm4  # Z(5)
       movaps 336(%eax), %xmm5  # Z(21)       
       movaps 176(%eax), %xmm6  # Z(11)       
       movaps 432(%eax), %xmm7  # Z(27)
       
       movaps %xmm4,%xmm0
       subpd %xmm5,%xmm4  # Z(5)-Z(21) -> xmm4
       addpd %xmm0,%xmm5  # Z(5)+Z(21) -> xmm5

       movaps %xmm6,%xmm0
       subpd %xmm7,%xmm6  # Z(11)-Z(27) -> xmm6
       addpd %xmm0,%xmm7  # Z(11)+Z(27) -> xmm7

       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4  # ZA3o -> xmm4
       addpd %xmm0,%xmm6  # ZB3o -> xmm6

       movaps %xmm5,%xmm0
       subpd %xmm7,%xmm5  # ZB3e -> xmm5
       addpd %xmm0,%xmm7  # ZA3e -> xmm7

       movaps %xmm3,%xmm0
       subpd %xmm7,%xmm3  
       addpd %xmm0,%xmm7  
       movaps  %xmm3, 240(%edx)   # ZA2e-ZA3e -> ZD(21) # ZD(15)
       movaps  %xmm7, 192(%edx)   # ZA2e+ZA3e -> ZD(5) # ZD(12)

       movaps %xmm1,%xmm0
       subpd %xmm5,%xmm1
       addpd %xmm0,%xmm5
       movaps  %xmm1, 208(%edx)   # ZB2e-ZB3e -> ZD(27) # ZD(13)
       movaps  %xmm5, 224(%edx)   # ZB2e+ZB3e -> ZD(11) # ZD(14)
       
       movaps .C1, %xmm1
       movaps .S1, %xmm3
       
       movaps %xmm2,%xmm5 # ZB2o
       movaps %xmm6,%xmm7 # ZB3o
       
       mulpd %xmm3,%xmm2 # S1P16*ZB2o
       mulpd %xmm1,%xmm5 # C1P16*ZB2o
      
       mulpd %xmm3,%xmm6 # S1P16*ZB3o
       mulpd %xmm1,%xmm7 # C1P16*ZB3o
       
       subpd %xmm7,%xmm2 # S1P16*ZB2o-C1P16*ZB3o
       addpd %xmm6,%xmm5 # C1P16*ZB2o+S1P16*ZB3o      
       
       movaps  %xmm2, 160(%edx)   # ZD(13) # ZD(10)
       movaps  %xmm5, 144(%edx)   # ZD(29) # ZD(9)
       
       movaps 128(%edx), %xmm0    # ZD(3) # ZD(8) -> ZA2o
       movaps %xmm4,%xmm7 # ZA3o
       movaps %xmm0,%xmm5 # ZA2o
       
       mulpd %xmm3,%xmm4 # S1P16*ZA3o
       mulpd %xmm1,%xmm7 # C1P16*ZA3o
       
       mulpd %xmm3,%xmm0 # S1P16*ZA2o
       mulpd %xmm1,%xmm5 # C1P16*ZA2o
      
       addpd %xmm7,%xmm0 # S1P16*ZA1o+C1P16*ZA4o
       subpd %xmm4,%xmm5 # C1P16*ZA1o-S1P16*ZA4o

       movaps  %xmm0,176(%edx)   # ZD(19) # ZD(11)
       movaps  %xmm5, 128(%edx)   # ZD(3) # ZD(8)
       
#---------------------------------------
       movaps  64(%eax), %xmm0  # Z(4)
       movaps 320(%eax), %xmm1  # Z(20)       
       movaps 192(%eax), %xmm2  # Z(12)       
       movaps 448(%eax), %xmm3  # Z(28)
       
       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0  # Z(4)-Z(20) -> xmm0
       addpd %xmm7,%xmm1  # Z(4)+Z(20) -> xmm1
		     
       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2  # Z(12)-Z(28) -> xmm2
       addpd %xmm7,%xmm3  # Z(12)+Z(28) -> xmm3
		     
       movaps %xmm0,%xmm7
       subpd %xmm2,%xmm0  # ZC2o -> xmm0
       addpd %xmm7,%xmm2  # ZD2o -> xmm2
		     
       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1  # ZD2e -> xmm1
       addpd %xmm7,%xmm3  # ZC2e -> xmm3

       movaps    (%eax), %xmm4  # Z(0)
       movaps 256(%eax), %xmm5  # Z(16)
       
       movaps %xmm4,%xmm7
       subpd %xmm5,%xmm4  # ZC0o -> xmm4
       addpd %xmm7,%xmm5  # ZC0e -> xmm5
       
       movaps .C4, %xmm6
       mulpd %xmm6,%xmm0 # C4P16*ZC2o -> xmm0
       
       movaps %xmm4,%xmm7
       subpd %xmm0,%xmm4  # ZC0o-C4P16*ZC2o
       addpd %xmm7,%xmm0  # ZC0o+C4P16*ZC2o

       movaps %xmm4,272(%edx)  # -> ZD(16) # ZD(17)
       movaps %xmm0,256(%edx)  # -> ZD(0)  # ZD(16)

       mulpd %xmm6,%xmm2 # C4P16*ZD2o -> xmm2
       
       movaps 128(%eax), %xmm4  # Z(8)
       movaps 384(%eax), %xmm6  # Z(24)
       
       movaps %xmm4,%xmm7
       subpd %xmm6,%xmm4  # ZD0o -> xmm4
       addpd %xmm7,%xmm6  # ZD0e -> xmm6
       
       movaps %xmm4,%xmm7
       subpd %xmm2,%xmm4  # ZD0o-C4P16*ZD2o
       addpd %xmm7,%xmm2  # ZD0o+C4P16*ZD2o

       movaps %xmm4, 304(%edx)  # -> ZD(24) # ZD(19)
       movaps %xmm2, 288(%edx)  # -> ZD(8) # ZD(18)
       
       movaps %xmm5,%xmm7
       subpd %xmm6,%xmm5  # ZC0e-ZD0e
       addpd %xmm7,%xmm6  # ZC0e+ZD0e

       movaps %xmm6,%xmm7
       subpd %xmm3,%xmm6  # (ZC0e+ZD0e)-ZC2e
       addpd %xmm7,%xmm3  # (ZC0e+ZD0e)+ZC2e

       movaps %xmm1,368(%edx)  # -> ZD(28) # ZD(23)
       movaps %xmm5,352(%edx)  # -> ZD(12) # ZD(22)
       movaps %xmm6,336(%edx)  # -> ZD(20) # ZD(21)
       movaps %xmm3,320(%edx)  # -> ZD(4) # ZD(20)       
       
#-------------------------------------------------------------

       movaps  32(%eax), %xmm0  # Z(2)
       movaps 288(%eax), %xmm1  # Z(18)       
       movaps 224(%eax), %xmm2  # Z(14)       
       movaps 480(%eax), %xmm3  # Z(30)
       
       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0  # Z(2)-Z(18) -> xmm0
       addpd %xmm7,%xmm1  # Z(2)+Z(18) -> xmm1
		     
       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2  # Z(14)-Z(30) -> xmm2
       addpd %xmm7,%xmm3  # Z(14)+Z(30) -> xmm3
		     
       movaps %xmm0,%xmm7
       subpd %xmm2,%xmm0  # ZC1o -> xmm0
       addpd %xmm7,%xmm2  # ZD1o -> xmm2
		     
       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1  # ZD1e -> xmm1
       addpd %xmm7,%xmm3  # ZC1e -> xmm3
       
       movaps  %xmm0, 384(%edx)   # ZC1o -> ZD(2) # ZD(24)
		     
       movaps  96(%eax), %xmm4  # Z(6)
       movaps 352(%eax), %xmm5  # Z(22)
       movaps 160(%eax), %xmm6  # Z(10)
       movaps 416(%eax), %xmm7  # Z(26)
       
       movaps %xmm4,%xmm0
       subpd %xmm5,%xmm4  # Z(6)-Z(22) -> xmm4
       addpd %xmm0,%xmm5  # Z(6)+Z(22) -> xmm5

       movaps %xmm6,%xmm0
       subpd %xmm7,%xmm6  # Z(10)-Z(26) -> xmm6
       addpd %xmm0,%xmm7  # Z(10)+Z(26) -> xmm7

       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4  # ZC3o -> xmm4
       addpd %xmm0,%xmm6  # ZD3o -> xmm6

       movaps %xmm5,%xmm0
       subpd %xmm7,%xmm5  # ZD3e -> xmm5
       addpd %xmm0,%xmm7  # ZC3e -> xmm7

       movaps %xmm3,%xmm0
       subpd %xmm7,%xmm3  # ZC1e-ZC3e
       addpd %xmm0,%xmm7
       movaps  %xmm7, 448(%edx)   # ZC1e+ZC3e -> ZD(6) # ZD(28)

       movaps %xmm1,%xmm0
       subpd %xmm5,%xmm1
       addpd %xmm0,%xmm5 # ZD1e+ZD3e 
       movaps  %xmm1, 464(%edx)   # ZD1e-ZD3e -> ZD(26) # ZD(29)
       
       movaps .C4, %xmm0
       mulpd %xmm0,%xmm3
       mulpd %xmm0,%xmm5       
       movaps  %xmm3, 496(%edx)   # -> ZD(22) # ZD(31)
       movaps  %xmm5, 480(%edx)   # -> ZD(10) # ZD(30)

       movaps .C2, %xmm1
       movaps .S2, %xmm3
       
       movaps %xmm2,%xmm5 # ZD1o
       movaps %xmm6,%xmm7 # ZD3o
       
       mulpd %xmm3,%xmm2 # S2P16*ZD1o
       mulpd %xmm1,%xmm5 # C2P16*ZD1o
      
       mulpd %xmm3,%xmm6 # S2P16*ZD3o
       mulpd %xmm1,%xmm7 # C2P16*ZD3o
       
       addpd %xmm7,%xmm2 # S2P16*ZD1o+C2P16*ZD3o
       subpd %xmm6,%xmm5 # C2P16*ZD1o-S2P16*ZD3o
       
       movaps  %xmm2, 416(%edx)   # ZD(14) # ZD(26)
       movaps  %xmm5, 400(%edx)   # ZD(30) # ZD(25)
       
       movaps  384(%edx), %xmm0    # ZD(2)  # ZD(24)-> ZC1o
       movaps %xmm4,%xmm7 # ZC3o
       movaps %xmm0,%xmm5 # ZC1o
       
       mulpd %xmm3,%xmm4 # S2P16*ZC3o
       mulpd %xmm1,%xmm7 # C2P16*ZC3o
       
       mulpd %xmm3,%xmm0 # S2P16*ZC1o
       mulpd %xmm1,%xmm5 # C2P16*ZC1o
      
       subpd %xmm7,%xmm0 # S2P16*ZC1o-C2P16*ZC3o
       addpd %xmm4,%xmm5 # C2P16*ZC1o+S2P16*ZC3o

       movaps  %xmm0,432(%edx)   # ZD(18) # ZD(27)
       movaps  %xmm5,384(%edx)   # ZD(2) # ZD(24)

#-------------------------------------------------------------
# K=0

       movaps  64(%edx), %xmm0  # ZD(7) # ZD(4)
       movaps 192(%edx), %xmm1  # ZD(5) # ZD(12)       
       
       addpd %xmm1,%xmm0 # ZA

       movaps 320(%edx), %xmm1  # ZD(4) # ZD(20)
       movaps 448(%edx), %xmm3  # ZD(6) # ZD(28)
       
       addpd %xmm1,%xmm3 # ZC

       movapd %xmm3,%xmm4
       subpd %xmm0,%xmm3 # ZC
       addpd %xmm4,%xmm0 # ZA

       movaps %xmm3, 256(%ecx)   # Z(16)
       movaps %xmm0,    (%ecx)   # Z(0)

#-------------------------------------------------------------
# K=1
       movaps .CI, %xmm7
       movaps .C4, %xmm6

       movaps 128(%edx), %xmm0  # ZD(3) # ZD(8)
       movaps 176(%edx), %xmm1  # ZD(19) # ZD(11)       
       movaps 144(%edx), %xmm2  # ZD(29) # ZD(9)
       movaps 160(%edx), %xmm3  # ZD(13) # ZD(10)

       
       addpd %xmm1,%xmm0
       subpd %xmm3,%xmm2
       mulpd %xmm6,%xmm0
       mulpd %xmm6,%xmm2
       
       movaps  (%edx), %xmm1  # ZD(1) # ZD(0)
       movaps 32(%edx), %xmm3  # ZD(15) # ZD(2)
       
       addpd %xmm0,%xmm1     # ZA
       addpd %xmm2,%xmm3
       xorpd %xmm7,%xmm3 # ZB
       shufpd $1,%xmm3,%xmm3        
       
       movaps 256(%edx), %xmm0  # ZD(0) # ZD(16)
       movaps 384(%edx), %xmm2  # ZD(2) # ZD(24)
       
       addpd %xmm2,%xmm0     # ZC

       movaps 416(%edx), %xmm2  # ZD(14) # ZD(26)
       movaps 288(%edx), %xmm4  # ZD(8) # ZD(18)

       addpd %xmm4,%xmm2     
       xorpd %xmm7,%xmm2    # ZD
       shufpd $1,%xmm2,%xmm2        

       movapd %xmm0,%xmm4
       subpd %xmm1,%xmm0 # ZC
       addpd %xmm4,%xmm1 # ZA

       movapd %xmm3,%xmm4
       subpd %xmm2,%xmm3 # ZD
       addpd %xmm4,%xmm2 # ZB
       
       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # Z(31)
       addpd %xmm4,%xmm2 # Z(1)

       movapd %xmm0,%xmm4
       subpd %xmm3,%xmm0 # Z(17)
       addpd %xmm4,%xmm3 # Z(15)

       movaps %xmm1, 496(%ecx)   # Z(31)
       movaps %xmm2,  16(%ecx)   # Z(1)
       movaps %xmm0, 272(%ecx)   # Z(17)
       movaps %xmm3, 240(%ecx)   # Z(15)       

#-------------------------------------------------------------
# K=2
       movaps .C2,%xmm5
       movaps .S2,%xmm6

       movaps 112(%edx), %xmm0  # ZD(23) # ZD(7)
       movaps 240(%edx), %xmm1  # ZD(21) # ZD(15)
       movaps  96(%edx), %xmm2  # ZD(9) # ZD(6)       
       movaps 224(%edx), %xmm3  # ZD(11) # ZD(14)       
       
       mulpd %xmm5,%xmm0
       mulpd %xmm6,%xmm1
       addpd %xmm1,%xmm0 # ZA
       
       mulpd %xmm6,%xmm2
       mulpd %xmm5,%xmm3
       addpd %xmm3,%xmm2
       xorpd %xmm7,%xmm2 # ZB
       shufpd $1,%xmm2,%xmm2       

       movaps  352(%edx), %xmm1  # ZD(12) # ZD(22)
       movaps  496(%edx), %xmm3  # ZD(22) # ZD(31)
       addpd %xmm3,%xmm1 # ZC

       movaps  368(%edx), %xmm4  # ZD(28) # ZD(23)
       movaps  480(%edx), %xmm3  # ZD(10) # ZD(30)
       addpd %xmm4,%xmm3
       xorpd %xmm7,%xmm3 # ZD
       shufpd $1,%xmm3,%xmm3       
       
       movapd %xmm1,%xmm4
       subpd %xmm0,%xmm1 # ZC
       addpd %xmm4,%xmm0 # ZA

       movapd %xmm2,%xmm4
       subpd %xmm3,%xmm2 # ZD
       addpd %xmm4,%xmm3 # ZB
       
       movapd %xmm0,%xmm4
       subpd %xmm3,%xmm0 # ZA-ZB
       addpd %xmm4,%xmm3 # ZA+ZB

       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # ZC-ZD
       addpd %xmm4,%xmm2 # ZC+ZD

       movaps %xmm0, 480(%ecx)   # Z(30)
       movaps %xmm3,  32(%ecx)   # Z(2)       
       movaps %xmm1, 288(%ecx)   # Z(18)       
       movaps %xmm2, 224(%ecx)   # Z(14)

#-------------------------------------------------------------
# K=3
       movaps .C4, %xmm6

       movaps 48(%edx), %xmm0  # ZD(17) # ZD(3)
       movaps  (%edx), %xmm1  # ZD(1) # ZD(0)
       movaps  16(%edx), %xmm2  # ZD(31) # ZD(1)
       movaps  32(%edx), %xmm3  # ZD(15) # ZD(2)       
       
       addpd %xmm0,%xmm1
       subpd %xmm3,%xmm2
       mulpd %xmm6,%xmm1
       mulpd %xmm6,%xmm2

       movaps 144(%edx), %xmm3  # ZD(29) # ZD(9)
       movaps 176(%edx), %xmm0  # ZD(19) # ZD(11)

       
       subpd %xmm0,%xmm1     # ZA
       addpd %xmm2,%xmm3
       xorpd %xmm7,%xmm3 # ZB
       shufpd $1,%xmm3,%xmm3        
       
       movaps 272(%edx), %xmm0  # ZD(16) # ZD(17)
       movaps 432(%edx), %xmm2  # ZD(18) # ZD(27)
       
       addpd %xmm2,%xmm0     # ZC

       movaps 304(%edx), %xmm4  # ZD(24) # ZD(19)
       movaps 400(%edx), %xmm2  # ZD(30) # ZD(25)

       subpd %xmm4,%xmm2     
       xorpd %xmm7,%xmm2    # ZD
       shufpd $1,%xmm2,%xmm2        

       movapd %xmm0,%xmm4
       subpd %xmm1,%xmm0 # ZC
       addpd %xmm4,%xmm1 # ZA

       movapd %xmm3,%xmm4
       subpd %xmm2,%xmm3 # ZD
       addpd %xmm4,%xmm2 # ZB
       
       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # Z(29)
       addpd %xmm4,%xmm2 # Z(3)

       movapd %xmm0,%xmm4
       subpd %xmm3,%xmm0 # Z(19)
       addpd %xmm4,%xmm3 # Z(13)

       movaps %xmm1, 464(%ecx)   # Z(29)       
       movaps %xmm2,  48(%ecx)   # Z(3)
       movaps %xmm0, 304(%ecx)   # Z(19)       
       movaps %xmm3, 208(%ecx)   # Z(13)

#-------------------------------------------------------------
# K=4

       movaps  64(%edx), %xmm0  # ZD(7) # ZD(4)
       movaps 192(%edx), %xmm1  # ZD(5) # ZD(12)       
       movaps  80(%edx), %xmm2  # ZD(25) # ZD(5)       
       movaps 208(%edx), %xmm3  # ZD(27) # ZD(13)
       
       subpd %xmm1,%xmm0 
       mulpd %xmm6,%xmm0 # ZA
       addpd %xmm3,%xmm2
       mulpd %xmm6,%xmm2 
       xorpd %xmm7,%xmm2 # ZB
       shufpd $1,%xmm2,%xmm2       

       movaps  464(%edx), %xmm1  # ZD(26) # ZD(29)
       movaps  336(%edx), %xmm3  # ZD(20) # ZD(21)=ZC
       xorpd %xmm7,%xmm1 # ZD
       shufpd $1,%xmm1,%xmm1       
       
       movapd %xmm3,%xmm4
       subpd %xmm0,%xmm3 # ZC
       addpd %xmm4,%xmm0 # ZA

       movapd %xmm2,%xmm4
       subpd %xmm1,%xmm2 # ZD
       addpd %xmm4,%xmm1 # ZB
       
       movapd %xmm0,%xmm4
       subpd %xmm1,%xmm0 # ZB
       addpd %xmm4,%xmm1 # ZA

       movapd %xmm3,%xmm4
       subpd %xmm2,%xmm3 # ZD
       addpd %xmm4,%xmm2 # ZC

       movaps %xmm0, 448(%ecx)   # Z(28)       
       movaps %xmm1,  64(%ecx)   # Z(4)
       movaps %xmm3, 320(%ecx)   # Z(20)
       movaps %xmm2, 192(%ecx)   # Z(12)       



#-------------------------------------------------------------
# K=5

       movaps  48(%edx), %xmm0  # ZD(17) # ZD(3)
       movaps    (%edx), %xmm1  # ZD(1) # ZD(0)
       movaps  16(%edx), %xmm2  # ZD(31) # ZD(1)
       movaps  32(%edx), %xmm3  # ZD(15) # ZD(2)       
       
       subpd %xmm0,%xmm1
       addpd %xmm3,%xmm2
       mulpd %xmm6,%xmm1
       mulpd %xmm6,%xmm2

       movaps 128(%edx), %xmm0  # ZD(3) # ZD(8)
       movaps 160(%edx), %xmm3  # ZD(13) # ZD(10)
       
       subpd %xmm0,%xmm1     # ZA
       addpd %xmm2,%xmm3
       xorpd %xmm7,%xmm3 # ZB
       shufpd $1,%xmm3,%xmm3        

       movaps 272(%edx), %xmm0  # ZD(16) # ZD(17)
       movaps 432(%edx), %xmm2  # ZD(18) # ZD(27)
       
       subpd %xmm2,%xmm0     # ZC

       movaps 304(%edx), %xmm4  # ZD(24) # ZD(19)
       movaps 400(%edx), %xmm2  # ZD(30) # ZD(25)

       addpd %xmm4,%xmm2     
       xorpd %xmm7,%xmm2    # ZD
       shufpd $1,%xmm2,%xmm2        

       movapd %xmm0,%xmm4
       subpd %xmm1,%xmm0 # ZC
       addpd %xmm4,%xmm1 # ZA

       movapd %xmm3,%xmm4
       subpd %xmm2,%xmm3 # ZD
       addpd %xmm4,%xmm2 # ZB
       
       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # Z(27)
       addpd %xmm4,%xmm2 # Z(5)

       movapd %xmm0,%xmm4
       subpd %xmm3,%xmm0 # Z(21)
       addpd %xmm4,%xmm3 # Z(11)

       movaps %xmm1, 432(%ecx)   # Z(27)
       movaps %xmm2,  80(%ecx)   # Z(5)
       movaps %xmm0, 336(%ecx)   # Z(21)       
       movaps %xmm3, 176(%ecx)   # Z(11)


#-------------------------------------------------------------
# K=6

#       movaps .C2,%xmm5
       movaps .S2,%xmm6

       movaps 112(%edx), %xmm0  # ZD(23) # ZD(7)
       movaps 240(%edx), %xmm1  # ZD(21) # ZD(15)
       movaps  96(%edx), %xmm2  # ZD(9) # ZD(6)       
       movaps 224(%edx), %xmm3  # ZD(11) # ZD(14)       


       mulpd %xmm6,%xmm0
       mulpd %xmm5,%xmm1
       subpd %xmm1,%xmm0 # ZA
       
       mulpd %xmm5,%xmm2
       mulpd %xmm6,%xmm3
       subpd %xmm3,%xmm2
       xorpd %xmm7,%xmm2 # ZB
       shufpd $1,%xmm2,%xmm2       

       movaps  352(%edx), %xmm1  # ZD(12) # ZD(22)
       movaps  496(%edx), %xmm3  # ZD(22) # ZD(31)
       subpd %xmm3,%xmm1 # ZC

       movaps  368(%edx), %xmm4  # ZD(28) # ZD(23)
       movaps  480(%edx), %xmm3  # ZD(10) # ZD(30)
       subpd %xmm4,%xmm3
       xorpd %xmm7,%xmm3 # ZD
       shufpd $1,%xmm3,%xmm3       
       
       movapd %xmm1,%xmm4
       subpd %xmm0,%xmm1 # ZC
       addpd %xmm4,%xmm0 # ZA

       movapd %xmm2,%xmm4
       subpd %xmm3,%xmm2 # ZD
       addpd %xmm4,%xmm3 # ZB
       
       movapd %xmm0,%xmm4
       subpd %xmm3,%xmm0 # ZA-ZB
       addpd %xmm4,%xmm3 # ZA+ZB

       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # ZC-ZD
       addpd %xmm4,%xmm2 # ZC+ZD

       movaps %xmm0, 416(%ecx)   # Z(26)       
       movaps %xmm3,  96(%ecx)   # Z(6)
       movaps %xmm1, 352(%ecx)   # Z(22)
       movaps %xmm2, 160(%ecx)   # Z(10)       

#-------------------------------------------------------------
# K=7
       movaps .C4, %xmm6

       movaps 128(%edx), %xmm0  # ZD(3) # ZD(8)
       movaps 176(%edx), %xmm1  # ZD(19) # ZD(11)       
       movaps 144(%edx), %xmm2  # ZD(29) # ZD(9)       
       movaps 160(%edx), %xmm3  # ZD(13) # ZD(10)
       
       subpd %xmm1,%xmm0
       addpd %xmm3,%xmm2
       mulpd %xmm6,%xmm0
       mulpd %xmm6,%xmm2

       movaps 48(%edx), %xmm1  # ZD(17) # ZD(3)
       movaps 16(%edx), %xmm3  # ZD(31) # ZD(1)
       
       subpd %xmm0,%xmm1     # ZA
       subpd %xmm2,%xmm3
       xorpd %xmm7,%xmm3 # ZB
       shufpd $1,%xmm3,%xmm3        
       
       movaps 256(%edx), %xmm0  # ZD(0) # ZD(16)
       movaps 384(%edx), %xmm2  # ZD(2) # ZD(24)
       
       subpd %xmm2,%xmm0     # ZC

       movaps 288(%edx), %xmm4  # ZD(8) # ZD(18)
       movaps 416(%edx), %xmm2  # ZD(14) # ZD(26)

       subpd %xmm4,%xmm2     
       xorpd %xmm7,%xmm2    # ZD
       shufpd $1,%xmm2,%xmm2        

       movapd %xmm0,%xmm4
       subpd %xmm1,%xmm0 # ZC
       addpd %xmm4,%xmm1 # ZA

       movapd %xmm3,%xmm4
       subpd %xmm2,%xmm3 # ZD
       addpd %xmm4,%xmm2 # ZB
       
       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # Z(25)
       addpd %xmm4,%xmm2 # Z(7)

       movapd %xmm0,%xmm4
       subpd %xmm3,%xmm0 # Z(23)
       addpd %xmm4,%xmm3 # Z(9)

       movaps %xmm1, 400(%ecx)   # Z(25)
       movaps %xmm2, 112(%ecx)   # Z(7)
       movaps %xmm0, 368(%ecx)   # Z(23)       
       movaps %xmm3, 144(%ecx)   # Z(9)


#-------------------------------------------------------------
# K=8

       movaps  80(%edx), %xmm2  # ZD(25) # ZD(5)       
       movaps 208(%edx), %xmm3  # ZD(27) # ZD(13)
       
       subpd %xmm3,%xmm2
       xorpd %xmm7,%xmm2 # ZD
       shufpd $1,%xmm2,%xmm2       

       movaps 320(%edx), %xmm1  # ZD(4) # ZD(20)
       movaps 448(%edx), %xmm3  # ZD(6) # ZD(28)
       
       subpd %xmm3,%xmm1 # ZB
       

       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # ZD
       addpd %xmm4,%xmm2 # ZB

       movaps %xmm1, 384(%ecx)   # Z(24)       
       movaps %xmm2, 128(%ecx)   # Z(8)       

#--------------------------------------------------------
       ret

.align 16
.CI:
      .long 0x0, 0x80000000, 0x0, 0x0
.align 16
.C1:
      .long 0xcff75cb0,0x3fef6297,0xcff75cb0,0x3fef6297
.align 16
.S1:
      .long 0x3c69a60b,0x3fc8f8b8,0x3c69a60b,0x3fc8f8b8
.align 16
.C2:
      .long 0xcf328d46,0x3fed906b,0xcf328d46,0x3fed906b
.align 16
.S2:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2
.align 16
.C4:
      .long 0x667f3bcd,0x3fe6a09e,0x667f3bcd,0x3fe6a09e
