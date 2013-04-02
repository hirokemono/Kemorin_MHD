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
# fft 512 out-of-place backward (16x32 type)
.text
.globl fjcob9_
fjcob9_:

       movl    4(%esp), %eax
       movl    8(%esp), %ecx  # : ZD のベースアドレス
       movl   16(%esp), %edx  # : ZT のベースアドレス
       pushl	%esi
       movl   16(%esp), %esi  # : ZDD のベースアドレス       
       
       pushl	%ebx
       
#------------------------------------
# fftj16 のループ
#------------------------------------

       lea 512(%eax),%ebx

.align 16
.LA:       
       
       movaps 512(%eax), %xmm0
       movaps 2560(%eax), %xmm2       
       movaps 4608(%eax), %xmm1
       movaps 6656(%eax), %xmm3

       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0
       addpd %xmm7,%xmm1

       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2
       addpd %xmm7,%xmm3

       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1
       addpd %xmm7,%xmm3

       movaps %xmm3,16(%ecx)
       
#-------------

       movaps 1536(%eax), %xmm5
       movaps 3584(%eax), %xmm6       
       movaps 5632(%eax), %xmm3
       movaps 7680(%eax), %xmm4

       movaps %xmm5,%xmm7
       subpd %xmm3,%xmm5
       addpd %xmm7,%xmm3

       movaps %xmm6,%xmm7
       subpd %xmm4,%xmm6
       addpd %xmm7,%xmm4

       movaps %xmm3,%xmm7
       subpd %xmm4,%xmm3
       addpd %xmm7,%xmm4

       movaps %xmm1,48(%ecx)
       movaps %xmm3,176(%ecx)       
       movaps %xmm4,144(%ecx)              

#-------------

       movaps .C2,%xmm1
       movaps .S2,%xmm3

       movaps %xmm0,%xmm7
       subpd %xmm6,%xmm0
       addpd %xmm7,%xmm6

       movaps %xmm5,%xmm7
       subpd %xmm2,%xmm5
       addpd %xmm7,%xmm2

       
       movaps %xmm1,%xmm7
       movaps %xmm3,%xmm4
       
       mulpd %xmm5,%xmm7
       mulpd %xmm0,%xmm4       
       subpd %xmm7,%xmm4
       
       mulpd %xmm1,%xmm0
       mulpd %xmm3,%xmm5
       addpd %xmm5,%xmm0

       movaps %xmm1,%xmm7

       mulpd %xmm6,%xmm7
       mulpd %xmm3,%xmm6
       
       mulpd %xmm2,%xmm3
       subpd %xmm3,%xmm7

       mulpd %xmm1,%xmm2
       addpd %xmm2,%xmm6

       movaps %xmm0,112(%ecx)
       movaps %xmm4,80(%ecx)
       movaps %xmm6,240(%ecx)              
       movaps %xmm7,208(%ecx)

       
#-----------------------------------

       movaps (%eax), %xmm0
       movaps 4096(%eax), %xmm3
       movaps 2048(%eax), %xmm2
       movaps 6144(%eax), %xmm1
       
       movaps %xmm0,%xmm7
       subpd %xmm3,%xmm0       
       addpd %xmm7,%xmm3

       movaps %xmm2,%xmm7
       subpd %xmm1,%xmm2
       addpd %xmm7,%xmm1
       
       movaps %xmm3,%xmm7
       subpd %xmm1,%xmm3
       addpd %xmm7,%xmm1

       movaps %xmm1,(%ecx)
       
#-----------------------------------

       movaps 1024(%eax), %xmm5
       movaps 3072(%eax), %xmm6       
       movaps 5120(%eax), %xmm4
       movaps 7168(%eax), %xmm1

       movaps %xmm5,%xmm7
       subpd %xmm4,%xmm5
       addpd %xmm7,%xmm4
       
       movaps %xmm6,%xmm7
       subpd %xmm1,%xmm6
       addpd %xmm7,%xmm1
       
       movaps %xmm4,%xmm7
       subpd %xmm1,%xmm4
       addpd %xmm7,%xmm1
       
       movaps %xmm5,%xmm7
       subpd %xmm6,%xmm5
       addpd %xmm7,%xmm6
       
       movaps %xmm1,128(%ecx)
       
#-----------------------------------

       movaps .C4,%xmm1
       mulpd %xmm1,%xmm5
       mulpd %xmm1,%xmm6       
       movaps %xmm0,%xmm7
       subpd %xmm5,%xmm0
       addpd %xmm7,%xmm5

       movaps %xmm2,%xmm7
       subpd %xmm6,%xmm2
       addpd %xmm7,%xmm6
       
       movaps %xmm0,64(%ecx)
       movaps %xmm2,192(%ecx)
       
#-----------------------------------

       movaps 48(%ecx), %xmm2
       movaps 176(%ecx), %xmm7       
       
       mulpd %xmm1,%xmm2
       mulpd %xmm1,%xmm7
       
       movaps .CI,%xmm1

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm1,%xmm7

       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm1,%xmm4

       movaps %xmm3,%xmm0
       subpd %xmm2,%xmm3
       addpd %xmm0,%xmm2

       movaps %xmm7,%xmm0
       subpd %xmm4,%xmm7
       addpd %xmm0,%xmm4
       
       movaps %xmm2,%xmm0
       subpd %xmm4,%xmm2
       addpd %xmm0,%xmm4

       movaps %xmm3,%xmm0
       subpd %xmm7,%xmm3
       addpd %xmm0,%xmm7
       
       
       movaps %xmm4,32(%ecx)              
       movaps %xmm7,96(%ecx)
       movaps %xmm3,160(%ecx)
       movaps %xmm2,224(%ecx)       
       
#-----------------------------------       
       
       movaps 16(%ecx), %xmm2
       movaps 112(%ecx), %xmm4
       movaps 144(%ecx), %xmm7       
       movaps 240(%ecx), %xmm3
       
       movaps %xmm5,%xmm0
       subpd %xmm4,%xmm5
       addpd %xmm0,%xmm4
       
       movaps %xmm3,%xmm0
       subpd %xmm6,%xmm3
       addpd %xmm0,%xmm6
       shufpd $0x1,%xmm3,%xmm3
       xorpd %xmm1,%xmm3
       shufpd $0x1,%xmm6,%xmm6
       xorpd %xmm1,%xmm6

       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4
       addpd %xmm0,%xmm6
       
       movaps %xmm5,%xmm0
       subpd %xmm3,%xmm5
       addpd %xmm0,%xmm3
       

       movaps %xmm6,16(%ecx)
       movaps %xmm3,112(%ecx)
       movaps %xmm5,144(%ecx)
       movaps %xmm4,240(%ecx)       
       
#-----------------------------------

       movaps (%ecx), %xmm3
       movaps 64(%ecx), %xmm4
       movaps 128(%ecx), %xmm5
       movaps 192(%ecx), %xmm6

       movaps %xmm3,%xmm0
       subpd %xmm5,%xmm3
       addpd %xmm0,%xmm5

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7

       shufpd $0x1,%xmm2,%xmm2
       xorpd %xmm1,%xmm2

       movaps %xmm5,%xmm0
       subpd %xmm7,%xmm5
       addpd %xmm0,%xmm7
       
       movaps %xmm3,%xmm0
       subpd %xmm2,%xmm3
       addpd %xmm0,%xmm2
       

       movaps %xmm7,(%ecx)
       movaps %xmm2,64(%ecx)
       movaps %xmm5,128(%ecx)       
       movaps %xmm3,192(%ecx)
       
#-----------------------------------       
       
       movaps 80(%ecx), %xmm3
       movaps 208(%ecx), %xmm5
       
       movaps %xmm4,%xmm0
       subpd %xmm3,%xmm4
       addpd %xmm0,%xmm3

       movaps %xmm5,%xmm0
       subpd %xmm6,%xmm5
       addpd %xmm0,%xmm6

       shufpd $0x1,%xmm5,%xmm5
       xorpd %xmm1,%xmm5
       shufpd $0x1,%xmm6,%xmm6
       xorpd %xmm1,%xmm6
       
       movaps %xmm3,%xmm0
       subpd %xmm5,%xmm3
       addpd %xmm0,%xmm5
       
       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4
       addpd %xmm0,%xmm6
       

       movaps %xmm5,48(%ecx)
       movaps %xmm6,80(%ecx)
       movaps %xmm4,176(%ecx)
       movaps %xmm3,208(%ecx)
       
       addl $16,%eax
       addl $256,%ecx
       cmpl %eax,%ebx
       jne .LA

       subl $512,%eax
       subl $8192,%ecx
       
#       jmp .LE
#-------------------------------------------------------------------------       
#------------------------------------
# fftj32new3
#------------------------------------

#       movl %eax,%esi # eax を esi に退避
#       movl %ecx,%eax # ecx を eax に
#       movl %esi,%ecx # eax と ecx を交換
       lea 256(%ecx),%ebx

.LB:
#------------------------------------
       movaps   256(%ecx), %xmm0  # Z(1) !
       movaps  4352(%ecx), %xmm1  # Z(17) !
       movaps  3840(%ecx), %xmm2  # Z(15) !
       movaps  7936(%ecx), %xmm3  # Z(31) !
       
###
       movaps %xmm0,%xmm7
       shufpd $1,%xmm0,%xmm0
       mulpd 32(%edx),%xmm7
       mulpd 48(%edx),%xmm0
       addpd %xmm7,%xmm0
###       
###
       movaps %xmm1,%xmm7
       shufpd $1,%xmm1,%xmm1
       mulpd 544(%edx),%xmm7
       mulpd 560(%edx),%xmm1
       addpd %xmm7,%xmm1
###       
###
       movaps %xmm2,%xmm7
       shufpd $1,%xmm2,%xmm2
       mulpd 480(%edx),%xmm7
       mulpd 496(%edx),%xmm2
       addpd %xmm7,%xmm2
###       
###
       movaps %xmm3,%xmm7
       shufpd $1,%xmm3,%xmm3
       mulpd 992(%edx),%xmm7
       mulpd 1008(%edx),%xmm3
       addpd %xmm7,%xmm3
###    
       
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
       
       movaps  %xmm0, (%esi)   # ZA1o -> ZT(1) # ZT(0)
		     
       movaps   1792(%ecx), %xmm4  # Z(7) !
       movaps   5888(%ecx), %xmm5  # Z(23) !
       movaps   2304(%ecx), %xmm6  # Z(9) !
       movaps   6400(%ecx), %xmm7  # Z(25) !

###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 224(%edx),%xmm0
       mulpd 240(%edx),%xmm4
       addpd %xmm0,%xmm4
###       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 736(%edx),%xmm0
       mulpd 752(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm6,%xmm0
       shufpd $1,%xmm6,%xmm6
       mulpd 288(%edx),%xmm0
       mulpd 304(%edx),%xmm6
       addpd %xmm0,%xmm6
###       
###
       movaps %xmm7,%xmm0
       shufpd $1,%xmm7,%xmm7
       mulpd 800(%edx),%xmm0
       mulpd 816(%edx),%xmm7
       addpd %xmm0,%xmm7
###       

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
       movaps  %xmm3, 112(%esi)   # ZA1e-ZA4e -> ZT(23) # ZT(7)
       movaps  %xmm7,  64(%esi)   # ZA1e+ZA4e -> ZT(7) # ZT(4)

       movaps %xmm1,%xmm0
       subpd %xmm5,%xmm1
       addpd %xmm0,%xmm5
       movaps  %xmm1,  80(%esi)   # ZB1e-ZB4e -> ZT(25) # ZT(5)
       movaps  %xmm5,  96(%esi)   # ZB1e+ZB4e -> ZT(9) # ZT(6)
       
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
       
       movaps  %xmm2, 32(%esi)   # ZT(15) # ZT(2)
       movaps  %xmm5, 16(%esi)   # ZT(31) # ZT(1)
       
       movaps  (%esi), %xmm0    # ZT(1) # ZT(0) -> ZA1o 
       movaps %xmm4,%xmm7 # ZA4o
       movaps %xmm0,%xmm5 # ZA1o
       
       mulpd %xmm3,%xmm4 # S1P16*ZA4o
       mulpd %xmm1,%xmm7 # C1P16*ZA4o
       
       mulpd %xmm3,%xmm0 # S1P16*ZA1o
       mulpd %xmm1,%xmm5 # C1P16*ZA1o
      
       subpd %xmm7,%xmm0 # S1P16*ZA1o-C1P16*ZA4o
       addpd %xmm4,%xmm5 # C1P16*ZA1o+S1P16*ZA4o

       movaps  %xmm0,48(%esi)   # ZT(17) # ZT(3)
       movaps  %xmm5, (%esi)   # ZT(1) # ZT(0)

#---------------------------------------
       
       movaps   768(%ecx), %xmm0  # Z(3) !
       movaps  4864(%ecx), %xmm1  # Z(19) !
       movaps  3328(%ecx), %xmm2  # Z(13) !
       movaps  7424(%ecx), %xmm3  # Z(29) !

###
       movaps %xmm0,%xmm7
       shufpd $1,%xmm0,%xmm0
       mulpd  96(%edx),%xmm7
       mulpd 112(%edx),%xmm0
       addpd %xmm7,%xmm0
###       
###
       movaps %xmm1,%xmm7
       shufpd $1,%xmm1,%xmm1
       mulpd 608(%edx),%xmm7
       mulpd 624(%edx),%xmm1
       addpd %xmm7,%xmm1
###       
###
       movaps %xmm2,%xmm7
       shufpd $1,%xmm2,%xmm2
       mulpd 416(%edx),%xmm7
       mulpd 432(%edx),%xmm2
       addpd %xmm7,%xmm2
###       
###
       movaps %xmm3,%xmm7
       shufpd $1,%xmm3,%xmm3
       mulpd 928(%edx),%xmm7
       mulpd 944(%edx),%xmm3
       addpd %xmm7,%xmm3
###

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
       
       movaps  %xmm0, 128(%esi)   # ZA2o -> ZT(3) # ZT(8)
		     
       movaps  1280(%ecx), %xmm4  # Z(5) !
       movaps  5376(%ecx), %xmm5  # Z(21) !
       movaps  2816(%ecx), %xmm6  # Z(11) !
       movaps  6912(%ecx), %xmm7  # Z(27) !
       
###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 160(%edx),%xmm0
       mulpd 176(%edx),%xmm4
       addpd %xmm0,%xmm4
###       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 672(%edx),%xmm0
       mulpd 688(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm6,%xmm0
       shufpd $1,%xmm6,%xmm6
       mulpd 352(%edx),%xmm0
       mulpd 368(%edx),%xmm6
       addpd %xmm0,%xmm6
###       
###
       movaps %xmm7,%xmm0
       shufpd $1,%xmm7,%xmm7
       mulpd 864(%edx),%xmm0
       mulpd 880(%edx),%xmm7
       addpd %xmm0,%xmm7
###       

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
       movaps  %xmm3, 240(%esi)   # ZA2e-ZA3e -> ZT(21) # ZT(15)
       movaps  %xmm7, 192(%esi)   # ZA2e+ZA3e -> ZT(5) # ZT(12)

       movaps %xmm1,%xmm0
       subpd %xmm5,%xmm1
       addpd %xmm0,%xmm5
       movaps  %xmm1, 208(%esi)   # ZB2e-ZB3e -> ZT(27) # ZT(13)
       movaps  %xmm5, 224(%esi)   # ZB2e+ZB3e -> ZT(11) # ZT(14)
       
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
       
       movaps  %xmm2, 160(%esi)   # ZT(13) # ZT(10)
       movaps  %xmm5, 144(%esi)   # ZT(29) # ZT(9)
       
       movaps 128(%esi), %xmm0    # ZT(3) # ZT(8) -> ZA2o
       movaps %xmm4,%xmm7 # ZA3o
       movaps %xmm0,%xmm5 # ZA2o
       
       mulpd %xmm3,%xmm4 # S1P16*ZA3o
       mulpd %xmm1,%xmm7 # C1P16*ZA3o
       
       mulpd %xmm3,%xmm0 # S1P16*ZA2o
       mulpd %xmm1,%xmm5 # C1P16*ZA2o
      
       addpd %xmm7,%xmm0 # S1P16*ZA1o+C1P16*ZA4o
       subpd %xmm4,%xmm5 # C1P16*ZA1o-S1P16*ZA4o

       movaps  %xmm0,176(%esi)   # ZT(19) # ZT(11)
       movaps  %xmm5, 128(%esi)   # ZT(3) # ZT(8)
       
#---------------------------------------
       
       movaps  1024(%ecx), %xmm0  # Z(4) !
       movaps  5120(%ecx), %xmm1  # Z(20) !
       movaps  3072(%ecx), %xmm2  # Z(12) !
       movaps  7168(%ecx), %xmm3  # Z(28) !

###
       movaps %xmm0,%xmm7
       shufpd $1,%xmm0,%xmm0
       mulpd  128(%edx),%xmm7
       mulpd  144(%edx),%xmm0
       addpd %xmm7,%xmm0
###       
###
       movaps %xmm1,%xmm7
       shufpd $1,%xmm1,%xmm1
       mulpd 640(%edx),%xmm7
       mulpd 656(%edx),%xmm1
       addpd %xmm7,%xmm1
###       
###
       movaps %xmm2,%xmm7
       shufpd $1,%xmm2,%xmm2
       mulpd 384(%edx),%xmm7
       mulpd 400(%edx),%xmm2
       addpd %xmm7,%xmm2
###       
###
       movaps %xmm3,%xmm7
       shufpd $1,%xmm3,%xmm3
       mulpd 896(%edx),%xmm7
       mulpd 912(%edx),%xmm3
       addpd %xmm7,%xmm3
###    

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

       movaps     (%ecx), %xmm4  # Z(0) !
       movaps 4096(%ecx), %xmm5  # Z(16) !
       
###
       movaps %xmm5,%xmm7
       shufpd $1,%xmm5,%xmm5
       mulpd 512(%edx),%xmm7
       mulpd 528(%edx),%xmm5
       addpd %xmm7,%xmm5
###    
       movaps %xmm4,%xmm7
       subpd %xmm5,%xmm4  # ZC0o -> xmm4
       addpd %xmm7,%xmm5  # ZC0e -> xmm5
       
       movaps .C4, %xmm6
       mulpd %xmm6,%xmm0 # C4P16*ZC2o -> xmm0
       
       movaps %xmm4,%xmm7
       subpd %xmm0,%xmm4  # ZC0o-C4P16*ZC2o
       addpd %xmm7,%xmm0  # ZC0o+C4P16*ZC2o

       movaps %xmm4,272(%esi)  # -> ZT(16) # ZT(17)
       movaps %xmm0,256(%esi)  # -> ZT(0)  # ZT(16)

       mulpd %xmm6,%xmm2 # C4P16*ZD2o -> xmm2
       
       movaps  2048(%ecx), %xmm4  # Z(8) !
       movaps  6144(%ecx), %xmm6  # Z(24) !

###
       movaps %xmm4,%xmm7
       shufpd $1,%xmm4,%xmm4
       mulpd 256(%edx),%xmm7
       mulpd 272(%edx),%xmm4
       addpd %xmm7,%xmm4
###       
###
       movaps %xmm6,%xmm7
       shufpd $1,%xmm6,%xmm6
       mulpd 768(%edx),%xmm7
       mulpd 784(%edx),%xmm6
       addpd %xmm7,%xmm6
###    

       movaps %xmm4,%xmm7
       subpd %xmm6,%xmm4  # ZD0o -> xmm4
       addpd %xmm7,%xmm6  # ZD0e -> xmm6
       
       movaps %xmm4,%xmm7
       subpd %xmm2,%xmm4  # ZD0o-C4P16*ZD2o
       addpd %xmm7,%xmm2  # ZD0o+C4P16*ZD2o

       movaps %xmm4, 304(%esi)  # -> ZT(24) # ZT(19)
       movaps %xmm2, 288(%esi)  # -> ZT(8) # ZT(18)
       
       movaps %xmm5,%xmm7
       subpd %xmm6,%xmm5  # ZC0e-ZD0e
       addpd %xmm7,%xmm6  # ZC0e+ZD0e

       movaps %xmm6,%xmm7
       subpd %xmm3,%xmm6  # (ZC0e+ZD0e)-ZC2e
       addpd %xmm7,%xmm3  # (ZC0e+ZD0e)+ZC2e

       movaps %xmm1,368(%esi)  # -> ZT(28) # ZT(23)
       movaps %xmm5,352(%esi)  # -> ZT(12) # ZT(22)
       movaps %xmm6,336(%esi)  # -> ZT(20) # ZT(21)
       movaps %xmm3,320(%esi)  # -> ZT(4) # ZT(20)       
       
#-------------------------------------------------------------

       movaps   512(%ecx), %xmm0  # Z(2) !
       movaps  4608(%ecx), %xmm1  # Z(18) !
       movaps  3584(%ecx), %xmm2  # Z(14) !
       movaps  7680(%ecx), %xmm3  # Z(30) !

###
       movaps %xmm0,%xmm7
       shufpd $1,%xmm0,%xmm0
       mulpd  64(%edx),%xmm7
       mulpd  80(%edx),%xmm0
       addpd %xmm7,%xmm0
###       
###
       movaps %xmm1,%xmm7
       shufpd $1,%xmm1,%xmm1
       mulpd 576(%edx),%xmm7
       mulpd 592(%edx),%xmm1
       addpd %xmm7,%xmm1
###       
###
       movaps %xmm2,%xmm7
       shufpd $1,%xmm2,%xmm2
       mulpd 448(%edx),%xmm7
       mulpd 464(%edx),%xmm2
       addpd %xmm7,%xmm2
###       
###
       movaps %xmm3,%xmm7
       shufpd $1,%xmm3,%xmm3
       mulpd 960(%edx),%xmm7
       mulpd 976(%edx),%xmm3
       addpd %xmm7,%xmm3
###       

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
       
       movaps  %xmm0, 384(%esi)   # ZC1o -> ZT(2) # ZT(24)
		     
       movaps  1536(%ecx), %xmm4  # Z(6) !
       movaps  5632(%ecx), %xmm5  # Z(22) !
       movaps  2560(%ecx), %xmm6  # Z(10) !
       movaps  6656(%ecx), %xmm7  # Z(26) !

###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 192(%edx),%xmm0
       mulpd 208(%edx),%xmm4
       addpd %xmm0,%xmm4
###       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 704(%edx),%xmm0
       mulpd 720(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm6,%xmm0
       shufpd $1,%xmm6,%xmm6
       mulpd 320(%edx),%xmm0
       mulpd 336(%edx),%xmm6
       addpd %xmm0,%xmm6
###       
###
       movaps %xmm7,%xmm0
       shufpd $1,%xmm7,%xmm7
       mulpd 832(%edx),%xmm0
       mulpd 848(%edx),%xmm7
       addpd %xmm0,%xmm7
###

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
       movaps  %xmm7, 448(%esi)   # ZC1e+ZC3e -> ZT(6) # ZT(28)

       movaps %xmm1,%xmm0
       subpd %xmm5,%xmm1
       addpd %xmm0,%xmm5 # ZD1e+ZD3e 
       movaps  %xmm1, 464(%esi)   # ZD1e-ZD3e -> ZT(26) # ZT(29)
       
       movaps .C4, %xmm0
       mulpd %xmm0,%xmm3
       mulpd %xmm0,%xmm5       
       movaps  %xmm3, 496(%esi)   # -> ZT(22) # ZT(31)
       movaps  %xmm5, 480(%esi)   # -> ZT(10) # ZT(30)

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
       
       movaps  %xmm2, 416(%esi)   # ZT(14) # ZT(26)
       movaps  %xmm5, 400(%esi)   # ZT(30) # ZT(25)
       
       movaps  384(%esi), %xmm0    # ZT(2)  # ZT(24)-> ZC1o
       movaps %xmm4,%xmm7 # ZC3o
       movaps %xmm0,%xmm5 # ZC1o
       
       mulpd %xmm3,%xmm4 # S2P16*ZC3o
       mulpd %xmm1,%xmm7 # C2P16*ZC3o
       
       mulpd %xmm3,%xmm0 # S2P16*ZC1o
       mulpd %xmm1,%xmm5 # C2P16*ZC1o
      
       subpd %xmm7,%xmm0 # S2P16*ZC1o-C2P16*ZC3o
       addpd %xmm4,%xmm5 # C2P16*ZC1o+S2P16*ZC3o

       movaps  %xmm0,432(%esi)   # ZT(18) # ZT(27)
       movaps  %xmm5,384(%esi)   # ZT(2) # ZT(24)
       
#-------------------------------------------------------------       
       
       addl $16,%ecx
       addl $512,%esi
       addl $1024,%edx
       cmpl %ecx,%ebx
       jne .LB
       
       subl $8192,%esi       
       
       subl $256,%ecx

       lea 256(%ecx),%ebx

#.LC:

       movaps .CI, %xmm7
       movaps .C4, %xmm6       

.LK1:
#-------------------------------------------------------------
# K=1

       movaps 128(%esi), %xmm0  # ZT(3) # ZT(8)
       movaps 176(%esi), %xmm1  # ZT(19) # ZT(11)       
       movaps 144(%esi), %xmm2  # ZT(29) # ZT(9)
       movaps 160(%esi), %xmm3  # ZT(13) # ZT(10)
       
       addpd %xmm1,%xmm0
       subpd %xmm3,%xmm2
       mulpd %xmm6,%xmm0
       mulpd %xmm6,%xmm2
       
       movaps  (%esi), %xmm1  # ZT(1) # ZT(0)
       movaps 32(%esi), %xmm3  # ZT(15) # ZT(2)
       
       addpd %xmm0,%xmm1     # ZA
       addpd %xmm2,%xmm3
       shufpd $1,%xmm3,%xmm3 
       xorpd %xmm7,%xmm3 # ZB       
       
       movaps 256(%esi), %xmm0  # ZT(0) # ZT(16)
       movaps 384(%esi), %xmm2  # ZT(2) # ZT(24)
       
       addpd %xmm2,%xmm0     # ZC

       movaps 416(%esi), %xmm2  # ZT(14) # ZT(26)
       movaps 288(%esi), %xmm4  # ZT(8) # ZT(18)

       addpd %xmm4,%xmm2     
       shufpd $1,%xmm2,%xmm2 
       xorpd %xmm7,%xmm2    # ZD  

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

       movaps %xmm2,  256(%ecx)   # Z(1) !
       movaps %xmm1, 7936(%ecx)   # Z(31) !
       movaps %xmm3, 3840(%ecx)   # Z(15) !
       movaps %xmm0, 4352(%ecx)   # Z(17) !
       
#-------------------------------------------------------------
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK1
       
       subl $256,%ecx
       subl $8192,%esi

.LK7:
#-------------------------------------------------------------
# K=7

       movaps 128(%esi), %xmm0  # ZT(3) # ZT(8)
       movaps 176(%esi), %xmm1  # ZT(19) # ZT(11)       
       movaps 144(%esi), %xmm2  # ZT(29) # ZT(9)       
       movaps 160(%esi), %xmm3  # ZT(13) # ZT(10)
       
       subpd %xmm1,%xmm0
       addpd %xmm3,%xmm2
       mulpd %xmm6,%xmm0
       mulpd %xmm6,%xmm2

       movaps 48(%esi), %xmm1  # ZT(17) # ZT(3)
       movaps 16(%esi), %xmm3  # ZT(31) # ZT(1)
       
       subpd %xmm0,%xmm1     # ZA
       subpd %xmm2,%xmm3
       shufpd $1,%xmm3,%xmm3 
       xorpd %xmm7,%xmm3 # ZB       
       
       movaps 256(%esi), %xmm0  # ZT(0) # ZT(16)
       movaps 384(%esi), %xmm2  # ZT(2) # ZT(24)
       
       subpd %xmm2,%xmm0     # ZC

       movaps 288(%esi), %xmm4  # ZT(8) # ZT(18)
       movaps 416(%esi), %xmm2  # ZT(14) # ZT(26)

       subpd %xmm4,%xmm2     
       shufpd $1,%xmm2,%xmm2 
       xorpd %xmm7,%xmm2    # ZD  

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

       movaps %xmm2, 1792(%ecx)   # Z(7) !
       movaps %xmm1, 6400(%ecx)   # Z(25) !
       movaps %xmm3, 2304(%ecx)   # Z(9) !
       movaps %xmm0, 5888(%ecx)   # Z(23) !

#-------------------------------------------------------------
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK7
       
       subl $256,%ecx
       subl $8192,%esi
       

.LK3:
#-------------------------------------------------------------
# K=3

       movaps 48(%esi), %xmm0  # ZT(17) # ZT(3)
       movaps  (%esi), %xmm1  # ZT(1) # ZT(0)
       movaps  16(%esi), %xmm2  # ZT(31) # ZT(1)
       movaps  32(%esi), %xmm3  # ZT(15) # ZT(2)       
       
       addpd %xmm0,%xmm1
       subpd %xmm3,%xmm2
       mulpd %xmm6,%xmm1
       mulpd %xmm6,%xmm2

       movaps 144(%esi), %xmm3  # ZT(29) # ZT(9)
       movaps 176(%esi), %xmm0  # ZT(19) # ZT(11)

       
       subpd %xmm0,%xmm1     # ZA
       addpd %xmm2,%xmm3
       shufpd $1,%xmm3,%xmm3 
       xorpd %xmm7,%xmm3 # ZB       
       
       movaps 272(%esi), %xmm0  # ZT(16) # ZT(17)
       movaps 432(%esi), %xmm2  # ZT(18) # ZT(27)
       
       addpd %xmm2,%xmm0     # ZC

       movaps 304(%esi), %xmm4  # ZT(24) # ZT(19)
       movaps 400(%esi), %xmm2  # ZT(30) # ZT(25)

       subpd %xmm4,%xmm2     
       shufpd $1,%xmm2,%xmm2 
       xorpd %xmm7,%xmm2    # ZD  

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

       movaps %xmm2,  768(%ecx)   # Z(3) !
       movaps %xmm1, 7424(%ecx)   # Z(29) !
       movaps %xmm3, 3328(%ecx)   # Z(13) !
       movaps %xmm0, 4864(%ecx)   # Z(19) !

#-------------------------------------------------------------
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK3

       subl $256,%ecx
       subl $8192,%esi
       
.LK5:
#-------------------------------------------------------------
# K=5

       movaps  48(%esi), %xmm0  # ZT(17) # ZT(3)
       movaps    (%esi), %xmm1  # ZT(1) # ZT(0)
       movaps  16(%esi), %xmm2  # ZT(31) # ZT(1)
       movaps  32(%esi), %xmm3  # ZT(15) # ZT(2)       
       
       subpd %xmm0,%xmm1
       addpd %xmm3,%xmm2
       mulpd %xmm6,%xmm1
       mulpd %xmm6,%xmm2

       movaps 128(%esi), %xmm0  # ZT(3) # ZT(8)
       movaps 160(%esi), %xmm3  # ZT(13) # ZT(10)
       
       subpd %xmm0,%xmm1     # ZA
       addpd %xmm2,%xmm3
       shufpd $1,%xmm3,%xmm3 
       xorpd %xmm7,%xmm3 # ZB       

       movaps 272(%esi), %xmm0  # ZT(16) # ZT(17)
       movaps 432(%esi), %xmm2  # ZT(18) # ZT(27)
       
       subpd %xmm2,%xmm0     # ZC

       movaps 304(%esi), %xmm4  # ZT(24) # ZT(19)
       movaps 400(%esi), %xmm2  # ZT(30) # ZT(25)

       addpd %xmm4,%xmm2     
       shufpd $1,%xmm2,%xmm2 
       xorpd %xmm7,%xmm2    # ZD  

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

       movaps %xmm2,  1280(%ecx)   # Z(5) !
       movaps %xmm1,  6912(%ecx)   # Z(27) !
       movaps %xmm3,  2816(%ecx)   # Z(11) !
       movaps %xmm0,  5376(%ecx)   # Z(21) !

#-------------------------------------------------------------
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK5

       subl $256,%ecx
       subl $8192,%esi

.LK0:
#-------------------------------------------------------------
# K=0,8

       movaps  64(%esi), %xmm0  # ZT(7) # ZT(4)
       movaps 192(%esi), %xmm1  # ZT(5) # ZT(12)
       movaps  80(%esi), %xmm2  # ZT(25) # ZT(5)       
       movaps 208(%esi), %xmm3  # ZT(27) # ZT(13)
       
       subpd %xmm3,%xmm2
       shufpd $1,%xmm2,%xmm2
       xorpd %xmm7,%xmm2 # ZD

       addpd %xmm1,%xmm0 # ZA

       movaps 320(%esi), %xmm1  # ZT(4) # ZT(20)
       movaps 448(%esi), %xmm3  # ZT(6) # ZT(28)
       
       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1 # ZB       
       addpd %xmm4,%xmm3 # ZC

       movapd %xmm3,%xmm4
       subpd %xmm0,%xmm3 # ZC
       addpd %xmm4,%xmm0 # ZA
       
       movapd %xmm1,%xmm4
       subpd %xmm2,%xmm1 # ZD
       addpd %xmm4,%xmm2 # ZB

       movaps %xmm0,      (%ecx)   # Z(0) !
       movaps %xmm3,  4096(%ecx)   # Z(16) !
       movaps %xmm2,  2048(%ecx)   # Z(8) !
       movaps %xmm1,  6144(%ecx)   # Z(24) !

#-------------------------------------------------------------       
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK0
       
       subl $256,%ecx
       subl $8192,%esi

       
.LK4:
#-------------------------------------------------------------
# K=4

       movaps  64(%esi), %xmm0  # ZT(7) # ZT(4)
       movaps 192(%esi), %xmm1  # ZT(5) # ZT(12)       
       movaps  80(%esi), %xmm2  # ZT(25) # ZT(5)       
       movaps 208(%esi), %xmm3  # ZT(27) # ZT(13)
       
       subpd %xmm1,%xmm0 
       mulpd %xmm6,%xmm0 # ZA
       addpd %xmm3,%xmm2
       mulpd %xmm6,%xmm2 
       shufpd $1,%xmm2,%xmm2
       xorpd %xmm7,%xmm2 # ZB

       movaps  464(%esi), %xmm1  # ZT(26) # ZT(29)
       movaps  336(%esi), %xmm3  # ZT(20) # ZT(21)=ZC
       shufpd $1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1 # ZD
       
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

       movaps %xmm1,  1024(%ecx)   # Z(4) !
       movaps %xmm0,  7168(%ecx)   # Z(28) !
       movaps %xmm2,  3072(%ecx)   # Z(12) !
       movaps %xmm3,  5120(%ecx)   # Z(20) !

#-------------------------------------------------------------
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK4

       subl $256,%ecx
       subl $8192,%esi

#-------------------------------------------------------------
       movaps .C2,%xmm5
       movaps .S2,%xmm6
.LK2:       
#-------------------------------------------------------------
# K=2

       movaps 112(%esi), %xmm0  # ZT(23) # ZT(7)
       movaps 240(%esi), %xmm1  # ZT(21) # ZT(15)
       movaps  96(%esi), %xmm2  # ZT(9) # ZT(6)       
       movaps 224(%esi), %xmm3  # ZT(11) # ZT(14)       
       
       mulpd %xmm5,%xmm0
       mulpd %xmm6,%xmm1
       addpd %xmm1,%xmm0 # ZA
       
       mulpd %xmm6,%xmm2
       mulpd %xmm5,%xmm3
       addpd %xmm3,%xmm2
       shufpd $1,%xmm2,%xmm2
       xorpd %xmm7,%xmm2 # ZB

       movaps  352(%esi), %xmm1  # ZT(12) # ZT(22)
       movaps  496(%esi), %xmm3  # ZT(22) # ZT(31)
       addpd %xmm3,%xmm1 # ZC

       movaps  368(%esi), %xmm4  # ZT(28) # ZT(23)
       movaps  480(%esi), %xmm3  # ZT(10) # ZT(30)
       addpd %xmm4,%xmm3
       shufpd $1,%xmm3,%xmm3
       xorpd %xmm7,%xmm3 # ZD
       
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

       movaps %xmm3,  512(%ecx)   # Z(2) !
       movaps %xmm0, 7680(%ecx)   # Z(30) !
       movaps %xmm2, 3584(%ecx)   # Z(14) !
       movaps %xmm1, 4608(%ecx)   # Z(18) !
       
#-------------------------------------------------------------
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK2

       subl $256,%ecx
       subl $8192,%esi

.LK6:
#-------------------------------------------------------------
# K=6

       movaps 112(%esi), %xmm0  # ZT(23) # ZT(7)
       movaps 240(%esi), %xmm1  # ZT(21) # ZT(15)
       movaps  96(%esi), %xmm2  # ZT(9) # ZT(6)       
       movaps 224(%esi), %xmm3  # ZT(11) # ZT(14)       


       mulpd %xmm6,%xmm0
       mulpd %xmm5,%xmm1
       subpd %xmm1,%xmm0 # ZA
       
       mulpd %xmm5,%xmm2
       mulpd %xmm6,%xmm3
       subpd %xmm3,%xmm2
       shufpd $1,%xmm2,%xmm2
       xorpd %xmm7,%xmm2 # ZB

       movaps  352(%esi), %xmm1  # ZT(12) # ZT(22)
       movaps  496(%esi), %xmm3  # ZT(22) # ZT(31)
       subpd %xmm3,%xmm1 # ZC

       movaps  368(%esi), %xmm4  # ZT(28) # ZT(23)
       movaps  480(%esi), %xmm3  # ZT(10) # ZT(30)
       subpd %xmm4,%xmm3
       shufpd $1,%xmm3,%xmm3
       xorpd %xmm7,%xmm3 # ZD
       
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

       movaps %xmm3,  1536(%ecx)   # Z(6) !
       movaps %xmm0,  6656(%ecx)   # Z(26) !
       movaps %xmm2,  2560(%ecx)   # Z(10) !
       movaps %xmm1,  5632(%ecx)   # Z(22) !

#-------------------------------------------------------------
       addl $16,%ecx
       addl $512,%esi
       cmpl %ecx,%ebx
       jne .LK6

#-------------------------------------------------------------
.LE:
       popl %ebx
       popl %esi       	
	
       ret

#--------------------------------------------------
.section .rodata
.align 16
.CI:
      .long 0x0,0x80000000,0x0,0x0
.align 16
.C1:
      .long 0xcff75cb0,0x3fef6297,0xcff75cb0,0x3fef6297
.align 16
.C1D:
      .long 0xcff75cb0,0xbfef6297,0xcff75cb0,0x3fef6297
.align 16
.S1:
      .long 0x3c69a60b,0x3fc8f8b8,0x3c69a60b,0x3fc8f8b8
.align 16
.S1D:
      .long 0x3c69a60b,0xbfc8f8b8,0x3c69a60b,0x3fc8f8b8
.align 16
.C2:
      .long 0xcf328d46,0x3fed906b,0xcf328d46,0x3fed906b
.align 16
.C2D:
      .long 0xcf328d46,0xbfed906b,0xcf328d46,0x3fed906b
.align 16
.S2:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2
.align 16
.S2D:
      .long 0xa6aea963,0xbfd87de2,0xa6aea963,0x3fd87de2
.align 16
.C3:
      .long 0x290ea1a3,0x3fea9b66,0x290ea1a3,0x3fea9b66
.align 16
.C3D:
      .long 0x290ea1a3,0xbfea9b66,0x290ea1a3,0x3fea9b66
.align 16
.S3:
      .long 0x39ae68c8,0x3fe1c73b,0x39ae68c8,0x3fe1c73b
.align 16
.S3D:
      .long 0x39ae68c8,0xbfe1c73b,0x39ae68c8,0x3fe1c73b
.align 16
.C4:
      .long 0x667f3bcd,0x3fe6a09e,0x667f3bcd,0x3fe6a09e
.align 16
.C4D:
      .long 0x667f3bcd,0xbfe6a09e,0x667f3bcd,0x3fe6a09e
