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
# fft 128 out-of-place forward (16x8 type)
.text
.globl fjcof7_
fjcof7_:

       movl    4(%esp), %eax
       movl    8(%esp), %ecx  # : ZD のベースアドレス
       movl   16(%esp), %edx  # : ZT のベースアドレス
       
       pushl %ebx
       
#------------------------------------
# fftj16 のループ
#------------------------------------

       movl %eax,%ebx
       addl $128,%ebx

#------------------------------------
.L0:
       movaps  128(%eax), %xmm0
       movaps  640(%eax), %xmm2
       movaps 1152(%eax), %xmm1
       movaps 1664(%eax), %xmm3

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

       movaps  384(%eax), %xmm5
       movaps  896(%eax), %xmm6
       movaps 1408(%eax), %xmm3
       movaps 1920(%eax), %xmm4

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

       movaps .C1,%xmm1
       movaps .C3,%xmm3       

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

       movaps     (%eax), %xmm0
       movaps 1024(%eax), %xmm3
       movaps  512(%eax), %xmm2
       movaps 1536(%eax), %xmm1
       
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

       movaps  256(%eax), %xmm5
       movaps  768(%eax), %xmm6
       movaps 1280(%eax), %xmm4
       movaps 1792(%eax), %xmm1

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

       movaps .C2,%xmm1
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

       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm1,%xmm4
       
       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm1,%xmm7

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
       jne .L0
       
       subl $128,%eax
       subl $2048,%ecx

#------------------------------------
# fftj8 のループ
#------------------------------------
      movl %ecx,%ebx
      addl $256,%ebx
       
      movaps     (%ecx), %xmm6 # Z(0)       
      movaps 1024(%ecx), %xmm7 # Z(4)
      movaps  256(%ecx), %xmm0 # Z(1)
      movaps 1280(%ecx), %xmm1 # Z(5)              
      movaps  768(%ecx), %xmm2 # Z(3)       
      movaps 1792(%ecx), %xmm3 # Z(7)
      
      movaps %xmm0,%xmm4
      subpd %xmm1,%xmm0
      addpd %xmm4,%xmm1
       
      movaps %xmm2,%xmm4
      subpd %xmm3,%xmm2
      addpd %xmm4,%xmm3
       
      movaps %xmm1,%xmm4
      subpd %xmm3,%xmm1
      addpd %xmm4,%xmm3
       
      movaps %xmm0,%xmm4
      subpd %xmm2,%xmm0
      addpd %xmm4,%xmm2
       
      movaps %xmm6,%xmm4
      subpd %xmm7,%xmm6
      addpd %xmm4,%xmm7
       
      movaps  512(%ecx), %xmm4 # Z(2)
      movaps 1536(%ecx), %xmm5 # Z(6)              
      movaps %xmm6, (%ecx)

      movaps %xmm4,%xmm6
      subpd %xmm5,%xmm4
      addpd %xmm6,%xmm5
       
      movaps .CI,%xmm6

      shufpd $0x1,%xmm2,%xmm2
      mulpd .C2D,%xmm2
      shufpd $0x1,%xmm4,%xmm4
      xorpd %xmm6,%xmm4
      shufpd $0x1,%xmm1,%xmm1
      xorpd %xmm6,%xmm1
      mulpd .C2,%xmm0              

      movaps %xmm2,%xmm6
      subpd %xmm4,%xmm2
      addpd %xmm6,%xmm4
       
      movaps %xmm7,%xmm6
      subpd %xmm5,%xmm7
      addpd %xmm6,%xmm5
       
      movaps %xmm5,%xmm6
      subpd %xmm3,%xmm5
      addpd %xmm6,%xmm3

      movaps (%ecx),%xmm6
# 何故か以下の順の方が Pen4では速い
      movaps %xmm3,    (%ecx)
      movaps %xmm5,1024(%ecx)
#      

      movaps %xmm7,%xmm5
      subpd %xmm1,%xmm7
      addpd %xmm5,%xmm1

      movaps %xmm7, 1536(%ecx)
      movaps %xmm1,  512(%ecx)
       
      movaps %xmm6,%xmm3
      subpd %xmm0,%xmm6
      addpd %xmm3,%xmm0
       
      movaps %xmm6,%xmm3
      subpd %xmm2,%xmm6
      addpd %xmm3,%xmm2
       
      movaps %xmm6, 1280(%ecx)       
      movaps %xmm2,  768(%ecx)

      movaps %xmm0,%xmm2
      subpd %xmm4,%xmm0
      addpd %xmm2,%xmm4
       
      movaps %xmm0,1792(%ecx)
      movaps %xmm4, 256(%ecx)

      addl $16,%ecx
      addl $256,%edx       
       
.align 16
.L1:
      movaps     (%ecx), %xmm6 # Z(0)       
      movaps 1024(%ecx), %xmm7 # Z(4)
      movaps  256(%ecx), %xmm0 # Z(1)
      movaps 1280(%ecx), %xmm1 # Z(5)              
      movaps  768(%ecx), %xmm2 # Z(3)       
      movaps 1792(%ecx), %xmm3 # Z(7)
      
###
       movaps %xmm7,%xmm4
       shufpd $1,%xmm7,%xmm7
       mulpd 128(%edx),%xmm4
       mulpd 144(%edx),%xmm7
       addpd %xmm4,%xmm7
###       
###
       movaps %xmm0,%xmm4
       shufpd $1,%xmm0,%xmm0
       mulpd  32(%edx),%xmm4
       mulpd  48(%edx),%xmm0
       addpd %xmm4,%xmm0
###       
###
       movaps %xmm1,%xmm4
       shufpd $1,%xmm1,%xmm1
       mulpd 160(%edx),%xmm4
       mulpd 176(%edx),%xmm1
       addpd %xmm4,%xmm1
###       
###
       movaps %xmm2,%xmm4
       shufpd $1,%xmm2,%xmm2
       mulpd  96(%edx),%xmm4
       mulpd 112(%edx),%xmm2
       addpd %xmm4,%xmm2
###       
###
       movaps %xmm3,%xmm4
       shufpd $1,%xmm3,%xmm3
       mulpd 224(%edx),%xmm4
       mulpd 240(%edx),%xmm3
       addpd %xmm4,%xmm3
###       
       
      movaps %xmm0,%xmm4
      subpd %xmm1,%xmm0
      addpd %xmm4,%xmm1
       
      movaps %xmm2,%xmm4
      subpd %xmm3,%xmm2
      addpd %xmm4,%xmm3
       
      movaps %xmm1,%xmm4
      subpd %xmm3,%xmm1
      addpd %xmm4,%xmm3
       
      movaps %xmm0,%xmm4
      subpd %xmm2,%xmm0
      addpd %xmm4,%xmm2
       
      movaps %xmm6,%xmm4
      subpd %xmm7,%xmm6
      addpd %xmm4,%xmm7
       
      movaps  512(%ecx), %xmm4 # Z(2)
      movaps 1536(%ecx), %xmm5 # Z(6)              
      movaps %xmm6, (%ecx)
      
###
       movaps %xmm4,%xmm6
       shufpd $1,%xmm4,%xmm4
       mulpd  64(%edx),%xmm6
       mulpd  80(%edx),%xmm4
       addpd %xmm6,%xmm4
###       
###
       movaps %xmm5,%xmm6
       shufpd $1,%xmm5,%xmm5
       mulpd 192(%edx),%xmm6
       mulpd 208(%edx),%xmm5
       addpd %xmm6,%xmm5
###       

      movaps %xmm4,%xmm6
      subpd %xmm5,%xmm4
      addpd %xmm6,%xmm5
       
      movaps .CI,%xmm6

      shufpd $0x1,%xmm2,%xmm2
      mulpd .C2D,%xmm2
      shufpd $0x1,%xmm4,%xmm4
      xorpd %xmm6,%xmm4
      shufpd $0x1,%xmm1,%xmm1
      xorpd %xmm6,%xmm1
      mulpd .C2,%xmm0              

      movaps %xmm2,%xmm6
      subpd %xmm4,%xmm2
      addpd %xmm6,%xmm4
       
      movaps %xmm7,%xmm6
      subpd %xmm5,%xmm7
      addpd %xmm6,%xmm5
       
      movaps %xmm5,%xmm6
      subpd %xmm3,%xmm5
      addpd %xmm6,%xmm3

      movaps (%ecx),%xmm6
# 何故か以下の順の方が Pen4では速い      
      movaps %xmm3,    (%ecx)
      movaps %xmm5,1024(%ecx)
#      

      movaps %xmm7,%xmm5
      subpd %xmm1,%xmm7
      addpd %xmm5,%xmm1

      movaps %xmm7, 1536(%ecx)
      movaps %xmm1,  512(%ecx)
       
      movaps %xmm6,%xmm3
      subpd %xmm0,%xmm6
      addpd %xmm3,%xmm0
       
      movaps %xmm6,%xmm3
      subpd %xmm2,%xmm6
      addpd %xmm3,%xmm2
       
      movaps %xmm6, 1280(%ecx)       
      movaps %xmm2,  768(%ecx)

      movaps %xmm0,%xmm2
      subpd %xmm4,%xmm0
      addpd %xmm2,%xmm4
       
      movaps %xmm0,1792(%ecx)
      movaps %xmm4, 256(%ecx)

      addl $16,%ecx
      addl $256,%edx       
      cmpl %ecx,%ebx
      jne .L1
       
#------------------------------------
      popl %ebx
	
      ret

.align 16
.CI:
      .long 0x0, 0x0, 0x0, 0x80000000
.align 16
.C1:
      .long 0xcf328d46,0x3fed906b,0xcf328d46,0x3fed906b
.align 16
.C2:
      .long 0x667f3bcd, 0x3fe6a09e,0x667f3bcd, 0x3fe6a09e
.align 16
.C2D:
      .long 0x667f3bcd, 0x3fe6a09e, 0x667f3bcd, 0xbfe6a09e
.align 16
.C3:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2
