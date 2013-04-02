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
# fft 64 out-of-place backward (8x8 type)
.text
.globl fjcob6_
fjcob6_:
       
       lea 128(%rdi),%r8

#------------------------------------------------------

.align 16
.L0:
       movaps    (%rdi), %xmm6 # Z(0)       
       movaps 512(%rdi), %xmm7 # Z(4)
       movaps 128(%rdi), %xmm0 # Z(1)
       movaps 640(%rdi), %xmm1 # Z(5)              
       movaps 384(%rdi), %xmm2 # Z(3)       
       movaps 896(%rdi), %xmm3 # Z(7)
       
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
       
       movaps 256(%rdi), %xmm4 # Z(2)
       movaps 768(%rdi), %xmm5 # Z(6)              
#       movaps %xmm6, (%rdx)
#      xmm6 -> xmm8

       movaps %xmm4,%xmm8
       subpd %xmm5,%xmm4
       addpd %xmm8,%xmm5
       
       movaps .CI,%xmm8

       shufpd $0x1,%xmm2,%xmm2
       mulpd .C2D,%xmm2
       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm8,%xmm4
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm8,%xmm1
       mulpd .C2,%xmm0              

       movaps %xmm2,%xmm8
       subpd %xmm4,%xmm2
       addpd %xmm8,%xmm4
       
       movaps %xmm7,%xmm8
       subpd %xmm5,%xmm7
       addpd %xmm8,%xmm5
       
       movaps %xmm5,%xmm8
       subpd %xmm3,%xmm5
       addpd %xmm8,%xmm3

#       movaps (%rdx),%xmm6
       movaps %xmm5,64(%rdx)
       movaps %xmm3,  (%rdx)

       movaps %xmm7,%xmm5
       subpd %xmm1,%xmm7
       addpd %xmm5,%xmm1

       movaps %xmm7,96(%rdx)
       movaps %xmm1,32(%rdx)
       
       movaps %xmm6,%xmm3
       subpd %xmm0,%xmm6
       addpd %xmm3,%xmm0
       
       movaps %xmm6,%xmm3
       subpd %xmm2,%xmm6
       addpd %xmm3,%xmm2
       
       movaps %xmm6, 80(%rdx)
       movaps %xmm2, 48(%rdx)

       movaps %xmm0,%xmm2
       subpd %xmm4,%xmm0
       addpd %xmm2,%xmm4
       
       movaps %xmm0,112(%rdx)
       movaps %xmm4, 16(%rdx)

       addq $128,%rdx       
       addq $16,%rdi
       cmpq %rdi,%r8
       jne .L0
       
       subq $128,%rdi
       subq $1024,%rdx
       

#--------------------------------
       lea 128(%rsi),%r8

       movaps    (%rdx), %xmm6 # Z(0)       
       movaps 512(%rdx), %xmm7 # Z(4)
       movaps 128(%rdx), %xmm0 # Z(1)
       movaps 640(%rdx), %xmm1 # Z(5)              
       movaps 384(%rdx), %xmm2 # Z(3)       
       movaps 896(%rdx), %xmm3 # Z(7)
       
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
       
       movaps 256(%rdx), %xmm4 # Z(2)
       movaps 768(%rdx), %xmm5 # Z(6)              
#       movaps %xmm6, (%rsi)
#      xmm6 -> xmm8

       movaps %xmm4,%xmm8
       subpd %xmm5,%xmm4
       addpd %xmm8,%xmm5
       
       movaps .CI,%xmm8

       shufpd $0x1,%xmm2,%xmm2
       mulpd .C2D,%xmm2
       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm8,%xmm4
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm8,%xmm1
       mulpd .C2,%xmm0              

       movaps %xmm2,%xmm8
       subpd %xmm4,%xmm2
       addpd %xmm8,%xmm4
       
       movaps %xmm7,%xmm8
       subpd %xmm5,%xmm7
       addpd %xmm8,%xmm5
       
       movaps %xmm5,%xmm8
       subpd %xmm3,%xmm5
       addpd %xmm8,%xmm3

#       movaps (%rsi),%xmm6
       movaps %xmm5,512(%rsi)
       movaps %xmm3,   (%rsi)

       movaps %xmm7,%xmm5
       subpd %xmm1,%xmm7
       addpd %xmm5,%xmm1

       movaps %xmm7,768(%rsi)
       movaps %xmm1,256(%rsi)
       
       movaps %xmm6,%xmm3
       subpd %xmm0,%xmm6
       addpd %xmm3,%xmm0
       
       movaps %xmm6,%xmm3
       subpd %xmm2,%xmm6
       addpd %xmm3,%xmm2
       
       movaps %xmm6, 640(%rsi)
       movaps %xmm2, 384(%rsi)

       movaps %xmm0,%xmm2
       subpd %xmm4,%xmm0
       addpd %xmm2,%xmm4
       
       movaps %xmm0,896(%rsi)
       movaps %xmm4,128(%rsi)

       addq $256,%rcx
       addq $16,%rsi
       addq $16,%rdx

.align 16
.L1:
       movaps    (%rdx), %xmm6 # Z(0)       
       movaps 512(%rdx), %xmm7 # Z(4)
       movaps 128(%rdx), %xmm0 # Z(1)
       movaps 640(%rdx), %xmm1 # Z(5)              
       movaps 384(%rdx), %xmm2 # Z(3)       
       movaps 896(%rdx), %xmm3 # Z(7)
#
       movaps %xmm7,%xmm4
       shufpd $0x1,%xmm7,%xmm7
       mulpd 128(%rcx),%xmm4
       mulpd 144(%rcx),%xmm7
       addpd %xmm4,%xmm7
#       
#
       movaps %xmm0,%xmm4
       shufpd $0x1,%xmm0,%xmm0
       mulpd 32(%rcx),%xmm4
       mulpd 48(%rcx),%xmm0
       addpd %xmm4,%xmm0
#       
#
       movaps %xmm1,%xmm4
       shufpd $0x1,%xmm1,%xmm1
       mulpd 160(%rcx),%xmm4
       mulpd 176(%rcx),%xmm1
       addpd %xmm4,%xmm1
#       
#
       movaps %xmm2,%xmm4
       shufpd $0x1,%xmm2,%xmm2
       mulpd 96(%rcx),%xmm4
       mulpd 112(%rcx),%xmm2
       addpd %xmm4,%xmm2
#       
#
       movaps %xmm3,%xmm4
       shufpd $0x1,%xmm3,%xmm3
       mulpd 224(%rcx),%xmm4
       mulpd 240(%rcx),%xmm3
       addpd %xmm4,%xmm3
#       

       
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

#       movaps %xmm6, (%rsi)
#     xmm6 -> xmm8

       movaps 256(%rdx), %xmm4 # Z(2)
       movaps 768(%rdx), %xmm5 # Z(6)
#
       movaps %xmm4,%xmm8
       shufpd $0x1,%xmm4,%xmm4
       mulpd 64(%rcx),%xmm8
       mulpd 80(%rcx),%xmm4
       addpd %xmm8,%xmm4
#       
#
       movaps %xmm5,%xmm8
       shufpd $0x1,%xmm5,%xmm5
       mulpd 192(%rcx),%xmm8
       mulpd 208(%rcx),%xmm5
       addpd %xmm8,%xmm5
#       
       movaps %xmm4,%xmm8
       subpd %xmm5,%xmm4
       addpd %xmm8,%xmm5
       
       movaps .CI,%xmm8

       shufpd $0x1,%xmm2,%xmm2
       mulpd .C2D,%xmm2
       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm8,%xmm4
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm8,%xmm1
       mulpd .C2,%xmm0              

       movaps %xmm2,%xmm8
       subpd %xmm4,%xmm2
       addpd %xmm8,%xmm4
       
       movaps %xmm7,%xmm8
       subpd %xmm5,%xmm7
       addpd %xmm8,%xmm5
       
       movaps %xmm5,%xmm8
       subpd %xmm3,%xmm5
       addpd %xmm8,%xmm3

#       movaps (%rsi),%xmm6
       movaps %xmm5,512(%rsi)
       movaps %xmm3,   (%rsi)

       movaps %xmm7,%xmm5
       subpd %xmm1,%xmm7
       addpd %xmm5,%xmm1

       movaps %xmm7,768(%rsi)
       movaps %xmm1,256(%rsi)
       
       movaps %xmm6,%xmm3
       subpd %xmm0,%xmm6
       addpd %xmm3,%xmm0
       
       movaps %xmm6,%xmm3
       subpd %xmm2,%xmm6
       addpd %xmm3,%xmm2
       
       movaps %xmm6, 640(%rsi)
       movaps %xmm2, 384(%rsi)

       movaps %xmm0,%xmm2
       subpd %xmm4,%xmm0
       addpd %xmm2,%xmm4
       
       movaps %xmm0,896(%rsi)
       movaps %xmm4,128(%rsi)

       addq $256,%rcx
       addq $16,%rsi
       addq $16,%rdx
       cmpq %rsi,%r8
       jne .L1
       
       ret
.align 16
.CI:
      .long 0x0, 0x80000000, 0x0, 0x0
.align 16
.C2:
      .long 0x667f3bcd, 0x3fe6a09e,0x667f3bcd, 0x3fe6a09e
.align 16
.C2D:
      .long 0x667f3bcd, 0xbfe6a09e,0x667f3bcd, 0x3fe6a09e
