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
# rfft 16 out-of-place backward
.text
.globl fjrob4_
fjrob4_:
       
#--------------------------------       
#  rfft のための前処理

      movaps .CID,%xmm7
      
      movaps (%rdi),%xmm0
      movaps %xmm0,%xmm1            
      xorpd %xmm7,%xmm0
      shufpd $1,%xmm1,%xmm1      
      addpd %xmm1,%xmm0
      movapd %xmm0,(%rsi)
      
      movaps 112(%rdi),%xmm1
      movaps  16(%rdi),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd .S1,%xmm2
      mulpd .C1D,%xmm0
      subpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0, 16(%rsi)      
      movaps %xmm1,112(%rsi)
      
      movaps 96(%rdi),%xmm1
      movaps 32(%rdi),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      xorpd %xmm7,%xmm0            
      shufpd $1,%xmm0,%xmm0
      subpd %xmm2,%xmm0
      mulpd .C2,%xmm0      
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,32(%rsi)      
      movaps %xmm1,96(%rsi)

      movaps 80(%rdi),%xmm1
      movaps 48(%rdi),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd .C1,%xmm2
      mulpd .S1D,%xmm0
      subpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,48(%rsi)
      movaps %xmm1,80(%rsi)
      
      movaps 64(%rdi),%xmm0
      addpd %xmm0,%xmm0
      xorpd %xmm7,%xmm0
      movaps %xmm0,64(%rsi)
      
#-------------------------------------------------------------       
# fft 8 in-place backward
#.text
#.globl fjcib3_
#fjcib3_:
      movaps    (%rsi), %xmm6 # Z(0)       
      movaps  64(%rsi), %xmm7 # Z(4)
      movaps  16(%rsi), %xmm0 # Z(1)
      movaps  80(%rsi), %xmm1 # Z(5)              
      movaps  48(%rsi), %xmm2 # Z(3)       
      movaps 112(%rsi), %xmm3 # Z(7)
       
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
       
      movaps 32(%rsi), %xmm4 # Z(2)
      movaps 96(%rsi), %xmm5 # Z(6)              

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

      movaps %xmm5,64(%rsi)
      movaps %xmm3,  (%rsi)

      movaps %xmm7,%xmm5
      subpd %xmm1,%xmm7
      addpd %xmm5,%xmm1

      movaps %xmm7, 96(%rsi)
      movaps %xmm1, 32(%rsi)
       
      movaps %xmm6,%xmm3
      subpd %xmm0,%xmm6
      addpd %xmm3,%xmm0
       
      movaps %xmm6,%xmm3
      subpd %xmm2,%xmm6
      addpd %xmm3,%xmm2
       
      movaps %xmm6, 80(%rsi)       
      movaps %xmm2, 48(%rsi)

      movaps %xmm0,%xmm2
      subpd %xmm4,%xmm0
      addpd %xmm2,%xmm4
       
      movaps %xmm0,112(%rsi)
      movaps %xmm4, 16(%rsi)

#-------------------------------------------------------------
.LE:

       ret

#--------------------------------------------------
.section .rodata
.align 16
.CID:
      .long 0x0, 0x0, 0x0, 0x80000000
.align 16
.CI:
      .long 0x0, 0x80000000, 0x0, 0x0
.align 16
.C2:
      .long 0x667f3bcd, 0x3fe6a09e,0x667f3bcd, 0x3fe6a09e
.align 16
.C2D:
      .long 0x667f3bcd, 0xbfe6a09e,0x667f3bcd, 0x3fe6a09e
.align 16
.C1:
      .long 0xcf328d46,0x3fed906b,0xcf328d46,0x3fed906b
.align 16
.S1:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2
.align 16
.C1D:
      .long 0xcf328d46,0xbfed906b,0xcf328d46,0x3fed906b
.align 16
.S1D:
      .long 0xa6aea963,0xbfd87de2,0xa6aea963,0x3fd87de2
