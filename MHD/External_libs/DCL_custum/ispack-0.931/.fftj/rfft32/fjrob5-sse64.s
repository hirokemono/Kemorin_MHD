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
# rfft 32 out-of-place backward
.text
.globl fjrob5_
fjrob5_:
       
#--------------------------------       
#  rfft のための前処理

      addq $512,%rcx
      lea 256(%rdi),%r8
      lea 256(%rsi),%r9
      
      movaps .CID,%xmm7
      
      movaps (%rdi),%xmm0
      movaps %xmm0,%xmm1            
      xorpd %xmm7,%xmm0
      shufpd $1,%xmm1,%xmm1      
      addpd %xmm1,%xmm0
      movapd %xmm0,(%rsi)
      
      movaps -16(%r8),%xmm1
      movaps 16(%rdi),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd 32(%rcx),%xmm2
      mulpd 48(%rcx),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,16(%rsi)
      movaps %xmm1,-16(%r9)
      
      addq $32,%rdi
      subq $32,%r8
      addq $32,%rsi
      subq $32,%r9
      addq $64,%rcx      

.align 16
.L1R:
      movaps (%r8),%xmm1
      movaps (%rdi),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd (%rcx),%xmm2
      mulpd 16(%rcx),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,(%rsi)      
      movaps %xmm1,(%r9)

      movaps -16(%r8),%xmm1
      movaps 16(%rdi),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd 32(%rcx),%xmm2
      mulpd 48(%rcx),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,16(%rsi)
      movaps %xmm1,-16(%r9)
      
      addq $32,%rdi
      subq $32,%r8
      addq $32,%rsi
      subq $32,%r9
      addq $64,%rcx      

      cmpq %rdi,%r8
      jne .L1R

      movaps (%rdi),%xmm0
      addpd %xmm0,%xmm0
      xorpd %xmm7,%xmm0
      movaps %xmm0,(%rsi)
      
      subq $128,%rsi
      movq %rsi,%rdi      

#-------------------------------------------------------------       
# fft 16 in-place backward
#.text
#.globl fjcib4_
#fjcib4_:
       movaps 16(%rdi), %xmm0
       movaps 80(%rdi), %xmm2       
       movaps 144(%rdi), %xmm1
       movaps 208(%rdi), %xmm3

       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0
       addpd %xmm7,%xmm1

       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2
       addpd %xmm7,%xmm3

       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1
       addpd %xmm7,%xmm3

       movaps %xmm3,16(%rdi)
       
#-------------

       movaps 48(%rdi), %xmm5
       movaps 112(%rdi), %xmm6       
       movaps 176(%rdi), %xmm3
       movaps 240(%rdi), %xmm4

       movaps %xmm5,%xmm7
       subpd %xmm3,%xmm5
       addpd %xmm7,%xmm3

       movaps %xmm6,%xmm7
       subpd %xmm4,%xmm6
       addpd %xmm7,%xmm4

       movaps %xmm3,%xmm7
       subpd %xmm4,%xmm3
       addpd %xmm7,%xmm4

#-------------

       movaps .C1,%xmm8
       movaps .C3,%xmm9       

       movaps %xmm0,%xmm7
       subpd %xmm6,%xmm0
       addpd %xmm7,%xmm6

       movaps %xmm5,%xmm7
       subpd %xmm2,%xmm5
       addpd %xmm7,%xmm2

       movaps %xmm8,%xmm7
       movaps %xmm9,%xmm10
       
       mulpd %xmm5,%xmm7
       mulpd %xmm0,%xmm10       
       subpd %xmm7,%xmm10
       
       mulpd %xmm8,%xmm0
       mulpd %xmm9,%xmm5
       addpd %xmm5,%xmm0
       
       movaps %xmm8,%xmm7
       
       mulpd %xmm6,%xmm7
       mulpd %xmm9,%xmm6
       
       mulpd %xmm2,%xmm9
       subpd %xmm9,%xmm7

       mulpd %xmm8,%xmm2
       addpd %xmm2,%xmm6

#-----------------------------------

       movaps (%rdi), %xmm11
       movaps 128(%rdi), %xmm9
       movaps 64(%rdi), %xmm2
       movaps 192(%rdi), %xmm8
       
       movaps %xmm11,%xmm14
       subpd %xmm9,%xmm11       
       addpd %xmm14,%xmm9

       movaps %xmm2,%xmm14
       subpd %xmm8,%xmm2
       addpd %xmm14,%xmm8
       
       movaps %xmm9,%xmm14
       subpd %xmm8,%xmm9
       addpd %xmm14,%xmm8
       
       movaps %xmm8,(%rdi)
       
#-----------------------------------

       movaps 32(%rdi), %xmm5
       movaps 96(%rdi), %xmm13       
       movaps 160(%rdi), %xmm12
       movaps 224(%rdi), %xmm8

       movaps %xmm5,%xmm14
       subpd %xmm12,%xmm5
       addpd %xmm14,%xmm12
       
       movaps %xmm13,%xmm14
       subpd %xmm8,%xmm13
       addpd %xmm14,%xmm8
       
       movaps %xmm12,%xmm14
       subpd %xmm8,%xmm12
       addpd %xmm14,%xmm8
       
       movaps %xmm5,%xmm14
       subpd %xmm13,%xmm5
       addpd %xmm14,%xmm13
       
#-----------------------------------

       movaps .C2,%xmm15
       mulpd %xmm15,%xmm5
       mulpd %xmm15,%xmm13       
       movaps %xmm11,%xmm14
       subpd %xmm5,%xmm11
       addpd %xmm14,%xmm5

       movaps %xmm2,%xmm14
       subpd %xmm13,%xmm2
       addpd %xmm14,%xmm13
       
       movaps %xmm11,64(%rdi)
       movaps %xmm2,192(%rdi)
       
#-----------------------------------

       mulpd %xmm15,%xmm1
       mulpd %xmm15,%xmm3
       
       movaps .CI,%xmm15

       movaps %xmm1,%xmm11
       subpd %xmm3,%xmm1
       addpd %xmm11,%xmm3

       shufpd $0x1,%xmm12,%xmm12
       xorpd %xmm15,%xmm12
       
       shufpd $0x1,%xmm3,%xmm3
       xorpd %xmm15,%xmm3

       movaps %xmm9,%xmm11
       subpd %xmm1,%xmm9
       addpd %xmm11,%xmm1

       movaps %xmm3,%xmm11
       subpd %xmm12,%xmm3
       addpd %xmm11,%xmm12
       
       movaps %xmm1,%xmm11
       subpd %xmm12,%xmm1
       addpd %xmm11,%xmm12

       movaps %xmm9,%xmm11
       subpd %xmm3,%xmm9
       addpd %xmm11,%xmm3
       
       movaps %xmm12,32(%rdi)              
       movaps %xmm3,96(%rdi)
       movaps %xmm9,160(%rdi)
       movaps %xmm1,224(%rdi)       
       
#-----------------------------------       
       
       movaps 16(%rdi), %xmm1

       movaps %xmm5,%xmm11
       subpd %xmm0,%xmm5
       addpd %xmm11,%xmm0
       
       movaps %xmm6,%xmm11
       subpd %xmm13,%xmm6
       addpd %xmm11,%xmm13
       shufpd $0x1,%xmm6,%xmm6
       xorpd %xmm15,%xmm6
       shufpd $0x1,%xmm13,%xmm13
       xorpd %xmm15,%xmm13

       movaps %xmm0,%xmm11
       subpd %xmm13,%xmm0
       addpd %xmm11,%xmm13
       
       movaps %xmm5,%xmm11
       subpd %xmm6,%xmm5
       addpd %xmm11,%xmm6

       movaps %xmm13,16(%rdi)              
       movaps %xmm6,112(%rdi)
       movaps %xmm5,144(%rdi)
       movaps %xmm0,240(%rdi)       
       
#-----------------------------------

       movaps (%rdi), %xmm6
       movaps 64(%rdi), %xmm0
       movaps 192(%rdi), %xmm13

       movaps %xmm6,%xmm11
       subpd %xmm8,%xmm6
       addpd %xmm11,%xmm8

       movaps %xmm1,%xmm11
       subpd %xmm4,%xmm1
       addpd %xmm11,%xmm4

       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm15,%xmm1

       movaps %xmm8,%xmm11
       subpd %xmm4,%xmm8
       addpd %xmm11,%xmm4
       
       movaps %xmm6,%xmm11
       subpd %xmm1,%xmm6
       addpd %xmm11,%xmm1

       movaps %xmm4,(%rdi)
       movaps %xmm1,64(%rdi)
       movaps %xmm8,128(%rdi)       
       movaps %xmm6,192(%rdi)
       
#-----------------------------------       
       
       movaps %xmm0,%xmm11
       subpd %xmm10,%xmm0
       addpd %xmm11,%xmm10

       movaps %xmm7,%xmm11
       subpd %xmm13,%xmm7
       addpd %xmm11,%xmm13

       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm15,%xmm7
       shufpd $0x1,%xmm13,%xmm13
       xorpd %xmm15,%xmm13
       
       movaps %xmm10,%xmm11
       subpd %xmm7,%xmm10
       addpd %xmm11,%xmm7
       
       movaps %xmm0,%xmm11
       subpd %xmm13,%xmm0
       addpd %xmm11,%xmm13

       movaps %xmm7,48(%rdi)
       movaps %xmm13,80(%rdi)
       movaps %xmm0,176(%rdi)
       movaps %xmm10,208(%rdi)              

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
.C1:
      .long 0xcf328d46,0x3fed906b,0xcf328d46,0x3fed906b
.align 16
.C2:
      .long 0x667f3bcd, 0x3fe6a09e,0x667f3bcd, 0x3fe6a09e
.align 16
.C3:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2
