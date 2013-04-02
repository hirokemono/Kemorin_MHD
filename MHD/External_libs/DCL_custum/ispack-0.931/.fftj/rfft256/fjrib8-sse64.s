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
# rfft 256 in-place backward
.text
.globl fjrib8_
fjrib8_:
#  rfft のための前処理

      movq %rcx,%r8
      addq $4096,%r8
      movq $2048,%r10  # %ebx に (N/2)*16 が入る
      addq %rdi,%r10
      movq %rdi,%r9
      movaps .CID,%xmm7
      
      movaps (%r9),%xmm0
      movaps %xmm0,%xmm1            
      xorpd %xmm7,%xmm0
      shufpd $1,%xmm1,%xmm1      
      addpd %xmm1,%xmm0
      movapd %xmm0,(%r9)
      
      movaps -16(%r10),%xmm1
      movaps 16(%r9),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd 32(%r8),%xmm2
      mulpd 48(%r8),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,16(%r9)      
      movaps %xmm1,-16(%r10)
      
      addq $32,%r9
      subq $32,%r10
      addq $64,%r8      

.align 16
.L1R:
      movaps (%r10),%xmm1
      movaps (%r9),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd (%r8),%xmm2
      mulpd 16(%r8),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,(%r9)      
      movaps %xmm1,(%r10)

      movaps -16(%r10),%xmm1
      movaps 16(%r9),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd 32(%r8),%xmm2
      mulpd 48(%r8),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,16(%r9)
      movaps %xmm1,-16(%r10)
      
      addq $32,%r9
      subq $32,%r10
      addq $64,%r8      

      cmpq %r9,%r10
      jne .L1R

      movaps (%r9),%xmm0
      addpd %xmm0,%xmm0
      xorpd %xmm7,%xmm0
      movaps %xmm0,(%r9)

#-----------------------------------------------------
# fft 128 in-place backward (16x8 type)
#.text
#.globl fjcib7_
#fjcib7_:
#       
#------------------------------------
# fftj16 のループ
#------------------------------------

       movq %rdi,%r8
       addq $128,%r8

#------------------------------------
.L0:
       movaps  128(%rdi), %xmm0
       movaps  640(%rdi), %xmm2       
       movaps 1152(%rdi), %xmm1
       movaps 1664(%rdi), %xmm3

       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0
       addpd %xmm7,%xmm1

       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2
       addpd %xmm7,%xmm3

       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1
       addpd %xmm7,%xmm3

       movaps %xmm3,16(%rsi)
       
#-------------

       movaps  384(%rdi), %xmm5
       movaps  896(%rdi), %xmm6       
       movaps 1408(%rdi), %xmm3
       movaps 1920(%rdi), %xmm4

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

       movaps     (%rdi), %xmm11
       movaps 1024(%rdi), %xmm9
       movaps  512(%rdi), %xmm2
       movaps 1536(%rdi), %xmm8
       
       movaps %xmm11,%xmm14
       subpd %xmm9,%xmm11       
       addpd %xmm14,%xmm9

       movaps %xmm2,%xmm14
       subpd %xmm8,%xmm2
       addpd %xmm14,%xmm8
       
       movaps %xmm9,%xmm14
       subpd %xmm8,%xmm9
       addpd %xmm14,%xmm8

       movaps %xmm8,(%rsi)
       
#-----------------------------------

       movaps  256(%rdi), %xmm5
       movaps  768(%rdi), %xmm13       
       movaps 1280(%rdi), %xmm12
       movaps 1792(%rdi), %xmm8

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
       
       movaps %xmm11,64(%rsi)
       movaps %xmm2,192(%rsi)
       
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

       movaps %xmm12,32(%rsi)
       movaps %xmm3,96(%rsi)
       movaps %xmm9,160(%rsi)
       movaps %xmm1,224(%rsi)       
       
#-----------------------------------       
       
       movaps 16(%rsi), %xmm1

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

       movaps %xmm13,16(%rsi)              
       movaps %xmm6,112(%rsi)
       movaps %xmm5,144(%rsi)
       movaps %xmm0,240(%rsi)       
       
#-----------------------------------

       movaps (%rsi), %xmm6
       movaps 64(%rsi), %xmm0
       movaps 192(%rsi), %xmm13

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

       movaps %xmm4,(%rsi)
       movaps %xmm1,64(%rsi)
       movaps %xmm8,128(%rsi)       
       movaps %xmm6,192(%rsi)
       
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

       movaps %xmm7,48(%rsi)
       movaps %xmm13,80(%rsi)
       movaps %xmm0,176(%rsi)
       movaps %xmm10,208(%rsi)              

       addq $16,%rdi
       addq $256,%rsi
       cmpq %rdi,%r8
       jne .L0
       
       subq $128,%rdi
       subq $2048,%rsi

#------------------------------------
# fftj8 のループ
#------------------------------------
      movq %rdi,%r8
      addq $256,%r8
       
      movaps     (%rsi), %xmm6 # Z(0)       
      movaps 1024(%rsi), %xmm7 # Z(4)
      movaps  256(%rsi), %xmm0 # Z(1)
      movaps 1280(%rsi), %xmm1 # Z(5)              
      movaps  768(%rsi), %xmm2 # Z(3)       
      movaps 1792(%rsi), %xmm3 # Z(7)
      
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
       
      movaps  512(%rsi), %xmm4 # Z(2)
      movaps 1536(%rsi), %xmm5 # Z(6)              
      movaps %xmm6, (%rdi)

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

      movaps (%rdi),%xmm6
# 何故か以下の順の方が Pen4では速い
      movaps %xmm3,    (%rdi)
      movaps %xmm5,1024(%rdi)
#      

      movaps %xmm7,%xmm5
      subpd %xmm1,%xmm7
      addpd %xmm5,%xmm1

      movaps %xmm7, 1536(%rdi)
      movaps %xmm1,  512(%rdi)
       
      movaps %xmm6,%xmm3
      subpd %xmm0,%xmm6
      addpd %xmm3,%xmm0
       
      movaps %xmm6,%xmm3
      subpd %xmm2,%xmm6
      addpd %xmm3,%xmm2
       
      movaps %xmm6, 1280(%rdi)       
      movaps %xmm2,  768(%rdi)

      movaps %xmm0,%xmm2
      subpd %xmm4,%xmm0
      addpd %xmm2,%xmm4
       
      movaps %xmm0,1792(%rdi)
      movaps %xmm4, 256(%rdi)

      addq $16,%rdi
      addq $16,%rsi
      addq $256,%rcx
       
.align 16
.L1:
      movaps     (%rsi), %xmm6 # Z(0)       
      movaps 1024(%rsi), %xmm7 # Z(4)
      movaps  256(%rsi), %xmm0 # Z(1)
      movaps 1280(%rsi), %xmm1 # Z(5)              
      movaps  768(%rsi), %xmm2 # Z(3)       
      movaps 1792(%rsi), %xmm3 # Z(7)
      
###
       movaps %xmm7,%xmm4
       shufpd $1,%xmm7,%xmm7
       mulpd 128(%rcx),%xmm4
       mulpd 144(%rcx),%xmm7
       addpd %xmm4,%xmm7
###       
###
       movaps %xmm0,%xmm4
       shufpd $1,%xmm0,%xmm0
       mulpd  32(%rcx),%xmm4
       mulpd  48(%rcx),%xmm0
       addpd %xmm4,%xmm0
###       
###
       movaps %xmm1,%xmm4
       shufpd $1,%xmm1,%xmm1
       mulpd 160(%rcx),%xmm4
       mulpd 176(%rcx),%xmm1
       addpd %xmm4,%xmm1
###       
###
       movaps %xmm2,%xmm4
       shufpd $1,%xmm2,%xmm2
       mulpd  96(%rcx),%xmm4
       mulpd 112(%rcx),%xmm2
       addpd %xmm4,%xmm2
###       
###
       movaps %xmm3,%xmm4
       shufpd $1,%xmm3,%xmm3
       mulpd 224(%rcx),%xmm4
       mulpd 240(%rcx),%xmm3
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
       
      movaps  512(%rsi), %xmm4 # Z(2)
      movaps 1536(%rsi), %xmm5 # Z(6)              
      movaps %xmm6, (%rdi)
      
###
       movaps %xmm4,%xmm6
       shufpd $1,%xmm4,%xmm4
       mulpd  64(%rcx),%xmm6
       mulpd  80(%rcx),%xmm4
       addpd %xmm6,%xmm4
###       
###
       movaps %xmm5,%xmm6
       shufpd $1,%xmm5,%xmm5
       mulpd 192(%rcx),%xmm6
       mulpd 208(%rcx),%xmm5
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

      movaps (%rdi),%xmm6
# 何故か以下の順の方が Pen4では速い      
      movaps %xmm3,    (%rdi)
      movaps %xmm5,1024(%rdi)
#      

      movaps %xmm7,%xmm5
      subpd %xmm1,%xmm7
      addpd %xmm5,%xmm1

      movaps %xmm7, 1536(%rdi)
      movaps %xmm1,  512(%rdi)
       
      movaps %xmm6,%xmm3
      subpd %xmm0,%xmm6
      addpd %xmm3,%xmm0
       
      movaps %xmm6,%xmm3
      subpd %xmm2,%xmm6
      addpd %xmm3,%xmm2
       
      movaps %xmm6, 1280(%rdi)       
      movaps %xmm2,  768(%rdi)

      movaps %xmm0,%xmm2
      subpd %xmm4,%xmm0
      addpd %xmm2,%xmm4
       
      movaps %xmm0,1792(%rdi)
      movaps %xmm4, 256(%rdi)

      addq $16,%rdi
      addq $16,%rsi
      addq $256,%rcx
      cmpq %rdi,%r8
      jne .L1

       ret
#----------------------------
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
.C2D:
      .long 0x667f3bcd, 0xbfe6a09e,0x667f3bcd, 0x3fe6a09e
.align 16
.C3:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2
