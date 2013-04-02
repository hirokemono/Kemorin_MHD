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
# rfft 8 out-of-place forward
.text
.globl fjrof3_
fjrof3_:

#-------------------------------------------------------------
# fft 4 in-place backward
#.text
#.globl fjcib2_
#fjcib2_:
      movaps .CID,%xmm7

      movaps   (%rdi),%xmm0
      movaps 16(%rdi),%xmm1
      movaps 32(%rdi),%xmm2
      movaps 48(%rdi),%xmm3       

      movaps %xmm0,%xmm4
      subpd  %xmm2,%xmm0
      addpd  %xmm4,%xmm2

      movaps %xmm1,%xmm4
      subpd  %xmm3,%xmm1
      addpd  %xmm4,%xmm3

      xorpd  %xmm7,%xmm1
      shufpd $0x1,%xmm1,%xmm1
       
      movaps %xmm2,%xmm4
      subpd  %xmm3,%xmm2
      addpd  %xmm4,%xmm3

      movaps %xmm0,%xmm4
      subpd  %xmm1,%xmm0      
      addpd  %xmm4,%xmm1
       
#-------------------------------------------------------------       
#  rfft のための後処理

      movaps %xmm3,%xmm4            
      xorpd %xmm7,%xmm3      
      shufpd $1,%xmm4,%xmm4      
      addpd %xmm4,%xmm3
      movapd %xmm3,(%rsi)
      
      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm4      
      subpd %xmm1,%xmm0
      
      movaps %xmm0,%xmm5
      shufpd $1,%xmm0,%xmm0
      mulpd .A2,%xmm5
      mulpd .B2,%xmm0
      addpd %xmm5,%xmm0
      
      addpd %xmm0,%xmm1
      subpd %xmm0,%xmm4
      xorpd %xmm7,%xmm4
      movaps %xmm1,16(%rsi)      
      movaps %xmm4,48(%rsi)

      xorpd %xmm7,%xmm2
      movaps %xmm2,32(%rsi)

#--------------------------------------------------       
       ret

#--------------------------------------------------
.section .rodata
.align 16
.CID:
      .long 0x0, 0x0, 0x0, 0x80000000
.align 16
.A2:
      .long 0x33018866,0x3fc2bec3,0x33018866,0x3fc2bec3
.align 16
.B2:
      .long 0x667f3bcd,0x3fd6a09e,0x667f3bcd,0xbfd6a09e
