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
# fft 4 out-of-place backward
.text
.globl fjcob2_
fjcob2_:
      movaps .CI,%xmm7

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
       
      shufpd $0x1,%xmm1,%xmm1
      xorpd  %xmm7,%xmm1
       
      movaps %xmm2,%xmm4
      addpd  %xmm3,%xmm2
      subpd  %xmm3,%xmm4

      movaps %xmm0,%xmm5
      addpd  %xmm1,%xmm0
      subpd  %xmm1,%xmm5
       
      movaps %xmm2,  (%rsi)
      movaps %xmm0,16(%rsi)       
      movaps %xmm4,32(%rsi)
      movaps %xmm5,48(%rsi)
      ret

.align 16
.CI:
      .long  0x0, 0x80000000, 0x0, 0x0
