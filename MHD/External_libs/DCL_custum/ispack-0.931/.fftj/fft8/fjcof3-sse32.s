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
# fft 4 out-of-place forward
.text
.globl fjcof3_
fjcof3_:
      movl	4(%esp), %eax
      movl      8(%esp), %ecx      

      movaps    (%eax), %xmm6 # Z(0)       
      movaps  64(%eax), %xmm7 # Z(4)
      movaps  16(%eax), %xmm0 # Z(1)
      movaps  80(%eax), %xmm1 # Z(5)              
      movaps  48(%eax), %xmm2 # Z(3)       
      movaps 112(%eax), %xmm3 # Z(7)
       
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
       
      movaps 32(%eax), %xmm4 # Z(2)
      movaps 96(%eax), %xmm5 # Z(6)              
      movaps %xmm6, (%ecx)              

      movaps %xmm4,%xmm6
      subpd %xmm5,%xmm4
      addpd %xmm6,%xmm5
       
      movaps .CI,%xmm6


      mulpd .C2D,%xmm2
      shufpd $0x1,%xmm2,%xmm2      
      xorpd %xmm6,%xmm4
      shufpd $0x1,%xmm4,%xmm4      
      xorpd %xmm6,%xmm1
      shufpd $0x1,%xmm1,%xmm1      
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
      movaps %xmm5,64(%ecx)
      movaps %xmm3,  (%ecx)

      movaps %xmm7,%xmm5
      subpd %xmm1,%xmm7
      addpd %xmm5,%xmm1

      movaps %xmm7, 96(%ecx)
      movaps %xmm1, 32(%ecx)
       
      movaps %xmm6,%xmm3
      subpd %xmm0,%xmm6
      addpd %xmm3,%xmm0
       
      movaps %xmm6,%xmm3
      subpd %xmm2,%xmm6
      addpd %xmm3,%xmm2
       
      movaps %xmm6, 80(%ecx)       
      movaps %xmm2, 48(%ecx)

      movaps %xmm0,%xmm2
      subpd %xmm4,%xmm0
      addpd %xmm2,%xmm4
       
      movaps %xmm0,112(%ecx)
      movaps %xmm4, 16(%ecx)
       
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
