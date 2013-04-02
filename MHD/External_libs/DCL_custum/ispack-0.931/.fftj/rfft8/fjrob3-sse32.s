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
# rfft 8 out-of-place backward
.text
.globl fjrob3_
fjrob3_:
      movl 4(%esp), %eax
      movl 8(%esp), %ecx
       
#--------------------------------       
#  rfft のための前処理

      movaps .CID,%xmm7
      
      movaps (%eax),%xmm0
      movaps %xmm0,%xmm1            
      xorpd %xmm7,%xmm0
      shufpd $1,%xmm1,%xmm1      
      addpd %xmm1,%xmm0
      
      movaps 48(%eax),%xmm3
      movaps 16(%eax),%xmm1

      xorpd %xmm7,%xmm3
      movaps %xmm1,%xmm4
      subpd %xmm3,%xmm1
      addpd %xmm4,%xmm3
      
      movaps %xmm1,%xmm4
      xorpd %xmm7,%xmm1            
      shufpd $1,%xmm1,%xmm1
      subpd %xmm4,%xmm1
      mulpd .C2,%xmm1      
      
      movaps %xmm1,%xmm4
      addpd %xmm3,%xmm1
      subpd %xmm4,%xmm3
      xorpd %xmm7,%xmm3      

      movaps 32(%eax),%xmm2
      addpd %xmm2,%xmm2
      xorpd %xmm7,%xmm2
      
#-------------------------------------------------------------       
# fft 4 in-place backward
#.text
#.globl fjcib2_
#fjcib2_:
#      movl 4(%esp),%eax
#      movaps .CI,%xmm7

#      movaps   (%eax),%xmm0
#      movaps 16(%eax),%xmm1
#      movaps 32(%eax),%xmm2
#      movaps 48(%eax),%xmm3       

      movaps %xmm0,%xmm4
      subpd  %xmm2,%xmm0
      addpd  %xmm4,%xmm2

      movaps %xmm1,%xmm4
      subpd  %xmm3,%xmm1
      addpd  %xmm4,%xmm3
       
      xorpd  %xmm7,%xmm1
      shufpd $0x1,%xmm1,%xmm1
       
      movaps %xmm2,%xmm4
      addpd  %xmm3,%xmm2
      subpd  %xmm3,%xmm4

      movaps %xmm0,%xmm5
      addpd  %xmm1,%xmm0
      subpd  %xmm1,%xmm5
       
      movaps %xmm2,  (%ecx)
      movaps %xmm0,16(%ecx)       
      movaps %xmm4,32(%ecx)
      movaps %xmm5,48(%ecx)

#-------------------------------------------------------------
       ret

#--------------------------------------------------
.section .rodata
.align 16
.CID:
      .long 0x0, 0x0, 0x0, 0x80000000
.align 16
.C2:
      .long 0x667f3bcd, 0x3fe6a09e,0x667f3bcd, 0x3fe6a09e
