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
# rfft 4 in-place forward
.text
.globl fjrif2_
fjrif2_:

#-------------------------------------------------------------
      movaps   (%rdi),%xmm0
      movaps 16(%rdi),%xmm1

      movaps %xmm0,%xmm2
      subpd  %xmm1,%xmm0
      addpd  %xmm2,%xmm1

#-------------------------------------------------------------       
#  rfft のための後処理

      movaps .CID,%xmm7
      
      movaps %xmm1,%xmm2            
      xorpd %xmm7,%xmm1      
      shufpd $1,%xmm2,%xmm2      
      addpd %xmm2,%xmm1
      movapd %xmm1,(%rdi)
      
      xorpd %xmm7,%xmm0
      movaps %xmm0,16(%rdi)

#--------------------------------------------------       
       ret

#--------------------------------------------------
.section .rodata
.align 16
.CID:
      .long 0x0, 0x0, 0x0, 0x80000000
