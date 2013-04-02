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
# fft 2 in-place backward
.text
.globl fjcib1_
fjcib1_:
       movl 4(%esp), %eax
       movaps   (%eax), %xmm0
       movaps 16(%eax), %xmm1
       movaps %xmm0,%xmm2
       addpd  %xmm1,%xmm0
       subpd  %xmm1,%xmm2
       movaps %xmm0,  (%eax)
       movaps %xmm2,16(%eax)
       ret
