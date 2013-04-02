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
# rfft 2 out-of-place forward
.text
.globl fjrof1_
fjrof1_:
      movl    4(%esp), %eax
      movl    8(%esp), %ecx

      movsd  (%eax),%xmm0
      movsd 8(%eax),%xmm1
      movsd %xmm0,%xmm2
      subsd %xmm1,%xmm0
      addsd %xmm2,%xmm1
      movsd %xmm1,(%ecx)
      movsd %xmm0,8(%ecx)      

      ret
