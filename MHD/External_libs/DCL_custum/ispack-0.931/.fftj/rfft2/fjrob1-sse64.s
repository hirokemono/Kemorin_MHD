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
# rfft 2 out-of-place backward
.text
.globl fjrob1_
fjrob1_:
      movsd  (%rdi),%xmm0
      movsd 8(%rdi),%xmm1
      movsd %xmm0,%xmm2
      subsd %xmm1,%xmm0
      addsd %xmm2,%xmm1
      movsd %xmm1,(%rsi)
      movsd %xmm0,8(%rsi)      

      ret
