########################################################################
# ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
# Copyright (C) 1998--2024 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
.section .note.GNU-stack,"",@progbits
.text
.globl sxotf1_
.globl _sxotf1_	
sxotf1_:
_sxotf1_:	
      # rdi, rsi, rdx, rcx, r8, r9
	movq (%rdi),%rdi # rdi に JM/JV
	shlq $7,%rdi # rdi に (JM/JV)*128
	movq (%rsi),%rsi # rsi に MM
	shlq $7,%rsi # rsi に MM*128
	movq (%rdx),%rdx # rdx に IM
	shlq $6,%rdx # rdx に IM/2*128
	
	# rcx が W, r8 が G

	addq %rcx,%rsi
	addq $128,%rsi	

L0:
	vmovapd (%rcx),%zmm0
	vmovapd 64(%rcx),%zmm1
	
	vmovapd %zmm0,(%r8)
	vmovapd %zmm1,64(%r8)

	addq $128,%rcx
	addq %rdi,%r8
	cmpq %rcx,%rsi
	jne L0
	
	ret
