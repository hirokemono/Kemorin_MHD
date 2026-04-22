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
.globl sxqtf1_
.globl _sxqtf1_	
sxqtf1_:
_sxqtf1_:	
      # rdi, rsi, rdx, rcx, r8, r9
	movq (%rdi),%rdi # rdi に JM/JV
	shlq $6,%rdi # rdi に (JM/JV)*64
	movq (%rsi),%rsi # rsi に MM
	shlq $6,%rsi # rsi に MM*64
	movq (%rdx),%rdx # rdx に IM
	shlq $5,%rdx # rdx に IM/2*64
	
	# rcx が W, r8 が G

	#	addq %r8,%rsi
	addq %rcx,%rsi	
	addq $64,%rsi
L0:
	vmovapd   (%rcx), %ymm0
	vmovapd 32(%rcx), %ymm1
	vmovapd %ymm0,  (%r8)
	vmovapd %ymm1,32(%r8)	

	addq $64,%rcx
	addq %rdi,%r8
	cmpq %rcx,%rsi
	jne L0

	ret
