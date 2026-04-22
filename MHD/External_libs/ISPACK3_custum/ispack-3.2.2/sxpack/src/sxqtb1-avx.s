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
.globl sxqtb1_
.globl _sxqtb1_	
sxqtb1_:
_sxqtb1_:	
      # rdi, rsi, rdx, rcx, r8, r9
	movq (%rdi),%rdi # rdi に JM/JV
	shlq $6,%rdi # rdi に (JM/JV)*64
	movq (%rsi),%rsi # rsi に MM
	shlq $6,%rsi # rsi に MM*64
	movq (%rdx),%rdx # rdx に IM
	shlq $5,%rdx # rdx に IM/2*64
	
	# rcx が W, r8 が G

	vxorpd %ymm2,%ymm2,%ymm2

	addq %r8,%rsi
	addq $64,%rsi	
	addq %r8,%rdx		
L0:
	vmovapd   (%rcx), %ymm0
	vmovapd 32(%rcx), %ymm1
	vmovapd %ymm0,  (%r8)
	vmovapd %ymm1,32(%r8)	

	addq $64,%r8
	addq %rdi,%rcx
	cmpq %r8,%rsi
	jne L0

	cmpq %r8,%rdx
	je LE

L1:
	vmovapd %ymm2,  (%r8)
	vmovapd %ymm2,32(%r8)	

	addq $64,%r8
	cmpq %r8,%rdx
	jne L1
LE:	
	
	ret
