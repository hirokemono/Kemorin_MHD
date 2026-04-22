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
.globl fxzqbp_
.globl _fxzqbp_	
fxzqbp_:
_fxzqbp_:	
	# N ¤Ï rdi, X ¤Ï rsi, IT ¤Ï rdx

	movq $0x8000000000000000,%rcx

	movq (%rdi),%rdi
	shlq $3,%rdi # N*8
	subq $24,%rdi # N*8-24

	movq $-8,%rax

L1:	addq $8,%rax
	cmpq %rdi,%rax
	jg L4
	movq (%rdx,%rax),%r8
	cmpq $0,%r8
	jl L1

	shlq $6,%r8
	vmovapd   (%rsi,%r8), %ymm0
	vmovapd 32(%rsi,%r8), %ymm1
	
L2:	addq $8,%rax
	movq (%rdx,%rax),%r9
	cmpq $0,%r9
	jl L3
	
	shlq $6,%r9
	vmovapd   (%rsi,%r9), %ymm2
	vmovapd 32(%rsi,%r9), %ymm3
	vmovapd %ymm2,  (%rsi,%r8)
	vmovapd %ymm3,32(%rsi,%r8)
	movq %r9,%r8
	jmp L2

L3: 	subq %rcx,%r9
	shlq $6,%r9
	vmovapd   (%rsi,%r9), %ymm2
	vmovapd 32(%rsi,%r9), %ymm3
	vmovapd %ymm0,  (%rsi,%r9)
	vmovapd %ymm1,32(%rsi,%r9)
	vmovapd %ymm2,  (%rsi,%r8)
	vmovapd %ymm3,32(%rsi,%r8)
	jmp L1

L4:
	ret
