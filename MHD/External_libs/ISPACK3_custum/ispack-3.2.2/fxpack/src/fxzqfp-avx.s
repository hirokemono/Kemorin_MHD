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
.globl fxzqfp_
.globl _fxzqfp_	
fxzqfp_:
_fxzqfp_:	
	# N は rdi, X は rsi, IT は rdx

	movq $0x8000000000000000,%rcx

	cvtsi2sdq (%rdi), %xmm0

	movsd C1(%rip),%xmm1
	divsd %xmm0, %xmm1
	movsd %xmm1,-8(%rsp)
	vbroadcastsd -8(%rsp),%ymm4 # 1/N
	vsubpd %ymm5,%ymm5,%ymm5
	vsubpd %ymm4,%ymm5,%ymm5    # -1/N

	movq (%rdi),%rdi
	movq %rdi,%r8 	
	shlq $3,%rdi # N*8
	subq $24,%rdi # N*8-24

	subq $1,%r8
	shlq $6,%r8 # (N-1)*64

	vmovapd   (%rsi), %ymm0
	vmovapd 32(%rsi), %ymm1
	vmulpd %ymm4,%ymm0,%ymm0
	vmulpd %ymm5,%ymm1,%ymm1
	vmovapd %ymm0,  (%rsi)
	vmovapd %ymm1,32(%rsi)
	
	vmovapd   (%rsi,%r8), %ymm0
	vmovapd 32(%rsi,%r8), %ymm1
	vmulpd %ymm4,%ymm0,%ymm0
	vmulpd %ymm5,%ymm1,%ymm1
	vmovapd %ymm0,  (%rsi,%r8)
	vmovapd %ymm1,32(%rsi,%r8)

	movq $-8,%rax

L1:	addq $8,%rax
	cmpq %rdi,%rax
	jg L5
	movq (%rdx,%rax),%r8
	cmpq $0,%r8
	jl L4

	shlq $6,%r8
	vmovapd   (%rsi,%r8), %ymm0
	vmovapd 32(%rsi,%r8), %ymm1
	vmulpd %ymm4,%ymm0,%ymm0
	vmulpd %ymm5,%ymm1,%ymm1
	
L2:	addq $8,%rax
	movq (%rdx,%rax),%r9
	cmpq $0,%r9
	jl L3
	
	shlq $6,%r9
	vmovapd   (%rsi,%r9), %ymm2
	vmovapd 32(%rsi,%r9), %ymm3
	vmulpd %ymm4,%ymm2,%ymm2
	vmulpd %ymm5,%ymm3,%ymm3
	vmovapd %ymm2,  (%rsi,%r8)
	vmovapd %ymm3,32(%rsi,%r8)
	movq %r9,%r8
	jmp L2

L3: 	subq %rcx,%r9
	shlq $6,%r9
	vmovapd   (%rsi,%r9), %ymm2
	vmovapd 32(%rsi,%r9), %ymm3
	vmulpd %ymm4,%ymm2,%ymm2
	vmulpd %ymm5,%ymm3,%ymm3
	vmovapd %ymm0,  (%rsi,%r9)
	vmovapd %ymm1,32(%rsi,%r9)
	vmovapd %ymm2,  (%rsi,%r8)
	vmovapd %ymm3,32(%rsi,%r8)
	jmp L1

L4:
	shlq $6,%r8
	vmovapd   (%rsi,%r8), %ymm0
	vmovapd 32(%rsi,%r8), %ymm1
	vmulpd %ymm4,%ymm0,%ymm0
	vmulpd %ymm5,%ymm1,%ymm1
	vmovapd %ymm0,  (%rsi,%r8)
	vmovapd %ymm1,32(%rsi,%r8)
	jmp L1

L5:
	ret

C1:	 # 倍精度不動小数点の 1
	.long   0x00000000,0x3ff00000
