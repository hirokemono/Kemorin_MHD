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
.globl fxzq2a_
.globl _fxzq2a_	
fxzq2a_:
_fxzq2a_:
	vbroadcastsd C2(%rip),%ymm15 # 倍精度不動小数点の 2 を ymm15 に
	movq (%rdi), %rdi  # K が rdi に
	movq (%rsi), %rsi  # L が rsi に
        # X の先頭アドレスは rdx
        # T の先頭アドレスは rcx
	# r8 空き

	shlq $5,%rsi # L/2*2*M*8=L*32

	movq %rdx,%r8
	addq %rsi,%r8 # X(1,1,0,1) のスタートアドレス

L0:	movq $0,%rax
	vbroadcastsd  (%rcx), %ymm8 # T(1,J)
	vbroadcastsd 8(%rcx), %ymm9 # T(2,J)

L1:	vmovapd   (%rdx,%rax), %ymm0 # X(1,0)
	vmovapd 32(%rdx,%rax), %ymm1 # X(2,0)
	vmovapd   (%r8,%rax), %ymm2 # X(1,1)
	vmovapd 32(%r8,%rax), %ymm3 # X(2,1)

	vmulpd %ymm8,%ymm2,%ymm10
	vmulpd %ymm9,%ymm2,%ymm11
	vsubpd %ymm10,%ymm0,%ymm10 # X1R
	vsubpd %ymm11,%ymm1,%ymm11 # X1I

	vmulpd %ymm9,%ymm3,%ymm12
	vmulpd %ymm8,%ymm3,%ymm13
	vaddpd %ymm12,%ymm10,%ymm10 # X(1,1)'
	vsubpd %ymm13,%ymm11,%ymm11 # X(2,1)'

	vmulpd %ymm15,%ymm0,%ymm0 # 2*X(1,0)
	vmulpd %ymm15,%ymm1,%ymm1 # 2*X(2,0)		
	vsubpd %ymm10,%ymm0,%ymm0 # X(1,0)'
	vsubpd %ymm11,%ymm1,%ymm1 # X(2,0)'	

	vmovapd %ymm0,  (%rdx,%rax)
	vmovapd %ymm1,32(%rdx,%rax)
	vmovapd %ymm10,  (%r8,%rax)
	vmovapd %ymm11,32(%r8,%rax)

	addq $64,%rax	
	cmpq %rsi,%rax
	jne L1

	addq %rsi,%rdx
	addq %rsi,%rdx	
	addq %rsi,%r8
	addq %rsi,%r8	
	addq $16,%rcx

	subq $1,%rdi
	jnz L0
	
	ret
C2: # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000	
