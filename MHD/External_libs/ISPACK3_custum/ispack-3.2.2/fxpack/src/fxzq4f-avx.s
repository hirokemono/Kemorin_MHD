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
.globl fxzq4f_
.globl _fxzq4f_	
fxzq4f_:
_fxzq4f_:
	vbroadcastsd CM2(%rip),%ymm15
	# 倍精度不動小数点の -2 を ymm15 の4箇所に	
	

	movq (%rdi), %rdi  # L が rdi に
	# X の先頭アドレスは rsi

	#------------------------

	shlq $4,%rdi # L/4*2*M*8=L*16
	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) のスタートアドレス
	movq %rdx,%r8
	addq %rdi,%r8 # X(1,1,0,2) のスタートアドレス
	movq %r8,%r9
	addq %rdi,%r9 # X(1,1,0,3) のスタートアドレス

	movq $0,%rax

L1:	vmovapd   (%rsi,%rax), %ymm0 # X(1,0)
	vmovapd 32(%rsi,%rax), %ymm1 # X(2,0)
	vmovapd   (%rdx,%rax), %ymm2 # X(1,1)
	vmovapd 32(%rdx,%rax), %ymm3 # X(2,1)
	vmovapd   (%r8, %rax), %ymm4 # X(1,2)
	vmovapd 32(%r8, %rax), %ymm5 # X(2,2)
	vmovapd   (%r9, %rax), %ymm6 # X(1,3)
	vmovapd 32(%r9, %rax), %ymm7 # X(2,3)

	vsubpd %ymm4,%ymm0,%ymm8 # X2R
	vsubpd %ymm1,%ymm5,%ymm9 # X2I
	vaddpd %ymm4,%ymm0,%ymm0 # X0R
	vaddpd %ymm5,%ymm1,%ymm1 # -X0I
	vsubpd %ymm6,%ymm2,%ymm10 # X3R
	vsubpd %ymm3,%ymm7,%ymm11 # X3I
	vaddpd %ymm6,%ymm2,%ymm2 # X1R
	vaddpd %ymm7,%ymm3,%ymm3 # -X1I

	vsubpd %ymm2,%ymm0,%ymm12 # X(1,2)'
	vsubpd %ymm1,%ymm3,%ymm13 # X(2,2)'	
	vaddpd %ymm2,%ymm0,%ymm0 # X(1,0)'
	#	vsubpd %ymm3,%ymm1,%ymm1 # X(2,0)'
	vmulpd %ymm15,%ymm1,%ymm1
	vsubpd %ymm13,%ymm1,%ymm1 # X(2,0)'	

	vsubpd %ymm11,%ymm8,%ymm2 # X(1,1)'
	vaddpd %ymm10,%ymm9,%ymm3 # X(2,1)'		
	vaddpd %ymm11,%ymm8,%ymm8 # X(1,3)'
	vsubpd %ymm10,%ymm9,%ymm9 # X(2,3)'		
	
	vmovapd %ymm0,  (%rsi,%rax)
	vmovapd %ymm1,32(%rsi,%rax)
	vmovapd %ymm2,  (%rdx,%rax)
	vmovapd %ymm3,32(%rdx,%rax)
	vmovapd %ymm12,  (%r8,%rax)
	vmovapd %ymm13,32(%r8,%rax)
	vmovapd %ymm8,  (%r9,%rax)
	vmovapd %ymm9,32(%r9,%rax)

	addq $64,%rax	
	cmpq %rdi,%rax
	jne L1

	ret
CM2: # 倍精度不動小数点の -2
	.long   0x00000000,0xc0000000
