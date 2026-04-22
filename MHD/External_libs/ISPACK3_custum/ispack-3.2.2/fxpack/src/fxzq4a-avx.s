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
.globl fxzq4a_
.globl _fxzq4a_	
fxzq4a_:
_fxzq4a_:
	vbroadcastsd C2(%rip),%ymm15 # 倍精度不動小数点の 2 を ymm15 に
	movq (%rdi), %rdi  # K が rdi に
	movq (%rsi), %rsi  # L が rsi に	
        # X の先頭アドレスは rdx
        # T の先頭アドレスは rcx
	# r8 空き

	shlq $4,%rsi # L/4*2*M*8=L*16
	movq %rsi,%r11
	shlq $2,%r11 # L/4*2*M*8*4=L*64
	
	movq %rdx,%r8
	addq %rsi,%r8 # X(1,1,0,1) のスタートアドレス
	movq %r8,%r9
	addq %rsi,%r9 # X(1,1,0,2) のスタートアドレス
	movq %r9,%r10
	addq %rsi,%r10 # X(1,1,0,3) のスタートアドレス

L0:	movq $0,%rax
	vbroadcastsd   (%rcx), %ymm8  # T1R
	vbroadcastsd  8(%rcx), %ymm9  # T1I
	vbroadcastsd 16(%rcx), %ymm10 # T2R
	vbroadcastsd 24(%rcx), %ymm11 # T2I

L1:	vmovapd   (%rdx,%rax), %ymm0 # X(1,0)
	vmovapd 32(%rdx,%rax), %ymm1 # X(2,0)
	vmovapd   (%r8, %rax), %ymm2 # X(1,1)
	vmovapd 32(%r8, %rax), %ymm3 # X(2,1)
	vmovapd   (%r9, %rax), %ymm4 # X(1,2)
	vmovapd 32(%r9, %rax), %ymm5 # X(2,2)
	vmovapd   (%r10,%rax), %ymm6 # X(1,3)
	vmovapd 32(%r10,%rax), %ymm7 # X(2,3)

	vmulpd %ymm10,%ymm4,%ymm12
	vsubpd %ymm12,%ymm0,%ymm12	
	vmulpd %ymm11,%ymm4,%ymm4	
	vsubpd %ymm4,%ymm1,%ymm4
	
	vmulpd %ymm11,%ymm5,%ymm13
	vaddpd %ymm13,%ymm12,%ymm12 # X2R	
	vmulpd %ymm10,%ymm5,%ymm5
	vsubpd %ymm5,%ymm4,%ymm4 # X2I (ymm5 空き)

	vmulpd %ymm15,%ymm0,%ymm0 # 2*X(1,0)
	vmulpd %ymm15,%ymm1,%ymm1 # 2*X(2,0)		
	vsubpd %ymm12,%ymm0,%ymm0 # X0R
	vsubpd %ymm4,%ymm1,%ymm1  # X0I

	vmulpd %ymm10,%ymm6,%ymm13
	vsubpd %ymm13,%ymm2,%ymm13
	vmulpd %ymm11,%ymm6,%ymm6
	vsubpd %ymm6,%ymm3,%ymm6

	vmulpd %ymm11,%ymm7,%ymm5
	vaddpd %ymm5,%ymm13,%ymm5 # X3R	
	vmulpd %ymm10,%ymm7,%ymm7
	vsubpd %ymm7,%ymm6,%ymm6 # X3I 
	
	vmulpd %ymm15,%ymm2,%ymm2 # 2*X(1,1)
	vmulpd %ymm15,%ymm3,%ymm3 # 2*X(2,1)		
	vsubpd %ymm5,%ymm2,%ymm2 # X1R
	vsubpd %ymm6,%ymm3,%ymm3 # X1I (ymm7, 13 空き)

	#---
	vmulpd %ymm8,%ymm2,%ymm13
	vsubpd %ymm13,%ymm0,%ymm13
	vmulpd %ymm9,%ymm2,%ymm2	
	vsubpd %ymm2,%ymm1,%ymm2

	vmulpd %ymm9,%ymm3,%ymm7
	vaddpd %ymm7,%ymm13,%ymm13 # X(1,2)'
	vmulpd %ymm8,%ymm3,%ymm3 
	vsubpd %ymm3,%ymm2,%ymm2 # X(2,2)' (ymm3 空き)
	
	vmulpd %ymm15,%ymm0,%ymm0 # 2*X0R
	vmulpd %ymm15,%ymm1,%ymm1 # 2*X0I
	vsubpd %ymm13,%ymm0,%ymm0 # X(1,0)'
	vsubpd %ymm2,%ymm1,%ymm1  # X(2,0)'

	vmulpd %ymm8,%ymm6,%ymm3
	vsubpd %ymm3,%ymm12,%ymm3
	vmulpd %ymm9,%ymm6,%ymm6
	vsubpd %ymm6,%ymm4,%ymm6 
	
	vmulpd %ymm9,%ymm5,%ymm7
	vsubpd %ymm7,%ymm3,%ymm3 # X(1,1)'
	vmulpd %ymm8,%ymm5,%ymm5
	vaddpd %ymm5,%ymm6,%ymm6 # X(2,1)' (ymm5 空き)

	vmulpd %ymm15,%ymm12,%ymm12 # 2*X2R
	vmulpd %ymm15,%ymm4,%ymm4 # 2*X0I
	vsubpd %ymm3,%ymm12,%ymm12 # X(1,3)'
	vsubpd %ymm6,%ymm4,%ymm4  # X(2,3)'

	vmovapd %ymm0,  (%rdx,%rax)
	vmovapd %ymm1,32(%rdx,%rax)
	vmovapd %ymm3,  (%r8,%rax)
	vmovapd %ymm6,32(%r8,%rax)
	vmovapd %ymm13,  (%r9,%rax)
	vmovapd %ymm2,32(%r9,%rax)
	vmovapd %ymm12,  (%r10,%rax)
	vmovapd %ymm4,32(%r10,%rax)

	addq $64,%rax	
	cmpq %rsi,%rax
	jne L1

	addq %r11,%rdx
	addq %r11,%r8	
	addq %r11,%r9
	addq %r11,%r10
	addq $48,%rcx

	subq $1,%rdi
	jnz L0
	
	ret
C2: # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000	
