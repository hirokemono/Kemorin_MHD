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
.globl lxqvzp_
.globl _lxqvzp_	
lxqvzp_:
_lxqvzp_:
# rdi, rsi,rdx,rcx,r8,r9	
	movq   (%rdi), %rdi  # JR
	movq   (%rsi), %rsi  # JM
	movq   (%rdx), %rdx  # ID
	# rcx,r8,r9: P, Q, G
	# rax, r10, r11 は free

	movq 8(%rsp), %r10  # G2 のアドレス			

	pushq %rbx
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15

	movq %r10, %r15  # G2 のアドレス			

        shlq $5,%rdi # JR*8*4 が rdi に
	movq %rdi,%r12
	shlq $3,%r12
	subq %rdi,%r12  # r12 に JB*8*4*7
	
	addq %r8,%r12 # Q の終了用のアドレスが r12 に
	subq $1,%rdx # ID-1
	imulq %rdi,%rdx # JR*(ID-1)*32 (32=8*JV)
	movq %rdx,%rdi
	addq %rdx,%rdi	# JR*(ID-1)*64 (32=8*JV)
	shlq $3,%rsi  # JM*8
	addq %rsi,%r9 # G(JM) のアドレス
	movq %r9,%r13
	movq %r9,%r14	
	addq %rdi,%r13	# G1 のスタートアドレス
	subq %rdi,%r14	
	subq $64,%r14	# G3 のスタートアドレス

	addq %rsi,%r15 # G2(JM) のアドレス
	movq %r15,%r10
	movq %r15,%r11	
	addq %rdi,%r10	# G2-1 のスタートアドレス
	subq %rdi,%r11	
	subq $64,%r11	# G2-3 のスタートアドレス
	
	addq %rdx,%rcx # Pのスタートアドレス

	shrq $1,%rsi  # JM/2*8

	movq %r8,%rax # Q の先頭アドレスを rax に
L00:
	vmovapd (%r14),%ymm8 # G3
	vmovapd (%r11),%ymm11 # G3-2

	vpermpd $27,%ymm8,%ymm8
	vpermpd $27,%ymm11,%ymm11

	vmovapd (%rcx,%rsi),%ymm10 # P2	
	vmulpd (%rcx),%ymm10,%ymm9 # P1*P2
	
	vmovapd (%r13),%ymm2 # G1
	vmovapd (%r10),%ymm3 # G1-2
	
	vaddpd %ymm8,%ymm2,%ymm0
	vsubpd %ymm8,%ymm2,%ymm1

	vaddpd %ymm11,%ymm3,%ymm4
	vsubpd %ymm11,%ymm3,%ymm5

	vmulpd %ymm10,%ymm0,%ymm0
	vmulpd %ymm9,%ymm1,%ymm1	
	
	vmulpd %ymm10,%ymm4,%ymm4
	vmulpd %ymm9,%ymm5,%ymm5
	
	vmovapd %ymm1, 96(%rax)
	vmovapd %ymm0,128(%rax)
	vmovapd %ymm5,160(%rax)
	vmovapd %ymm4,192(%rax)
	
	addq $224,%rax
	addq $32,%rcx
	addq $64,%r13
	subq $64,%r14	
	addq $64,%r10
	subq $64,%r11	
	cmpq %rax,%r12
	jne L00

	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbp	
	popq %rbx

	ret
	
