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
.globl lxqpzv_
.globl _lxqpzv_	
lxqpzv_:
_lxqpzv_:
# rdi, rsi,rdx,rcx,r8,r9,8(%rsp)
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

	movq %r10,%r15  # G2 のアドレス		

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

	vxorpd %ymm14,%ymm14,%ymm14
	
	addq %rdx,%rcx # Pのスタートアドレス

	movq %r8,%rax # Q の先頭アドレスを rax に
L00:
	vmovapd  96(%rax),%ymm4 # Q4
	vmovapd 128(%rax),%ymm5 # Q5
	vmovapd 160(%rax),%ymm6 # Q6
	vmovapd 192(%rax),%ymm7 # Q7

	vmovapd (%rcx),%ymm15 # P
	vmulpd %ymm15,%ymm4,%ymm4 # Q4*P
	vmulpd %ymm15,%ymm6,%ymm6 # Q6*P
	vaddpd %ymm4,%ymm5,%ymm8 #  GQ1+Q5
	vsubpd %ymm4,%ymm5,%ymm5 # -GQ1+Q5	
	vaddpd %ymm6,%ymm7,%ymm4 #  GQ2+Q7
	vsubpd %ymm6,%ymm7,%ymm7 # -GQ2+Q7

	vmovapd %ymm8,(%r13) # G1
	vmovapd %ymm14,32(%r13) # G2
	vmovapd %ymm4,(%r10) # G1
	vmovapd %ymm14,32(%r10) # G2
	
	vpermpd $27,%ymm5,%ymm5
	vpermpd $27,%ymm7,%ymm7

	vmovapd %ymm5,(%r14) # G3
	vmovapd %ymm14,32(%r14) # G4
	vmovapd %ymm7,(%r11) # G3
	vmovapd %ymm14,32(%r11) # G4
	
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
	
