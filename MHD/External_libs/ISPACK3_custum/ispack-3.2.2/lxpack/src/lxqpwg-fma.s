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
.globl lxqpwg_
.globl _lxqpwg_	
lxqpwg_:
_lxqpwg_:
# rdi, rsi,rdx,rcx,r8,r9	
	movq   (%rdi), %rdi  # JR
	movq   (%rsi), %rsi  # JM
	movq   (%rdx), %rdx  # ID
	# rcx,r8,r9: P, Q, G
	# rax, r10, r11 は free

	pushq %rbx
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14

        shlq $5,%rdi # JR*8*4 が rdi に
	movq %rdi,%r12
	shlq $3,%r12
	subq %rdi,%r12  # r12 に JB*8*4*7 が入る
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
	addq %rdx,%rcx # Pのスタートアドレス

	movq %r8,%rax # Q の先頭アドレスを rax に
L00:
	vmovapd (%rcx),%ymm0 # P
	vmulpd 96(%rax),%ymm0,%ymm1 # Q4*P		
	vmovapd 128(%rax),%ymm3 # Q5
	vmulpd 160(%rax),%ymm0,%ymm2 # Q6*P		
	vmovapd 192(%rax),%ymm4 # Q7	
	
	vaddpd %ymm1,%ymm3,%ymm0 #  GQ1+Q5
	vsubpd %ymm1,%ymm3,%ymm3 # -GQ1+Q5
	vaddpd %ymm2,%ymm4,%ymm1 #  GQ2+Q7
	vsubpd %ymm2,%ymm4,%ymm4 # -GQ2+Q7
	
	vpermpd $27,%ymm3,%ymm3
	vpermpd $27,%ymm4,%ymm4

	vmovapd %ymm0,(%r13) # G1
	vmovapd %ymm1,32(%r13) # G2	
	vmovapd %ymm3,(%r14) # G3
	vmovapd %ymm4,32(%r14) # G4
		
	addq $224,%rax
	addq $32,%rcx
	addq $64,%r13
	subq $64,%r14	
	cmpq %rax,%r12
	jne L00

	popq %r14
	popq %r13
	popq %r12
	popq %rbp	
	popq %rbx

	ret
	
