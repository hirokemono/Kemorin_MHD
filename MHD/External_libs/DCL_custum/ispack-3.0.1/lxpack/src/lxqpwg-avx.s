########################################################################
# ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
# Copyright (C) 1998--2019 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
	# rax, r10, r11 �� free

	pushq %rbx
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15

        shlq $5,%rdi # JR*8*4 �� rdi ��
	movq %rdi,%r12
	shlq $3,%r12
	subq %rdi,%r12  # r12 �� JB*8*4*7 ������
	addq %r8,%r12 # Q �ν�λ�ѤΥ��ɥ쥹�� r12 ��
	subq $1,%rdx # ID-1
	imulq %rdi,%rdx # JR*(ID-1)*32 (32=8*JV)
	movq %rdx,%rdi
	addq %rdx,%rdi	# JR*(ID-1)*64 (32=8*JV)
	shlq $3,%rsi  # JM*8
	addq %rsi,%r9 # G(JM) �Υ��ɥ쥹
	movq %r9,%r13
	movq %r9,%r14	
	addq %rdi,%r13	# G1 �Υ������ȥ��ɥ쥹
	subq %rdi,%r14	
	subq $64,%r14	# G3 �Υ������ȥ��ɥ쥹
	addq %rdx,%rcx # P�Υ������ȥ��ɥ쥹

	movq %r8,%rax # Q ����Ƭ���ɥ쥹�� rax ��
L00:
	vmovapd  96(%rax),%ymm4 # Q4
	vmovapd 128(%rax),%ymm5 # Q5
	vmovapd 160(%rax),%ymm6 # Q6
	vmovapd 192(%rax),%ymm7 # Q7
	vmovapd (%rcx),%ymm8 # P
	vmulpd %ymm8,%ymm4,%ymm4 # Q4*P
	vmulpd %ymm8,%ymm6,%ymm6 # Q6*P
	vaddpd %ymm4,%ymm5,%ymm8 #  GQ1+Q5
	vsubpd %ymm4,%ymm5,%ymm5 # -GQ1+Q5	
	vaddpd %ymm6,%ymm7,%ymm4 #  GQ2+Q7
	vsubpd %ymm6,%ymm7,%ymm7 # -GQ2+Q7

	vmovapd %ymm8,(%r13) # G1
	vmovapd %ymm4,32(%r13) # G2

	vextractf128 $1,%ymm5,%xmm8 # ymm5 �ξ�� 128bit �� xmm8��
	vextractf128 $1,%ymm7,%xmm4 # ymm7 �ξ�� 128bit �� xmm4��	

	vinsertf128 $1,%xmm5,%ymm8,%ymm8 # ymm5 �β���Ⱦʬ�� ymm8�ξ��Ⱦʬ��
	vinsertf128 $1,%xmm7,%ymm4,%ymm4 # ymm5 �β���Ⱦʬ�� ymm4�ξ��Ⱦʬ��

	vshufpd $5,%ymm8,%ymm8,%ymm8 # �����ؤ�
	vshufpd $5,%ymm4,%ymm4,%ymm4 # �����ؤ�

	vmovapd %ymm8,(%r14) # G3
	vmovapd %ymm4,32(%r14) # G4
	
	addq $224,%rax
	addq $32,%rcx
	addq $64,%r13
	subq $64,%r14	
	cmpq %rax,%r12
	jne L00

	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbp	
	popq %rbx

	ret
	
