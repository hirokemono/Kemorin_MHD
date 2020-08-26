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
.globl lxqpwv_
.globl _lxqpwv_	
lxqpwv_:
_lxqpwv_:
# rdi, rsi,rdx,rcx,r8,r9,8(%rsp)
	movq   (%rdi), %rdi  # JR
	movq   (%rsi), %rsi  # JM
	movq   (%rdx), %rdx  # ID
	# rcx,r8,r9: P, Q, G
	# rax, r10, r11 �� free

	movq 8(%rsp), %r10  # G2 �Υ��ɥ쥹		

	pushq %rbx
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15

	movq %r10, %r15  # G2 �Υ��ɥ쥹		

        shlq $5,%rdi # JR*8*4 �� rdi ��
	movq %rdi,%r12
	shlq $3,%r12
	addq %rdi,%r12  # 
	addq %rdi,%r12  # 
	addq %rdi,%r12  # r12 �� JB*8*4*11
	
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

	addq %rsi,%r15 # G2(JM) �Υ��ɥ쥹
	movq %r15,%r10
	movq %r15,%r11	
	addq %rdi,%r10	# G2-1 �Υ������ȥ��ɥ쥹
	subq %rdi,%r11	
	subq $64,%r11	# G2-3 �Υ������ȥ��ɥ쥹
	
	addq %rdx,%rcx # P�Υ������ȥ��ɥ쥹

	movq %r8,%rax # Q ����Ƭ���ɥ쥹�� rax ��
L00:
	vmovapd  96(%rax),%ymm4 # Q4
	vmovapd 128(%rax),%ymm5 # Q5
	vmovapd 160(%rax),%ymm6 # Q6
	vmovapd 192(%rax),%ymm7 # Q7

	vmovapd 224(%rax),%ymm0 # Q8
	vmovapd 256(%rax),%ymm1 # Q9
	vmovapd 288(%rax),%ymm2 # Q10
	vmovapd 320(%rax),%ymm3 # Q11		

	vmovapd (%rcx),%ymm15 # P
	vmulpd %ymm15,%ymm4,%ymm4 # Q4*P
	vmulpd %ymm15,%ymm6,%ymm6 # Q6*P
	vaddpd %ymm4,%ymm5,%ymm8 #  GQ1+Q5
	vsubpd %ymm4,%ymm5,%ymm5 # -GQ1+Q5	
	vaddpd %ymm6,%ymm7,%ymm4 #  GQ2+Q7
	vsubpd %ymm6,%ymm7,%ymm7 # -GQ2+Q7

	vmulpd %ymm15,%ymm0,%ymm0 # Q8*P
	vmulpd %ymm15,%ymm2,%ymm2 # Q10*P
	vaddpd %ymm0,%ymm1,%ymm9 #  GQ1+Q9
	vsubpd %ymm0,%ymm1,%ymm1 # -GQ1+Q9	
	vaddpd %ymm2,%ymm3,%ymm0 #  GQ2+Q11
	vsubpd %ymm2,%ymm3,%ymm3 # -GQ2+Q11

	vmovapd %ymm8,(%r13) # G1
	vmovapd %ymm4,32(%r13) # G2

	vmovapd %ymm9,(%r10) # G1
	vmovapd %ymm0,32(%r10) # G2
	
	vextractf128 $1,%ymm5,%xmm8 # ymm5 �ξ�� 128bit �� xmm8��
	vextractf128 $1,%ymm7,%xmm4 # ymm7 �ξ�� 128bit �� xmm4��
	vextractf128 $1,%ymm1,%xmm9 # ymm5 �ξ�� 128bit �� xmm8��
	vextractf128 $1,%ymm3,%xmm0 # ymm7 �ξ�� 128bit �� xmm4��	
	
	vinsertf128 $1,%xmm5,%ymm8,%ymm8 # ymm5 �β���Ⱦʬ�� ymm8�ξ��Ⱦʬ��
	vinsertf128 $1,%xmm7,%ymm4,%ymm4 # ymm5 �β���Ⱦʬ�� ymm4�ξ��Ⱦʬ��
	vinsertf128 $1,%xmm1,%ymm9,%ymm9 # ymm5 �β���Ⱦʬ�� ymm8�ξ��Ⱦʬ��
	vinsertf128 $1,%xmm3,%ymm0,%ymm0 # ymm5 �β���Ⱦʬ�� ymm4�ξ��Ⱦʬ��

	vshufpd $5,%ymm8,%ymm8,%ymm8 # �����ؤ�
	vshufpd $5,%ymm4,%ymm4,%ymm4 # �����ؤ�
	vshufpd $5,%ymm9,%ymm9,%ymm9 # �����ؤ�
	vshufpd $5,%ymm0,%ymm0,%ymm0 # �����ؤ�

	vmovapd %ymm8,(%r14) # G3
	vmovapd %ymm4,32(%r14) # G4
	vmovapd %ymm9,(%r11) # G3
	vmovapd %ymm0,32(%r11) # G4
	
	addq $352,%rax
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
	
