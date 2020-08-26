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
.globl lxovwp_
.globl _lxovwp_	
lxovwp_:
_lxovwp_:
# rdi, rsi,rdx,rcx,r8,r9	
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

        shlq $6,%rdi # JR*8*8 �� rdi ��
	movq %rdi,%r12
	shlq $3,%r12
	addq %rdi,%r12  # 
	addq %rdi,%r12  # 
	addq %rdi,%r12  # r12 �� JB*8*8*11
	addq %r8,%r12 # Q �ν�λ�ѤΥ��ɥ쥹�� r12 ��
	subq $1,%rdx # ID-1
	imulq %rdi,%rdx # JR*(ID-1)*64 (64=8*JV)
	movq %rdx,%rdi
	addq %rdx,%rdi	# JR*(ID-1)*128 (64=8*JV)
	shlq $3,%rsi  # JM*8
	addq %rsi,%r9 # G(JM) �Υ��ɥ쥹
	movq %r9,%r13
	movq %r9,%r14	
	addq %rdi,%r13	# G1 �Υ������ȥ��ɥ쥹
	subq %rdi,%r14	
	subq $128,%r14	# G3 �Υ������ȥ��ɥ쥹

	addq %rsi,%r15 # G2(JM) �Υ��ɥ쥹
	movq %r15,%r10
	movq %r15,%r11	
	addq %rdi,%r10	# G2-1 �Υ������ȥ��ɥ쥹
	subq %rdi,%r11	
	subq $128,%r11	# G2-3 �Υ������ȥ��ɥ쥹
	
	addq %rdx,%rcx # P�Υ������ȥ��ɥ쥹

	shrq $1,%rsi  # JM/2*8	

	movq %r8,%rax # Q ����Ƭ���ɥ쥹�� rax ��
L00:
	vmovapd (%r14),%zmm8 # G3
	vmovapd 64(%r14),%zmm4 # G4

	vmovapd (%r11),%zmm13 # G3-2
	vmovapd 64(%r11),%zmm14 # G4-2

	vshuff64x2 $78,%zmm8,%zmm8,%zmm8
	vpermpd $27,%zmm8,%zmm8
	vshuff64x2 $78,%zmm4,%zmm4,%zmm4
	vpermpd $27,%zmm4,%zmm4

	vshuff64x2 $78,%zmm13,%zmm13,%zmm13
	vpermpd $27,%zmm13,%zmm13
	vshuff64x2 $78,%zmm14,%zmm14,%zmm14
	vpermpd $27,%zmm14,%zmm14
	
	vmovapd (%rcx),%zmm9 # P1	
	vmovapd (%rcx,%rsi),%zmm10 # P2
	vmulpd  %zmm10,%zmm9,%zmm9 # P1*P2	
	vmovapd (%r13),%zmm12 # G1
	vmovapd 64(%r13),%zmm11 # G2

	vmovapd (%r10),%zmm5 # G1-2
	vmovapd 64(%r10),%zmm6 # G2-2

	vaddpd %zmm8,%zmm12,%zmm0
	vsubpd %zmm8,%zmm12,%zmm1
	vaddpd %zmm4,%zmm11,%zmm2
	vsubpd %zmm4,%zmm11,%zmm3

	vaddpd %zmm13,%zmm5,%zmm4
	vsubpd %zmm13,%zmm5,%zmm5
	vaddpd %zmm14,%zmm6,%zmm7
	vsubpd %zmm14,%zmm6,%zmm8
	
	vmulpd %zmm10,%zmm0,%zmm0
	vmulpd %zmm9,%zmm1,%zmm1	
	vmulpd %zmm10,%zmm2,%zmm2
	vmulpd %zmm9,%zmm3,%zmm3	

	vmulpd %zmm10,%zmm4,%zmm4
	vmulpd %zmm9,%zmm5,%zmm5
	vmulpd %zmm10,%zmm7,%zmm7
	vmulpd %zmm9,%zmm8,%zmm8
	
	vmovapd %zmm1,192(%rax)
	vmovapd %zmm0,256(%rax)
	vmovapd %zmm3,320(%rax)
	vmovapd %zmm2,384(%rax)

	vmovapd %zmm5,448(%rax)
	vmovapd %zmm4,512(%rax)
	vmovapd %zmm8,576(%rax)
	vmovapd %zmm7,640(%rax)
	
	addq $704,%rax
	addq $64,%rcx
	addq $128,%r13
	subq $128,%r14	
	addq $128,%r10
	subq $128,%r11	
	cmpq %rax,%r12
	jne L00

	popq %r15	
	popq %r14
	popq %r13
	popq %r12
	popq %rbp	
	popq %rbx

	ret
	
