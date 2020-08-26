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
.globl fxzo4b_
.globl _fxzo4b_	
fxzo4b_:
_fxzo4b_:

	movq (%rdi), %rdi  # L �� rdi ��
	# X ����Ƭ���ɥ쥹�� rsi

	#------------------------

	shlq $5,%rdi # L/4*2*M*8*2=L*32
	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) �Υ������ȥ��ɥ쥹
	movq %rdx,%r8
	addq %rdi,%r8 # X(1,1,0,2) �Υ������ȥ��ɥ쥹
	movq %r8,%r9
	addq %rdi,%r9 # X(1,1,0,3) �Υ������ȥ��ɥ쥹

	movq $0,%rax

L1:	vmovapd   (%rsi,%rax), %zmm0 # X(1,0)
	vmovapd 64(%rsi,%rax), %zmm1 # X(2,0)
	vmovapd   (%rdx,%rax), %zmm2 # X(1,1)
	vmovapd 64(%rdx,%rax), %zmm3 # X(2,1)
	vmovapd   (%r8, %rax), %zmm4 # X(1,2)
	vmovapd 64(%r8, %rax), %zmm5 # X(2,2)
	vmovapd   (%r9, %rax), %zmm6 # X(1,3)
	vmovapd 64(%r9, %rax), %zmm7 # X(2,3)

	vsubpd %zmm4,%zmm0,%zmm8 # X2R
	vsubpd %zmm5,%zmm1,%zmm9 # X2I
	vaddpd %zmm4,%zmm0,%zmm0 # X0R
	vaddpd %zmm5,%zmm1,%zmm1 # X0I
	vsubpd %zmm6,%zmm2,%zmm10 # X3R
	vsubpd %zmm7,%zmm3,%zmm11 # X3I
	vaddpd %zmm6,%zmm2,%zmm2 # X1R
	vaddpd %zmm7,%zmm3,%zmm3 # X1I

	vsubpd %zmm2,%zmm0,%zmm12 # X(1,2)'
	vsubpd %zmm3,%zmm1,%zmm13 # X(2,2)'	
	vaddpd %zmm2,%zmm0,%zmm0 # X(1,0)'
	vaddpd %zmm3,%zmm1,%zmm1 # X(2,0)'

	vsubpd %zmm11,%zmm8,%zmm2 # X(1,1)'
	vaddpd %zmm10,%zmm9,%zmm3 # X(2,1)'		
	vaddpd %zmm11,%zmm8,%zmm8 # X(1,3)'
	vsubpd %zmm10,%zmm9,%zmm9 # X(2,3)'		
	
	vmovapd %zmm0,  (%rsi,%rax)
	vmovapd %zmm1,64(%rsi,%rax)
	vmovapd %zmm2,  (%rdx,%rax)
	vmovapd %zmm3,64(%rdx,%rax)
	vmovapd %zmm12,  (%r8,%rax)
	vmovapd %zmm13,64(%r8,%rax)
	vmovapd %zmm8,  (%r9,%rax)
	vmovapd %zmm9,64(%r9,%rax)

	addq $128,%rax	
	cmpq %rdi,%rax
	jne L1

	ret
       
	
