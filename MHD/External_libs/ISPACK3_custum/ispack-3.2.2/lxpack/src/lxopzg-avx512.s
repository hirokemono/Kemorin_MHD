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
.globl lxopzg_
.globl _lxopzg_	
lxopzg_:
_lxopzg_:
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

        shlq $6,%rdi # JR*8*8 が rdi に
	movq %rdi,%r12
	shlq $2,%r12
	addq %rdi,%r12  # r12 に JB*8*8*5 が入る
	addq %r8,%r12 # Q の終了用のアドレスが r12 に
	subq $1,%rdx # ID-1
	imulq %rdi,%rdx # JR*(ID-1)*64 (64=8*JV)
	movq %rdx,%rdi
	addq %rdx,%rdi	# JR*(ID-1)*128 (64=8*JV)
	shlq $3,%rsi  # JM*8
	addq %rsi,%r9 # G(JM) のアドレス
	movq %r9,%r13
	movq %r9,%r14	
	addq %rdi,%r13	# G1 のスタートアドレス
	subq %rdi,%r14	
	subq $128,%r14	# G3 のスタートアドレス
	addq %rdx,%rcx # Pのスタートアドレス

#        vsubpd %zmm8,%zmm8,%zmm8
	vpxorq %zmm8,%zmm8,%zmm8	

	movq %r8,%rax # Q の先頭アドレスを rax に
L00:
	vmovapd (%rcx),%zmm0 # P
	vmulpd 192(%rax),%zmm0,%zmm1 # Q4*P		
	vmovapd 256(%rax),%zmm3 # Q5
	
	vaddpd %zmm1,%zmm3,%zmm0 #  GQ1+Q5
	vsubpd %zmm1,%zmm3,%zmm3 # -GQ1+Q5
	
#	vshuff64x2 $27,%zmm3,%zmm3,%zmm3
	vshuff64x2 $78,%zmm3,%zmm3,%zmm3
	vpermpd $27,%zmm3,%zmm3

	vmovapd %zmm0,(%r13) # G1
	vmovapd %zmm8,64(%r13) # G2	
	vmovapd %zmm3,(%r14) # G3
	vmovapd %zmm8,64(%r14) # G4
		
	addq $320,%rax
	addq $64,%rcx
	addq $128,%r13
	subq $128,%r14	
	cmpq %rax,%r12
	jne L00

	popq %r14
	popq %r13
	popq %r12
	popq %rbp	
	popq %rbx

	ret
	
