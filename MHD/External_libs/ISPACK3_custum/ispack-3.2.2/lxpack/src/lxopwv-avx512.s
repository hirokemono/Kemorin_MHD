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
.globl lxopwv_
.globl _lxopwv_	
lxopwv_:
_lxopwv_:
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

        shlq $6,%rdi # JR*8*8 が rdi に
	movq %rdi,%r12
	shlq $3,%r12
	addq %rdi,%r12  # 
	addq %rdi,%r12  # 
	addq %rdi,%r12  # r12 に JB*8*4*11

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

	addq %rsi,%r15 # G2(JM) のアドレス
	movq %r15,%r10
	movq %r15,%r11	
	addq %rdi,%r10	# G2-1 のスタートアドレス
	subq %rdi,%r11	
	subq $128,%r11	# G2-3 のスタートアドレス
	
	addq %rdx,%rcx # Pのスタートアドレス

	movq %r8,%rax # Q の先頭アドレスを rax に
L00:
	vmovapd (%rcx),%zmm0 # P
	vmulpd 192(%rax),%zmm0,%zmm1 # Q4*P		
	vmovapd 256(%rax),%zmm3 # Q5
	vmulpd 320(%rax),%zmm0,%zmm2 # Q6*P		
	vmovapd 384(%rax),%zmm4 # Q7

	vmulpd 448(%rax),%zmm0,%zmm5 # Q4*P		
	vmovapd 512(%rax),%zmm7 # Q5
	vmulpd 576(%rax),%zmm0,%zmm6 # Q6*P		
	vmovapd 640(%rax),%zmm8 # Q7	
	
	vaddpd %zmm1,%zmm3,%zmm0 #  GQ1+Q5
	vsubpd %zmm1,%zmm3,%zmm3 # -GQ1+Q5
	vaddpd %zmm2,%zmm4,%zmm1 #  GQ2+Q7
	vsubpd %zmm2,%zmm4,%zmm4 # -GQ2+Q7

	vaddpd %zmm5,%zmm7,%zmm9 #  GQ1+Q5
	vsubpd %zmm5,%zmm7,%zmm7 # -GQ1+Q5
	vaddpd %zmm6,%zmm8,%zmm5 #  GQ2+Q7
	vsubpd %zmm6,%zmm8,%zmm8 # -GQ2+Q7
	
	vshuff64x2 $78,%zmm3,%zmm3,%zmm3	
	vpermpd $27,%zmm3,%zmm3
	vshuff64x2 $78,%zmm4,%zmm4,%zmm4
	vpermpd $27,%zmm4,%zmm4

	vshuff64x2 $78,%zmm7,%zmm7,%zmm7
	vpermpd $27,%zmm7,%zmm7
	vshuff64x2 $78,%zmm8,%zmm8,%zmm8
	vpermpd $27,%zmm8,%zmm8

	vmovapd %zmm0,(%r13) # G1
	vmovapd %zmm1,64(%r13) # G2	
	vmovapd %zmm3,(%r14) # G3
	vmovapd %zmm4,64(%r14) # G4
		
	vmovapd %zmm9,(%r10) # G1
	vmovapd %zmm5,64(%r10) # G2	
	vmovapd %zmm7,(%r11) # G3
	vmovapd %zmm8,64(%r11) # G4
		
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
	
