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
.globl sxotf2_
.globl _sxotf2_	
sxotf2_:
_sxotf2_:	
      # rdi, rsi, rdx
	movq (%rdi),%rdi # rdi ¤Ë IM
	shlq $3,%rdi # rdi ¤Ë IM*8

	pushq %r12
	pushq %r13
	
	lea (%rdi,%rdi),%r8
	lea (%r8,%rdi),%r9
	lea (%r9,%rdi),%r10
	lea (%r10,%rdi),%r11
	lea (%r11,%rdi),%r12
	lea (%r12,%rdi),%r13
	
	lea (%rsi,%rdi),%rcx	
L0:
	vmovapd (%rsi), %zmm0
	vmovapd (%rsi,%rdi), %zmm1
	vmovapd (%rsi,%r8), %zmm2
	vmovapd (%rsi,%r9), %zmm3
	vmovapd (%rsi,%r10), %zmm4
	vmovapd (%rsi,%r11), %zmm5
	vmovapd (%rsi,%r12), %zmm6
	vmovapd (%rsi,%r13), %zmm7

	vunpcklpd %zmm1,%zmm0,%zmm8
	vunpckhpd %zmm1,%zmm0,%zmm9
	vunpcklpd %zmm3,%zmm2,%zmm10
	vunpckhpd %zmm3,%zmm2,%zmm11
	vunpcklpd %zmm5,%zmm4,%zmm12
	vunpckhpd %zmm5,%zmm4,%zmm13
	vunpcklpd %zmm7,%zmm6,%zmm14
	vunpckhpd %zmm7,%zmm6,%zmm15

	vshuff64x2 $136,%zmm10,%zmm8, %zmm0
	vshuff64x2 $221,%zmm10,%zmm8, %zmm1
	vshuff64x2 $136,%zmm14,%zmm12,%zmm2
	vshuff64x2 $221,%zmm14,%zmm12,%zmm3
	vshuff64x2 $136,%zmm11,%zmm9, %zmm4
	vshuff64x2 $221,%zmm11,%zmm9, %zmm5
	vshuff64x2 $136,%zmm15,%zmm13,%zmm6
	vshuff64x2 $221,%zmm15,%zmm13,%zmm7

	vshuff64x2 $136,%zmm2,%zmm0,%zmm8
	vshuff64x2 $221,%zmm2,%zmm0,%zmm9
	vshuff64x2 $136,%zmm6,%zmm4,%zmm10
	vshuff64x2 $221,%zmm6,%zmm4,%zmm11
	vshuff64x2 $136,%zmm3,%zmm1,%zmm12
	vshuff64x2 $221,%zmm3,%zmm1,%zmm13
	vshuff64x2 $136,%zmm7,%zmm5,%zmm14
	vshuff64x2 $221,%zmm7,%zmm5,%zmm15
	
	vmovapd %zmm8,    (%rdx)
	vmovapd %zmm10,  64(%rdx)
	vmovapd %zmm12, 128(%rdx)
	vmovapd %zmm14, 192(%rdx)
	vmovapd %zmm9,  256(%rdx)
	vmovapd %zmm11, 320(%rdx)
	vmovapd %zmm13, 384(%rdx)
	vmovapd %zmm15, 448(%rdx)
	
	addq $512,%rdx
	addq $64,%rsi	
	cmpq %rsi,%rcx	
	jne L0

	popq %r13	
	popq %r12
       
	ret
