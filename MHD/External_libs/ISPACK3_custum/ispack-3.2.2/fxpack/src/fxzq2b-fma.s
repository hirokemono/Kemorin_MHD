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
.globl fxzq2b_
.globl _fxzq2b_	
fxzq2b_:
_fxzq2b_:

	movq (%rdi), %rdi  # L が rdi に
	# X の先頭アドレスは rsi

	#------------------------

	shlq $5,%rdi # L/2*2*M*8=L*32
	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) のスタートアドレス
	movq $0,%rax

L1:	vmovapd   (%rsi,%rax), %ymm0 # X(1,0)
	vmovapd 32(%rsi,%rax), %ymm1 # X(2,0)
	vmovapd   (%rdx,%rax), %ymm2 # X(1,1)
	vmovapd 32(%rdx,%rax), %ymm3 # X(2,1)

	vaddpd %ymm0,%ymm2,%ymm4
	vaddpd %ymm1,%ymm3,%ymm5
	vsubpd %ymm2,%ymm0,%ymm6
	vsubpd %ymm3,%ymm1,%ymm7
	
	vmovapd %ymm4,  (%rsi,%rax)
	vmovapd %ymm5,32(%rsi,%rax)
	vmovapd %ymm6,  (%rdx,%rax)
	vmovapd %ymm7,32(%rdx,%rax)

	addq $64,%rax	
	cmpq %rdi,%rax
	jne L1

	ret
       
	
