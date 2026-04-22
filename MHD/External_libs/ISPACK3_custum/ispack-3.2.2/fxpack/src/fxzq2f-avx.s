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
.globl fxzq2f_
.globl _fxzq2f_	
fxzq2f_:
_fxzq2f_:

	movq (%rdi), %rdi  # L が rdi に
	# X の先頭アドレスは rsi

	vbroadcastsd CM2(%rip),%ymm9
	# 倍精度不動小数点の -2 を ymm9 の4箇所に	

	#------------------------

	shlq $5,%rdi # L/2*2*M*8=L*32
	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) のスタートアドレス
	movq $0,%rax

L1:	vmovapd   (%rsi,%rax), %ymm0 # X(1,0)
	vmovapd 32(%rsi,%rax), %ymm1 # X(2,0)
	vmovapd   (%rdx,%rax), %ymm2 # X(1,1)
	vmovapd 32(%rdx,%rax), %ymm3 # X(2,1)

	vaddpd %ymm0,%ymm2,%ymm4 # X(1,0) + X(1,1)
	vsubpd %ymm2,%ymm0,%ymm6 # X(1,0) - X(1,1)
	vsubpd %ymm1,%ymm3,%ymm7 # X(2,1) - X(2,0)
	vmulpd %ymm9,%ymm1,%ymm10 # -2*X(2,0)
	vsubpd %ymm7,%ymm10,%ymm5 # -2*X(2,0) - X(2,1)'
	
	vmovapd %ymm4,  (%rsi,%rax)
	vmovapd %ymm5,32(%rsi,%rax)
	vmovapd %ymm6,  (%rdx,%rax)
	vmovapd %ymm7,32(%rdx,%rax)

	addq $64,%rax	
	cmpq %rdi,%rax
	jne L1

	ret
CM2: # 倍精度不動小数点の -2
	.long   0x00000000,0xc0000000
