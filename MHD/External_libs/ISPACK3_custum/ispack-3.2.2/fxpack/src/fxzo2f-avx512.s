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
.globl fxzo2f_
.globl _fxzo2f_	
fxzo2f_:
_fxzo2f_:

	movq (%rdi), %rdi  # L が rdi に
	# X の先頭アドレスは rsi

	vbroadcastsd CM2(%rip),%zmm9
	# 倍精度不動小数点の -2 を ymm9 の4箇所に	

	#------------------------

	shlq $6,%rdi # L/2*2*M*8*2=L*64
	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) のスタートアドレス
	movq $0,%rax

L1:	vmovapd   (%rsi,%rax), %zmm0 # X(1,0)
	vmovapd 64(%rsi,%rax), %zmm1 # X(2,0)
	vmovapd   (%rdx,%rax), %zmm2 # X(1,1)
	vmovapd 64(%rdx,%rax), %zmm3 # X(2,1)

	vaddpd %zmm0,%zmm2,%zmm4 # X(1,0) + X(1,1)
	vsubpd %zmm2,%zmm0,%zmm6 # X(1,0) - X(1,1)
	vsubpd %zmm1,%zmm3,%zmm7 # X(2,1) - X(2,0)
##	vmulpd %zmm9,%zmm1,%zmm10 # -2*X(2,0)
##	vsubpd %zmm7,%zmm10,%zmm5 # -2*X(2,0) - X(2,1)'
	vfmsub213pd %zmm7,%zmm9,%zmm1 # -2*X(2,0) - X(2,1)'
##
	
	vmovapd %zmm4,  (%rsi,%rax)
##	vmovapd %zmm5,64(%rsi,%rax)
	vmovapd %zmm1,64(%rsi,%rax)
##	
	vmovapd %zmm6,  (%rdx,%rax)
	vmovapd %zmm7,64(%rdx,%rax)

	addq $128,%rax	
	cmpq %rdi,%rax
	jne L1

	ret
CM2: # 倍精度不動小数点の -2
	.long   0x00000000,0xc0000000
