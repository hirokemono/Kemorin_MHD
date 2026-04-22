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
.globl fxzo2a_
.globl _fxzo2a_	
fxzo2a_:
_fxzo2a_:
	vbroadcastsd C2(%rip),%zmm15 # 倍精度不動小数点の 2 を zmm15 に
	movq (%rdi), %rdi  # K が rdi に
	movq (%rsi), %rsi  # L が rsi に
        # X の先頭アドレスは rdx
        # T の先頭アドレスは rcx
	# r8 空き

	shlq $6,%rsi # L/2*2*M*8*2=L*64

	movq %rdx,%r8
	addq %rsi,%r8 # X(1,1,0,1) のスタートアドレス

L0:	movq $0,%rax
	vbroadcastsd  (%rcx), %zmm8 # T(1,J)
	vbroadcastsd 8(%rcx), %zmm9 # T(2,J)

L1:	vmovapd   (%rdx,%rax), %zmm0 # X(1,0)
	vmovapd 64(%rdx,%rax), %zmm1 # X(2,0)
	vmovapd   (%r8,%rax), %zmm2 # X(1,1)
	vmovapd 64(%r8,%rax), %zmm3 # X(2,1)

##	vmulpd %zmm8,%zmm2,%zmm10
##	vsubpd %zmm10,%zmm0,%zmm10 # X1R
	vmovapd %zmm2,%zmm10
	vfnmadd213pd %zmm0,%zmm8,%zmm10
##	
##	vmulpd %zmm9,%zmm2,%zmm2
##	vsubpd %zmm2,%zmm1,%zmm2 # X1I
	vfnmadd213pd %zmm1,%zmm9,%zmm2
##
##	vmulpd %zmm9,%zmm3,%zmm11
##	vaddpd %zmm11,%zmm10,%zmm10 # X(1,1)'
	vfmadd231pd %zmm9,%zmm3,%zmm10
##	
##	vmulpd %zmm8,%zmm3,%zmm3	
##	vsubpd %zmm3,%zmm2,%zmm2 # X(2,1)'
	vfnmadd231pd %zmm8,%zmm3,%zmm2
##
##	vmulpd %zmm15,%zmm0,%zmm0 # 2*X(1,0)
##	vsubpd %zmm10,%zmm0,%zmm0 # X(1,0)'
	vfmsub213pd %zmm10,%zmm15,%zmm0
##	
##	vmulpd %zmm15,%zmm1,%zmm1 # 2*X(2,0)			
##	vsubpd %zmm2,%zmm1,%zmm1 # X(2,0)'	
	vfmsub213pd %zmm2,%zmm15,%zmm1
##	
	vmovapd %zmm0,  (%rdx,%rax)
	vmovapd %zmm1,64(%rdx,%rax)
	vmovapd %zmm10,  (%r8,%rax)
	vmovapd %zmm2,64(%r8,%rax)

	addq $128,%rax	
	cmpq %rsi,%rax
	jne L1

	addq %rsi,%rdx
	addq %rsi,%rdx	
	addq %rsi,%r8
	addq %rsi,%r8	
	addq $16,%rcx

	subq $1,%rdi
	jnz L0
	
	ret
C2: # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000	
