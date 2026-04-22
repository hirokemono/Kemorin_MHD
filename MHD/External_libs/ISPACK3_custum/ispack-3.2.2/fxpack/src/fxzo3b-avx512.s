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
.globl fxzo3b_
.globl _fxzo3b_	
fxzo3b_:
_fxzo3b_:
	movq (%rdi), %rdi  # L が rdi に
        movq  $0x5555555555555556, %rax
        imulq %rdi # この段階で, L/3 が %rdx に入る
	# X の先頭アドレスは rsi
	movq %rdx, %rdi  # L/3 が rdi に
	shlq $7,%rdi # L/3*2*M*8*2=L/3*128
	
	#------------------------

	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) のスタートアドレス
	movq %rdx,%r8
	addq %rdi,%r8 # X(1,1,0,2) のスタートアドレス
	movq $0,%rax

	vbroadcastsd C1(%rip),%zmm13
	vbroadcastsd C2(%rip),%zmm14
	vbroadcastsd C3(%rip),%zmm15	
	
	
L1:	vmovapd   (%rsi,%rax), %zmm0 # X(1,0)
	vmovapd 64(%rsi,%rax), %zmm1 # X(2,0)
	vmovapd   (%rdx,%rax), %zmm2 # X(1,1)
	vmovapd 64(%rdx,%rax), %zmm3 # X(2,1)
	vmovapd   (%r8, %rax), %zmm4 # X(1,2)
	vmovapd 64(%r8, %rax), %zmm5 # X(2,2)

	vsubpd %zmm4,%zmm2,%zmm6 # X1R
	vsubpd %zmm5,%zmm3,%zmm7 # X1I
	vaddpd %zmm4,%zmm2,%zmm2 # X0R
	vaddpd %zmm5,%zmm3,%zmm3 # X0I

	vaddpd %zmm0,%zmm2,%zmm4 # X0R+X(IV,1,I,0)
	vaddpd %zmm1,%zmm3,%zmm5 # X0I+X(IV,2,I,0)
##	vmulpd %zmm13,%zmm2,%zmm2 # X0R*C1
##	vmulpd %zmm13,%zmm3,%zmm3 # X0I*C1
##	vaddpd %zmm0,%zmm2,%zmm0 # X2R
##	vaddpd %zmm1,%zmm3,%zmm1 # X2I
	vfmadd231pd %zmm13,%zmm2,%zmm0
	vfmadd231pd %zmm13,%zmm3,%zmm1
##	
##	vmulpd %zmm14,%zmm6,%zmm6 # X1R*C2
##	vmulpd %zmm14,%zmm7,%zmm7 # X1I*C2
##	vaddpd %zmm7,%zmm0,%zmm7 # X2R+C2*X1I
##	vsubpd %zmm6,%zmm1,%zmm6 # X2I-C2*X1R
	vfnmadd213pd %zmm1,%zmm14,%zmm6
	vfmadd213pd %zmm0,%zmm14,%zmm7
##
##	vmulpd %zmm15,%zmm0,%zmm0 # 2*X2R
##	vmulpd %zmm15,%zmm1,%zmm1 # 2*X2I
##	vsubpd %zmm7,%zmm0,%zmm0 # 2*X2R-(X2R+C2*X1I)
##	vsubpd %zmm6,%zmm1,%zmm1 # 2*X2I-(X2I-C2*X1R)
	vfmsub213pd %zmm7,%zmm15,%zmm0
	vfmsub213pd %zmm6,%zmm15,%zmm1	
##	
	
	vmovapd %zmm4,  (%rsi,%rax)
	vmovapd %zmm5,64(%rsi,%rax)
	vmovapd %zmm7,  (%r8,%rax)
	vmovapd %zmm6,64(%r8,%rax)
	vmovapd %zmm0,  (%rdx,%rax)
	vmovapd %zmm1,64(%rdx,%rax)

	addq $128,%rax	
	cmpq %rdi,%rax
	jne L1

	ret
       
C1:	# -0.5D0
        .long   0x00000000,0xbfe00000
C2:	# 0.86602540378443864676D0
	.long   0xe8584caa,0x3febb67a
C3:	 # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000
	
