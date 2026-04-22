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
.globl fxzq3f_
.globl _fxzq3f_	
fxzq3f_:
_fxzq3f_:
	movq (%rdi), %rdi  # L が rdi に
        movq  $0x5555555555555556, %rax
        imulq %rdi # この段階で, L/3 が %rdx に入る
	# X の先頭アドレスは rsi
	movq %rdx, %rdi  # L/3 が rdi に
	shlq $6,%rdi # L/3*2*M*8=L/3*64
	
	#------------------------

	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) のスタートアドレス
	movq %rdx,%r8
	addq %rdi,%r8 # X(1,1,0,2) のスタートアドレス
	movq $0,%rax

	vbroadcastsd C1(%rip),%ymm13
	vbroadcastsd C2(%rip),%ymm14
	vbroadcastsd C3(%rip),%ymm15
	vbroadcastsd CP(%rip),%ymm12	
	
L1:	vmovapd   (%rsi,%rax), %ymm0 # X(1,0)
	vmovapd 32(%rsi,%rax), %ymm1 # X(2,0)
	vmovapd   (%rdx,%rax), %ymm2 # X(1,1)
	vmovapd 32(%rdx,%rax), %ymm3 # X(2,1)
	vmovapd   (%r8, %rax), %ymm4 # X(1,2)
	vmovapd 32(%r8, %rax), %ymm5 # X(2,2)

	vsubpd %ymm4,%ymm2,%ymm6 # X1R
	vsubpd %ymm3,%ymm5,%ymm7 # X1I
	vaddpd %ymm4,%ymm2,%ymm2 # X0R
##	vaddpd %ymm5,%ymm3,%ymm3 # -X0I
##	vmulpd %ymm12,%ymm3,%ymm3 # X0I
	vfmsub231pd %ymm12,%ymm5,%ymm3
##	
	vaddpd %ymm0,%ymm2,%ymm4 # X0R+X(IV,1,I,0)
	vsubpd %ymm1,%ymm3,%ymm5 # X0I-X(IV,2,I,0)
##	vmulpd %ymm13,%ymm2,%ymm2 # X0R*C1
##	vmulpd %ymm13,%ymm3,%ymm3 # X0I*C1
##	vaddpd %ymm0,%ymm2,%ymm0 # X2R
##	vsubpd %ymm1,%ymm3,%ymm1 # X2I
	vfmadd231pd %ymm13,%ymm2,%ymm0
	vfmsub231pd %ymm13,%ymm3,%ymm1	
##	
##	vmulpd %ymm14,%ymm6,%ymm6 # X1R*C2
##	vmulpd %ymm14,%ymm7,%ymm7 # X1I*C2
##	vaddpd %ymm7,%ymm0,%ymm7 # X2R+C2*X1I
##	vsubpd %ymm6,%ymm1,%ymm6 # X2I-C2*X1R
	vfnmadd213pd %ymm1,%ymm14,%ymm6
	vfmadd213pd %ymm0,%ymm14,%ymm7
##	
##	vmulpd %ymm15,%ymm0,%ymm0 # 2*X2R
##	vmulpd %ymm15,%ymm1,%ymm1 # 2*X2I
##	vsubpd %ymm7,%ymm0,%ymm0 # 2*X2R-(X2R+C2*X1I)
##	vsubpd %ymm6,%ymm1,%ymm1 # 2*X2I-(X2I-C2*X1R)
	vfmsub213pd %ymm7,%ymm15,%ymm0
	vfmsub213pd %ymm6,%ymm15,%ymm1
##	
	vmovapd %ymm4,  (%rsi,%rax)
	vmovapd %ymm5,32(%rsi,%rax)
	vmovapd %ymm7,  (%r8,%rax)
	vmovapd %ymm6,32(%r8,%rax)
	vmovapd %ymm0,  (%rdx,%rax)
	vmovapd %ymm1,32(%rdx,%rax)

	addq $64,%rax	
	cmpq %rdi,%rax
	jne L1

	ret
       
C1:	# -0.5D0
        .long   0x00000000,0xbfe00000
C2:	# 0.86602540378443864676D0
	.long   0xe8584caa,0x3febb67a
C3:	 # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000
CP:	# 倍精度不動小数点の -1
        .long   0x00000000,0xbff00000
