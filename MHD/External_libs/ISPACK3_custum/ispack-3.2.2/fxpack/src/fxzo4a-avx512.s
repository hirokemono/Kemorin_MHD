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
.globl fxzo4a_
.globl _fxzo4a_	
fxzo4a_:
_fxzo4a_:
	vbroadcastsd C2(%rip),%zmm15 # 倍精度不動小数点の 2 を zmm15 に
	movq (%rdi), %rdi  # K が rdi に
	movq (%rsi), %rsi  # L が rsi に	
        # X の先頭アドレスは rdx
        # T の先頭アドレスは rcx
	# r8 空き

	shlq $5,%rsi # L/4*2*M*8*2=L*32
	movq %rsi,%r11
	shlq $2,%r11 # L/4*2*M*8*4*2=L*128
	
	movq %rdx,%r8
	addq %rsi,%r8 # X(1,1,0,1) のスタートアドレス
	movq %r8,%r9
	addq %rsi,%r9 # X(1,1,0,2) のスタートアドレス
	movq %r9,%r10
	addq %rsi,%r10 # X(1,1,0,3) のスタートアドレス

L0:	movq $0,%rax
	vbroadcastsd   (%rcx), %zmm8  # T1R
	vbroadcastsd  8(%rcx), %zmm9  # T1I
	vbroadcastsd 16(%rcx), %zmm10 # T2R
	vbroadcastsd 24(%rcx), %zmm11 # T2I

L1:	vmovapd   (%rdx,%rax), %zmm0 # X(1,0)
	vmovapd 64(%rdx,%rax), %zmm1 # X(2,0)
	vmovapd   (%r8, %rax), %zmm2 # X(1,1)
	vmovapd 64(%r8, %rax), %zmm3 # X(2,1)
	vmovapd   (%r9, %rax), %zmm4 # X(1,2)
	vmovapd 64(%r9, %rax), %zmm5 # X(2,2)
	vmovapd   (%r10,%rax), %zmm6 # X(1,3)
	vmovapd 64(%r10,%rax), %zmm7 # X(2,3)

##	vmulpd %zmm10,%zmm4,%zmm12
##	vsubpd %zmm12,%zmm0,%zmm12
	vmovapd %zmm4,%zmm12
	vfnmadd213pd %zmm0,%zmm10,%zmm12
##
##	vmulpd %zmm11,%zmm4,%zmm4	
##	vsubpd %zmm4,%zmm1,%zmm4
	vfnmadd213pd %zmm1,%zmm11,%zmm4
##	
##	vmulpd %zmm11,%zmm5,%zmm13
##	vaddpd %zmm13,%zmm12,%zmm12 # X2R	
	vfmadd231pd %zmm11,%zmm5,%zmm12
##
##	vmulpd %zmm10,%zmm5,%zmm5
##	vsubpd %zmm5,%zmm4,%zmm4 # X2I (zmm5 空き)
	vfnmadd231pd %zmm10,%zmm5,%zmm4
##
##	vmulpd %zmm15,%zmm0,%zmm0 # 2*X(1,0)
##	vsubpd %zmm12,%zmm0,%zmm0 # X0R
	vfmsub213pd %zmm12,%zmm15,%zmm0
##	
##	vmulpd %zmm15,%zmm1,%zmm1 # 2*X(2,0)		
##	vsubpd %zmm4,%zmm1,%zmm1  # X0I
	vfmsub213pd %zmm4,%zmm15,%zmm1
##
##	vmulpd %zmm10,%zmm6,%zmm13
##	vsubpd %zmm13,%zmm2,%zmm13
	vmovapd %zmm6,%zmm13
	vfnmadd213pd %zmm2,%zmm10,%zmm13
##	
##	vmulpd %zmm11,%zmm6,%zmm6
##	vsubpd %zmm6,%zmm3,%zmm6
	vfnmadd213pd %zmm3,%zmm11,%zmm6
##	
##	vmulpd %zmm11,%zmm7,%zmm5
##	vaddpd %zmm5,%zmm13,%zmm5 # X3R	
	vmovapd %zmm7,%zmm5
	vfmadd213pd %zmm13,%zmm11,%zmm5
##
##	vmulpd %zmm10,%zmm7,%zmm7
##	vsubpd %zmm7,%zmm6,%zmm6 # X3I 
	vfnmadd231pd %zmm10,%zmm7,%zmm6
##	
##	vmulpd %zmm15,%zmm2,%zmm2 # 2*X(1,1)
##	vsubpd %zmm5,%zmm2,%zmm2 # X1R	
	vfmsub213pd %zmm5,%zmm15,%zmm2
##
##	vmulpd %zmm15,%zmm3,%zmm3 # 2*X(2,1)		
##	vsubpd %zmm6,%zmm3,%zmm3 # X1I (zmm7, 13 空き)
	vfmsub213pd %zmm6,%zmm15,%zmm3
##	
	#---
##	vmulpd %zmm8,%zmm2,%zmm13
##	vsubpd %zmm13,%zmm0,%zmm13
	vmovapd %zmm2,%zmm13
	vfnmadd213pd %zmm0,%zmm8,%zmm13
##	
##	vmulpd %zmm9,%zmm2,%zmm2	
##	vsubpd %zmm2,%zmm1,%zmm2
	vfnmadd213pd %zmm1,%zmm9,%zmm2
##	
##	vmulpd %zmm9,%zmm3,%zmm7
##	vaddpd %zmm7,%zmm13,%zmm13 # X(1,2)'
	vfmadd231pd %zmm3,%zmm9,%zmm13
##	
##	vmulpd %zmm8,%zmm3,%zmm3 
##	vsubpd %zmm3,%zmm2,%zmm2 # X(2,2)' (zmm3 空き)
	vfnmadd231pd %zmm3,%zmm8,%zmm2
##	
##	vmulpd %zmm15,%zmm0,%zmm0 # 2*X0R
##	vsubpd %zmm13,%zmm0,%zmm0 # X(1,0)'	
	vfmsub213pd %zmm13,%zmm15,%zmm0
##	
##	vmulpd %zmm15,%zmm1,%zmm1 # 2*X0I
##	vsubpd %zmm2,%zmm1,%zmm1  # X(2,0)'
	vfmsub213pd %zmm2,%zmm15,%zmm1
##	
##	vmulpd %zmm8,%zmm6,%zmm3
##	vsubpd %zmm3,%zmm12,%zmm3
	vmovapd %zmm6,%zmm3
	vfnmadd213pd %zmm12,%zmm8,%zmm3
##	
##	vmulpd %zmm9,%zmm6,%zmm6
##	vsubpd %zmm6,%zmm4,%zmm6 
	vfnmadd213pd %zmm4,%zmm9,%zmm6
##	
##	vmulpd %zmm9,%zmm5,%zmm7
##	vsubpd %zmm7,%zmm3,%zmm3 # X(1,1)'
	vfnmadd231pd %zmm9,%zmm5,%zmm3
##
##	vmulpd %zmm8,%zmm5,%zmm5
##	vaddpd %zmm5,%zmm6,%zmm6 # X(2,1)' (zmm5 空き)
	vfmadd231pd %zmm8,%zmm5,%zmm6
##
##	vmulpd %zmm15,%zmm12,%zmm12 # 2*X2R
##	vsubpd %zmm3,%zmm12,%zmm12 # X(1,3)'	
	vfmsub213pd %zmm3,%zmm15,%zmm12
##
##	vmulpd %zmm15,%zmm4,%zmm4 # 2*X0I
##	vsubpd %zmm6,%zmm4,%zmm4  # X(2,3)'
	vfmsub213pd %zmm6,%zmm15,%zmm4
##	
	vmovapd %zmm0,  (%rdx,%rax)
	vmovapd %zmm1,64(%rdx,%rax)
	vmovapd %zmm3,  (%r8,%rax)
	vmovapd %zmm6,64(%r8,%rax)
	vmovapd %zmm13,  (%r9,%rax)
	vmovapd %zmm2,64(%r9,%rax)
	vmovapd %zmm12,  (%r10,%rax)
	vmovapd %zmm4,64(%r10,%rax)

	addq $128,%rax	
	cmpq %rsi,%rax
	jne L1

	addq %r11,%rdx
	addq %r11,%r8	
	addq %r11,%r9
	addq %r11,%r10
	addq $48,%rcx

	subq $1,%rdi
	jnz L0
	
	ret
C2: # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000	
