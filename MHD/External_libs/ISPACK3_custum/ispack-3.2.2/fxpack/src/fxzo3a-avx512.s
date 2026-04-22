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
.globl fxzo3a_
.globl _fxzo3a_	
fxzo3a_:
_fxzo3a_:
	movq (%rdi), %rdi  # K が rdi に
	movq (%rsi), %rsi  # L が rsi に
	movq %rdx, %r8  # 
        # X の先頭アドレスは r8 にコピー
        # T の先頭アドレスは rcx

        movq  %rsi, %r11
        movq  $0x5555555555555556, %rax
        imulq %rsi # この段階で, L/3 が %rdx に入る
	movq %rdx, %rsi  # L/3 が rdi に
	shlq $7,%rsi # L/3*2*M*8*2=L/3*128
	shlq $7,%r11 # L/3*2*M*8*3*2=L*128

	movq %r8,%rdx # rdx に Xの先頭アドレスを復帰
	addq %rsi,%r8 # X(1,1,0,1) のスタートアドレス
	movq %r8,%r9
	addq %rsi,%r9 # X(1,1,0,2) のスタートアドレス

	vbroadcastsd C1(%rip),%zmm13
	vbroadcastsd C2(%rip),%zmm14
	vbroadcastsd C3(%rip),%zmm15	

L0:	movq $0,%rax
	
	vbroadcastsd   (%rcx), %zmm8  # T1R
	vbroadcastsd  8(%rcx), %zmm9  # T1I
	vbroadcastsd 16(%rcx), %zmm10 # T2R
	vbroadcastsd 24(%rcx), %zmm11 # T2I

L1:	vmovapd   (%r8, %rax), %zmm2 # X(1,1)
	vmovapd 64(%r8, %rax), %zmm3 # X(2,1)
	vmovapd   (%r9, %rax), %zmm4 # X(1,2)
	vmovapd 64(%r9, %rax), %zmm5 # X(2,2)

	vmulpd %zmm8,%zmm2,%zmm0 # T1R*X(1,1)		
	vmulpd %zmm8,%zmm3,%zmm1 # T1R*X(2,1)
	
##	vmulpd %zmm9,%zmm2,%zmm2 # T1I*X(1,1)
##	vaddpd %zmm2,%zmm1,%zmm2 # X0I
	vfmadd213pd %zmm1,%zmm9,%zmm2
##
##	vmulpd %zmm9,%zmm3,%zmm3 # T1I*X(2,1)	
##	vsubpd %zmm3,%zmm0,%zmm3 # X0R	
	vfnmadd213pd %zmm0,%zmm9,%zmm3
##
##	vmulpd %zmm10,%zmm4,%zmm0 # T2R*X(1,2)
##	vsubpd %zmm0,%zmm3,%zmm0 # X1R'
	vmovapd %zmm4,%zmm0
	vfnmadd213pd %zmm3,%zmm10,%zmm0
##	
##	vmulpd %zmm10,%zmm5,%zmm1 # T2R*X(2,2)
##	vsubpd %zmm1,%zmm2,%zmm1 # X1I'
	vmovapd %zmm5,%zmm1
	vfnmadd213pd %zmm2,%zmm10,%zmm1
##
##	vmulpd %zmm11,%zmm4,%zmm4 # T2I*X(1,2)
##	vsubpd %zmm4,%zmm1,%zmm4 # X1I
	vfnmadd213pd %zmm1,%zmm11,%zmm4
##	
##	vmulpd %zmm11,%zmm5,%zmm5 # T2I*X(2,2)
##	vaddpd %zmm5,%zmm0,%zmm5 # X1R
	vfmadd213pd %zmm0,%zmm11,%zmm5
##
	vmovapd   (%rdx,%rax), %zmm0 # X(1,0)
	vmovapd 64(%rdx,%rax), %zmm1 # X(2,0)
	
##	vmulpd %zmm15,%zmm3,%zmm3 # 2*X0R
##	vsubpd %zmm5,%zmm3,%zmm3 # X0R
	vfmsub213pd %zmm5,%zmm15,%zmm3	
##	
##	vmulpd %zmm15,%zmm2,%zmm2 # 2*X0I	
##	vsubpd %zmm4,%zmm2,%zmm2 # X0I
	vfmsub213pd %zmm4,%zmm15,%zmm2
##
	vaddpd %zmm0,%zmm3,%zmm6 # X0R+X(IV,1,I,0)
	vaddpd %zmm1,%zmm2,%zmm7 # X0I+X(IV,2,I,0)
	
##	vmulpd %zmm13,%zmm3,%zmm3 # X0R*C1
##	vaddpd %zmm0,%zmm3,%zmm0 # X2R
	vfmadd231pd %zmm3,%zmm13,%zmm0
##	
##	vmulpd %zmm13,%zmm2,%zmm2 # X0I*C1
##	vaddpd %zmm1,%zmm2,%zmm1 # X2I
	vfmadd231pd %zmm2,%zmm13,%zmm1
##	
##	vmulpd %zmm14,%zmm5,%zmm5 # X1R*C2
##	vsubpd %zmm5,%zmm1,%zmm5 # X2I-C2*X1R
	vfnmadd213pd %zmm1,%zmm14,%zmm5
##	
##	vmulpd %zmm14,%zmm4,%zmm4 # X1I*C2
##	vaddpd %zmm4,%zmm0,%zmm4 # X2R+C2*X1I
	vfmadd213pd %zmm0,%zmm14,%zmm4
##	
##	vmulpd %zmm15,%zmm0,%zmm0 # 2*X2R
##	vsubpd %zmm4,%zmm0,%zmm0 # 2*X2R-(X2R+C2*X1I)
	vfmsub213pd %zmm4,%zmm15,%zmm0
##	
##	vmulpd %zmm15,%zmm1,%zmm1 # 2*X2I
##	vsubpd %zmm5,%zmm1,%zmm1 # 2*X2I-(X2I-C2*X1R)
	vfmsub213pd %zmm5,%zmm15,%zmm1
##
	vmovapd %zmm6,  (%rdx,%rax)
	vmovapd %zmm7,64(%rdx,%rax)
	vmovapd %zmm0,  (%r8,%rax)
	vmovapd %zmm1,64(%r8,%rax)
	vmovapd %zmm4,  (%r9,%rax)
	vmovapd %zmm5,64(%r9,%rax)

	addq $128,%rax	
	cmpq %rsi,%rax
	jne L1

	addq %r11,%rdx
	addq %r11,%r8	
	addq %r11,%r9
	addq $32,%rcx

	subq $1,%rdi
	jnz L0
	
	ret

C1:	# -0.5D0
        .long   0x00000000,0xbfe00000
C2:	# 0.86602540378443864676D0
	.long   0xe8584caa,0x3febb67a
C3:	 # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000
	
	
