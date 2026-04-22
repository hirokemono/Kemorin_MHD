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
.globl fxzq3a_
.globl _fxzq3a_	
fxzq3a_:
_fxzq3a_:
	movq (%rdi), %rdi  # K が rdi に
	movq (%rsi), %rsi  # L が rsi に
	movq %rdx, %r8  # 
        # X の先頭アドレスは r8 にコピー
        # T の先頭アドレスは rcx

        movq  %rsi, %r11
        movq  $0x5555555555555556, %rax
        imulq %rsi # この段階で, L/3 が %rdx に入る
	movq %rdx, %rsi  # L/3 が rdi に
	shlq $6,%rsi # L/3*2*M*8=L/3*64
	shlq $6,%r11 # L/3*2*M*8*3=L*64

	movq %r8,%rdx # rdx に Xの先頭アドレスを復帰
	addq %rsi,%r8 # X(1,1,0,1) のスタートアドレス
	movq %r8,%r9
	addq %rsi,%r9 # X(1,1,0,2) のスタートアドレス

	vbroadcastsd C1(%rip),%ymm13
	vbroadcastsd C2(%rip),%ymm14
	vbroadcastsd C3(%rip),%ymm15	

L0:	movq $0,%rax
	
	vbroadcastsd   (%rcx), %ymm8  # T1R
	vbroadcastsd  8(%rcx), %ymm9  # T1I
	vbroadcastsd 16(%rcx), %ymm10 # T2R
	vbroadcastsd 24(%rcx), %ymm11 # T2I

L1:	vmovapd   (%r8, %rax), %ymm2 # X(1,1)
	vmovapd 32(%r8, %rax), %ymm3 # X(2,1)
	vmovapd   (%r9, %rax), %ymm4 # X(1,2)
	vmovapd 32(%r9, %rax), %ymm5 # X(2,2)

	vmulpd %ymm8,%ymm2,%ymm0 # T1R*X(1,1)		
	vmulpd %ymm8,%ymm3,%ymm1 # T1R*X(2,1)
	
##	vmulpd %ymm9,%ymm2,%ymm2 # T1I*X(1,1)
##	vaddpd %ymm2,%ymm1,%ymm2 # X0I
	vfmadd213pd %ymm1,%ymm9,%ymm2
##
##	vmulpd %ymm9,%ymm3,%ymm3 # T1I*X(2,1)	
##	vsubpd %ymm3,%ymm0,%ymm3 # X0R	
	vfnmadd213pd %ymm0,%ymm9,%ymm3
##
##	vmulpd %ymm10,%ymm4,%ymm0 # T2R*X(1,2)
##	vsubpd %ymm0,%ymm3,%ymm0 # X1R'
	vmovapd %ymm4,%ymm0
	vfnmadd213pd %ymm3,%ymm10,%ymm0
##	
##	vmulpd %ymm10,%ymm5,%ymm1 # T2R*X(2,2)
##	vsubpd %ymm1,%ymm2,%ymm1 # X1I'
	vmovapd %ymm5,%ymm1
	vfnmadd213pd %ymm2,%ymm10,%ymm1
##
##	vmulpd %ymm11,%ymm4,%ymm4 # T2I*X(1,2)
##	vsubpd %ymm4,%ymm1,%ymm4 # X1I
	vfnmadd213pd %ymm1,%ymm11,%ymm4
##	
##	vmulpd %ymm11,%ymm5,%ymm5 # T2I*X(2,2)
##	vaddpd %ymm5,%ymm0,%ymm5 # X1R
	vfmadd213pd %ymm0,%ymm11,%ymm5
##
	vmovapd   (%rdx,%rax), %ymm0 # X(1,0)
	vmovapd 32(%rdx,%rax), %ymm1 # X(2,0)
	
##	vmulpd %ymm15,%ymm3,%ymm3 # 2*X0R
##	vsubpd %ymm5,%ymm3,%ymm3 # X0R
	vfmsub213pd %ymm5,%ymm15,%ymm3	
##	
##	vmulpd %ymm15,%ymm2,%ymm2 # 2*X0I	
##	vsubpd %ymm4,%ymm2,%ymm2 # X0I
	vfmsub213pd %ymm4,%ymm15,%ymm2
##
	vaddpd %ymm0,%ymm3,%ymm6 # X0R+X(IV,1,I,0)
	vaddpd %ymm1,%ymm2,%ymm7 # X0I+X(IV,2,I,0)
	
##	vmulpd %ymm13,%ymm3,%ymm3 # X0R*C1
##	vaddpd %ymm0,%ymm3,%ymm0 # X2R
	vfmadd231pd %ymm3,%ymm13,%ymm0
##	
##	vmulpd %ymm13,%ymm2,%ymm2 # X0I*C1
##	vaddpd %ymm1,%ymm2,%ymm1 # X2I
	vfmadd231pd %ymm2,%ymm13,%ymm1
##	
##	vmulpd %ymm14,%ymm5,%ymm5 # X1R*C2
##	vsubpd %ymm5,%ymm1,%ymm5 # X2I-C2*X1R
	vfnmadd213pd %ymm1,%ymm14,%ymm5
##	
##	vmulpd %ymm14,%ymm4,%ymm4 # X1I*C2
##	vaddpd %ymm4,%ymm0,%ymm4 # X2R+C2*X1I
	vfmadd213pd %ymm0,%ymm14,%ymm4
##	
##	vmulpd %ymm15,%ymm0,%ymm0 # 2*X2R
##	vsubpd %ymm4,%ymm0,%ymm0 # 2*X2R-(X2R+C2*X1I)
	vfmsub213pd %ymm4,%ymm15,%ymm0
##	
##	vmulpd %ymm15,%ymm1,%ymm1 # 2*X2I
##	vsubpd %ymm5,%ymm1,%ymm1 # 2*X2I-(X2I-C2*X1R)
	vfmsub213pd %ymm5,%ymm15,%ymm1
##
	vmovapd %ymm6,  (%rdx,%rax)
	vmovapd %ymm7,32(%rdx,%rax)
	vmovapd %ymm0,  (%r8,%rax)
	vmovapd %ymm1,32(%r8,%rax)
	vmovapd %ymm4,  (%r9,%rax)
	vmovapd %ymm5,32(%r9,%rax)

	addq $64,%rax	
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
	
	
