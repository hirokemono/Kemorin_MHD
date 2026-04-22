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
.globl fxzo5b_
.globl _fxzo5b_	
fxzo5b_:
_fxzo5b_:
	movq (%rdi), %rdi  # L が rdi に
        movq $0x6666666666666667, %rax
        imulq %rdi 
        sarq $1, %rdx # この段階で, L/5 が %rdx に入る
	# X の先頭アドレスは rsi
	movq %rdx, %rdi  # L/5 が rdi に
	shlq $7,%rdi # L/5*2*M*8*2=L/5*128
	
	#------------------------

	movq %rsi,%rdx
	addq %rdi,%rdx # X(1,1,0,1) のスタートアドレス
	movq %rdx,%r8
	addq %rdi,%r8 # X(1,1,0,2) のスタートアドレス
	movq %r8,%r9
	addq %rdi,%r9 # X(1,1,0,3) のスタートアドレス
	movq %r9,%r10
	addq %rdi,%r10 # X(1,1,0,4) のスタートアドレス
	movq $0,%rax

	vbroadcastsd C0(%rip),%zmm11
	vbroadcastsd C1(%rip),%zmm12
	vbroadcastsd C2(%rip),%zmm13
	vbroadcastsd C3(%rip),%zmm14
	vbroadcastsd C4(%rip),%zmm15
	
	
L1:	vmovapd   (%rsi,%rax), %zmm0 # X(1,0)
	vmovapd 64(%rsi,%rax), %zmm1 # X(2,0)
	vmovapd   (%rdx,%rax), %zmm2 # X(1,1)
	vmovapd 64(%rdx,%rax), %zmm3 # X(2,1)
	vmovapd   (%r8, %rax), %zmm4 # X(1,2)
	vmovapd 64(%r8, %rax), %zmm5 # X(2,2)
	vmovapd   (%r9, %rax), %zmm6 # X(1,3)
	vmovapd 64(%r9, %rax), %zmm7 # X(2,3)
	vmovapd   (%r10,%rax), %zmm8 # X(1,4)
	vmovapd 64(%r10,%rax), %zmm9 # X(2,4)

	vsubpd %zmm8,%zmm2,%zmm10 # X0R
	vaddpd %zmm8,%zmm2,%zmm2 # X1R	
	vsubpd %zmm9,%zmm3,%zmm8 # X0I
	vaddpd %zmm9,%zmm3,%zmm3 # X1I # 9 空き

	vsubpd %zmm6,%zmm4,%zmm9 # X3R
	vaddpd %zmm6,%zmm4,%zmm4 # X4R # 	
	vsubpd %zmm7,%zmm5,%zmm6 # X3I
	vaddpd %zmm7,%zmm5,%zmm5 # X4I #  7空き

##	vmulpd %zmm14,%zmm9,%zmm7 # X3R*C3
##	vaddpd %zmm10,%zmm7,%zmm7 # X2R
	vmovapd %zmm9,%zmm7
        vfmadd213pd %zmm10,%zmm14,%zmm7
##	
##	vmulpd %zmm14,%zmm10,%zmm10 # X0R*C3
##	vsubpd %zmm9,%zmm10,%zmm9 # X3R # 10 空き
        vfmsub231pd %zmm10,%zmm14,%zmm9
##
##	vmulpd %zmm14,%zmm6,%zmm10 # X3I*C3
##	vaddpd %zmm10,%zmm8,%zmm10 # X2I
	vmovapd %zmm6,%zmm10
        vfmadd213pd %zmm8,%zmm14,%zmm10
##
##	vmulpd %zmm14,%zmm8,%zmm8 # X0I*C3
##	vsubpd %zmm6,%zmm8,%zmm6 # X3I # 8 空き
	vfmsub231pd %zmm14,%zmm8,%zmm6
##
	vaddpd %zmm4,%zmm2,%zmm8 # X0R
	vsubpd %zmm4,%zmm2,%zmm2 # X1R # 4 空き
	vaddpd %zmm5,%zmm3,%zmm4 # X0I
	vsubpd %zmm5,%zmm3,%zmm3 # X1I # 5空き
	
##	vmulpd %zmm12,%zmm8,%zmm5 # X0R*C1
##	vsubpd %zmm5,%zmm0,%zmm5 # X4R
	vmovapd %zmm8,%zmm5
        vfnmadd213pd %zmm0,%zmm12,%zmm5
##
	vaddpd %zmm8,%zmm0,%zmm0 # X(1,0) # 8 空き
##	vmulpd %zmm12,%zmm4,%zmm8 # X0I*C1
##	vsubpd %zmm8,%zmm1,%zmm8 # X4I
	vmovapd %zmm4,%zmm8
        vfnmadd213pd %zmm1,%zmm12,%zmm8
##
	vaddpd %zmm4,%zmm1,%zmm1 # X(2,0) # 4 空き

	#--

##	vmulpd %zmm13,%zmm2,%zmm2 # X1R*C2
##	vsubpd %zmm2,%zmm5,%zmm2 # X1R  # 4 空き
        vfnmadd213pd %zmm5,%zmm13,%zmm2
##	
##	vmulpd %zmm13,%zmm3,%zmm3 # X1I*C2
##	vsubpd %zmm3,%zmm8,%zmm3 # X1I  # 4 空き
        vfnmadd213pd %zmm8,%zmm13,%zmm3
##	
##	vmulpd %zmm11,%zmm5,%zmm5 # X4R*2
##	vsubpd %zmm2,%zmm5,%zmm5 # X4R  # 4 空き	
        vfmsub213pd %zmm2,%zmm11,%zmm5
## 
##	vmulpd %zmm11,%zmm8,%zmm8 # X4I*2
##	vsubpd %zmm3,%zmm8,%zmm8 # X4I  # 4 空き
        vfmsub213pd %zmm3,%zmm11,%zmm8
##
##	vmulpd %zmm15,%zmm6,%zmm6 # X3I*C4
##	vsubpd %zmm6,%zmm2,%zmm6 # X(1,3)  # 4 空き
        vfnmadd213pd %zmm2,%zmm15,%zmm6
##
##	vmulpd %zmm15,%zmm9,%zmm9 # X3R*C4
##	vaddpd %zmm9,%zmm3,%zmm9 # X(2,3)  # 4 空き
        vfmadd213pd %zmm3,%zmm15,%zmm9
##	
##	vmulpd %zmm11,%zmm2,%zmm2 # X1R*2
##	vsubpd %zmm6,%zmm2,%zmm2 # X(1,2)
        vfmsub213pd %zmm6,%zmm11,%zmm2
## 	
##	vmulpd %zmm11,%zmm3,%zmm3 # X1I*2
##	vsubpd %zmm9,%zmm3,%zmm3 # X(2,2)
        vfmsub213pd %zmm9,%zmm11,%zmm3
##	
##	vmulpd %zmm15,%zmm10,%zmm10 # X2I*C4
##	vsubpd %zmm10,%zmm5,%zmm10 # X(1,4)  # 4 空き
        vfnmadd213pd %zmm5,%zmm15,%zmm10
## 
##	vmulpd %zmm15,%zmm7,%zmm7 # X2R*C4
##	vaddpd %zmm7,%zmm8,%zmm7 # X(2,4)  # 4 空き
        vfmadd213pd %zmm8,%zmm15,%zmm7
##
##	vmulpd %zmm11,%zmm5,%zmm5 # X4R*2
##	vsubpd %zmm10,%zmm5,%zmm5 # X(1,1)	
        vfmsub213pd %zmm10,%zmm11,%zmm5
##
##	vmulpd %zmm11,%zmm8,%zmm8 # X4I*2
##	vsubpd %zmm7,%zmm8,%zmm8 # X(2,1)	
        vfmsub213pd %zmm7,%zmm11,%zmm8	
##	
	vmovapd %zmm0,  (%rsi,%rax)
	vmovapd %zmm1,64(%rsi,%rax)
	vmovapd %zmm5,  (%rdx,%rax)
	vmovapd %zmm8,64(%rdx,%rax)
	vmovapd %zmm2,  (%r8,%rax)
	vmovapd %zmm3,64(%r8,%rax)
	vmovapd %zmm6,  (%r9,%rax)
	vmovapd %zmm9,64(%r9,%rax)
	vmovapd %zmm10,  (%r10,%rax)
	vmovapd %zmm7,64(%r10,%rax)

	addq $128,%rax	
	cmpq %rdi,%rax
	jne L1

	ret

C0:	# 2D0	
	.long   0x00000000,0x40000000	
C1:	# 0.25D0
        .long   0x00000000,0x3fd00000
C2:	# 0.5590169943749474241D0
	.long  0x9b97f4a8,0x3fe1e377 
C3:	# 0.6180339887498948482D0
	.long  0x372fe950,0x3fe3c6ef
C4:	# -0.9510565162951535721D0
	.long 0x134454ff,0xbfee6f0e
	
