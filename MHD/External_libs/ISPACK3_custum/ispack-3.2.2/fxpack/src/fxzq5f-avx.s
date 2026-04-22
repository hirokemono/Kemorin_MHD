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
.globl fxzq5f_
.globl _fxzq5f_	
fxzq5f_:
_fxzq5f_:
	movq (%rdi), %rdi  # L が rdi に
        movq $0x6666666666666667, %rax
        imulq %rdi 
        sarq $1, %rdx # この段階で, L/5 が %rdx に入る
	# X の先頭アドレスは rsi
	movq %rdx, %rdi  # L/5 が rdi に
	shlq $6,%rdi # L/5*2*M*8=L/5*64
	
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

	vbroadcastsd C0(%rip),%ymm11
	vbroadcastsd C1(%rip),%ymm12
	vbroadcastsd C2(%rip),%ymm13
	vbroadcastsd C3(%rip),%ymm14
	vbroadcastsd C4(%rip),%ymm15
	
	
L1:	vxorpd %ymm1,%ymm1,%ymm1
	vxorpd %ymm3,%ymm3,%ymm3
	vxorpd %ymm5,%ymm5,%ymm5
	vxorpd %ymm7,%ymm7,%ymm7
	vxorpd %ymm9,%ymm9,%ymm9	

	vmovapd   (%rsi,%rax), %ymm0 # X(1,0)
	vsubpd 32(%rsi,%rax), %ymm1, %ymm1 # X(2,0)
	vmovapd   (%rdx,%rax), %ymm2 # X(1,1)
	vsubpd 32(%rdx,%rax), %ymm3, %ymm3 # X(2,1)
	vmovapd   (%r8, %rax), %ymm4 # X(1,2)
	vsubpd 32(%r8, %rax), %ymm5, %ymm5 # X(2,2)
	vmovapd   (%r9, %rax), %ymm6 # X(1,3)
	vsubpd 32(%r9, %rax), %ymm7, %ymm7 # X(2,3)
	vmovapd   (%r10,%rax), %ymm8 # X(1,4)
	vsubpd 32(%r10,%rax), %ymm9, %ymm9 # X(2,4)

	vsubpd %ymm8,%ymm2,%ymm10 # X0R
	vaddpd %ymm8,%ymm2,%ymm2 # X1R	
	vsubpd %ymm9,%ymm3,%ymm8 # X0I
	vaddpd %ymm9,%ymm3,%ymm3 # X1I # 9 空き

	vsubpd %ymm6,%ymm4,%ymm9 # X3R
	vaddpd %ymm6,%ymm4,%ymm4 # X4R # 	
	vsubpd %ymm7,%ymm5,%ymm6 # X3I
	vaddpd %ymm7,%ymm5,%ymm5 # X4I #  7空き

	vmulpd %ymm14,%ymm9,%ymm7 # X3R*C3
	vaddpd %ymm10,%ymm7,%ymm7 # X2R
	vmulpd %ymm14,%ymm10,%ymm10 # X0R*C3
	vsubpd %ymm9,%ymm10,%ymm9 # X3R # 10 空き

	vmulpd %ymm14,%ymm6,%ymm10 # X3I*C3
	vaddpd %ymm10,%ymm8,%ymm10 # X2I
	vmulpd %ymm14,%ymm8,%ymm8 # X0I*C3
	vsubpd %ymm6,%ymm8,%ymm6 # X3I # 8 空き
	
	vaddpd %ymm4,%ymm2,%ymm8 # X0R
	vsubpd %ymm4,%ymm2,%ymm2 # X1R # 4 空き
	vaddpd %ymm5,%ymm3,%ymm4 # X0I
	vsubpd %ymm5,%ymm3,%ymm3 # X1I # 5空き
	
	vmulpd %ymm12,%ymm8,%ymm5 # X0R*C1
	vsubpd %ymm5,%ymm0,%ymm5 # X4R
	vaddpd %ymm8,%ymm0,%ymm0 # X(1,0) # 8 空き
	vmulpd %ymm12,%ymm4,%ymm8 # X0I*C1
	vsubpd %ymm8,%ymm1,%ymm8 # X4I
	vaddpd %ymm4,%ymm1,%ymm1 # X(2,0) # 4 空き

	#--

	vmulpd %ymm13,%ymm2,%ymm2 # X1R*C2
	vsubpd %ymm2,%ymm5,%ymm2 # X1R  # 4 空き
	vmulpd %ymm13,%ymm3,%ymm3 # X1I*C2
	vsubpd %ymm3,%ymm8,%ymm3 # X1I  # 4 空き

	vmulpd %ymm11,%ymm5,%ymm5 # X4R*2
	vsubpd %ymm2,%ymm5,%ymm5 # X4R  # 4 空き	
	vmulpd %ymm11,%ymm8,%ymm8 # X4I*2
	vsubpd %ymm3,%ymm8,%ymm8 # X4I  # 4 空き


	vmulpd %ymm15,%ymm6,%ymm6 # X3I*C4
	vsubpd %ymm6,%ymm2,%ymm6 # X(1,3)  # 4 空き
	vmulpd %ymm15,%ymm9,%ymm9 # X3R*C4
	vaddpd %ymm9,%ymm3,%ymm9 # X(2,3)  # 4 空き

	vmulpd %ymm11,%ymm2,%ymm2 # X1R*2
	vsubpd %ymm6,%ymm2,%ymm2 # X(1,2)
	vmulpd %ymm11,%ymm3,%ymm3 # X1I*2
	vsubpd %ymm9,%ymm3,%ymm3 # X(2,2)

	vmulpd %ymm15,%ymm10,%ymm10 # X2I*C4
	vsubpd %ymm10,%ymm5,%ymm10 # X(1,4)  # 4 空き
	vmulpd %ymm15,%ymm7,%ymm7 # X2R*C4
	vaddpd %ymm7,%ymm8,%ymm7 # X(2,4)  # 4 空き
	
	vmulpd %ymm11,%ymm5,%ymm5 # X4R*2
	vsubpd %ymm10,%ymm5,%ymm5 # X(1,1)	
	vmulpd %ymm11,%ymm8,%ymm8 # X4I*2
	vsubpd %ymm7,%ymm8,%ymm8 # X(2,1)	
	
	
	vmovapd %ymm0,  (%rsi,%rax)
	vmovapd %ymm1,32(%rsi,%rax)
	vmovapd %ymm5,  (%rdx,%rax)
	vmovapd %ymm8,32(%rdx,%rax)
	vmovapd %ymm2,  (%r8,%rax)
	vmovapd %ymm3,32(%r8,%rax)
	vmovapd %ymm6,  (%r9,%rax)
	vmovapd %ymm9,32(%r9,%rax)
	vmovapd %ymm10,  (%r10,%rax)
	vmovapd %ymm7,32(%r10,%rax)

	addq $64,%rax	
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
	
