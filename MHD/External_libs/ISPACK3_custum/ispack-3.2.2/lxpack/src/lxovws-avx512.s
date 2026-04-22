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
.globl lxovws_
.globl _lxovws_	
lxovws_:
_lxovws_:	
	movq   (%rdi), %rdi  # : JB が rdi に
	movq   (%r8), %r8  # : IL が r8 に
	movq   (%r9), %r9  # : ILEV が r9 に	
	
	# JB: rdi
        # AC: rsi	
	# SD: rdx	
	# Q: rcx
	# IL: r8
	# ILEV: r9

	subq $1,%r8
	shlq $3,%r8
	addq %r8,%rsi

	shlq $6,%rdi # JB*8*8 が rdi に
	movq %rdi,%r10
	movq %rdi,%r11			
	shlq $3,%r10
	shlq $2,%r11
	addq %r11,%r10
	subq %rdi,%r10	# r10 に JB*8*4*11 が入る
	addq %rcx,%r10

	cmpq $0,%r9
	je L0
	cmpq $9,%r9
	je L9
	cmpq $8,%r9
	je L8
	cmpq $7,%r9
	je L7
	cmpq $6,%r9
	je L6
	cmpq $5,%r9
	je L5
	cmpq $4,%r9
	je L4
	cmpq $3,%r9
	je L3
	cmpq $2,%r9
	je L2
	
# ILEV=0 case
L0:
	call LC
	addq $128,%rdx
	addq $32,%rsi
	call LC
	ret

# ILEV=9 case
L9:
	call LC
	addq $128,%rdx
	addq $32,%rsi
	call LC5
	ret

# ILEV=8 case
L8:
	call LC
	addq $128,%rdx
	addq $32,%rsi
	call LC4
	ret

# ILEV=7 case
L7:
	call LC
	addq $128,%rdx
	addq $32,%rsi
	call LC3
	ret

# ILEV=6 case
L6:
	call LC
	addq $128,%rdx
	addq $32,%rsi
	call LC2
	ret

# ILEV=5 case
L5:
	call LC5
	ret

# ILEV=4 case
L4:
	call LC4
	ret

# ILEV=3 case
L3:
	call LC3
	ret

# ILEV=2 case
L2:
	call LC2
	ret

# common
LC:	
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 

#	vsubpd %zmm16,%zmm16,%zmm16
#	vsubpd %zmm17,%zmm17,%zmm17	
#	vsubpd %zmm18,%zmm18,%zmm18
#	vsubpd %zmm19,%zmm19,%zmm19
#	vsubpd %zmm20,%zmm20,%zmm20
#	vsubpd %zmm21,%zmm21,%zmm21
#	vsubpd %zmm22,%zmm22,%zmm22	
#	vsubpd %zmm23,%zmm23,%zmm23
#	vsubpd %zmm24,%zmm24,%zmm24
#	vsubpd %zmm25,%zmm25,%zmm25
#	vsubpd %zmm26,%zmm26,%zmm26
#	vsubpd %zmm27,%zmm27,%zmm27
#	vsubpd %zmm28,%zmm28,%zmm28
#	vsubpd %zmm29,%zmm29,%zmm29
#	vsubpd %zmm30,%zmm30,%zmm30	
#	vsubpd %zmm31,%zmm31,%zmm31	
	
	vpxorq %zmm16,%zmm16,%zmm16
	vpxorq %zmm17,%zmm17,%zmm17	
	vpxorq %zmm18,%zmm18,%zmm18
	vpxorq %zmm19,%zmm19,%zmm19
	vpxorq %zmm20,%zmm20,%zmm20
	vpxorq %zmm21,%zmm21,%zmm21
	vpxorq %zmm22,%zmm22,%zmm22	
	vpxorq %zmm23,%zmm23,%zmm23
	vpxorq %zmm24,%zmm24,%zmm24
	vpxorq %zmm25,%zmm25,%zmm25
	vpxorq %zmm26,%zmm26,%zmm26
	vpxorq %zmm27,%zmm27,%zmm27
	vpxorq %zmm28,%zmm28,%zmm28
	vpxorq %zmm29,%zmm29,%zmm29
	vpxorq %zmm30,%zmm30,%zmm30	
	vpxorq %zmm31,%zmm31,%zmm31	
	
	movq %rcx,%rax
LC0:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm15
	vfmadd231pd %zmm14,%zmm8,%zmm15
	vfmadd213pd %zmm12,%zmm13,%zmm15 # Q1new

	vfmadd213pd %zmm11,%zmm10,%zmm14
	vfmadd213pd %zmm13,%zmm15,%zmm14 # Q2new
	
	vmovapd %zmm15,64(%rax) # Q1new
	vmovapd %zmm14,128(%rax) # Q2new	

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm20
	vfmadd231pd %zmm13,%zmm14,%zmm28

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm24
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm21
	vfmadd231pd %zmm13,%zmm14,%zmm29
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm25

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm22
	vfmadd231pd %zmm13,%zmm14,%zmm30

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm26
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm23
	vfmadd231pd %zmm13,%zmm14,%zmm31
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm27

	addq $704,%rax
	cmpq %rax,%r10
	jne LC0

	vshuff64x2 $27,%zmm16,%zmm16,%zmm0
	vaddpd %zmm0,%zmm16,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd (%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)

	vshuff64x2 $27,%zmm17,%zmm17,%zmm0
	vaddpd %zmm0,%zmm17,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 8(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,8(%rdx)

	vshuff64x2 $27,%zmm18,%zmm18,%zmm0
	vaddpd %zmm0,%zmm18,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 16(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,16(%rdx)

	vshuff64x2 $27,%zmm19,%zmm19,%zmm0
	vaddpd %zmm0,%zmm19,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 24(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,24(%rdx)

	vshuff64x2 $27,%zmm20,%zmm20,%zmm0
	vaddpd %zmm0,%zmm20,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 32(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,32(%rdx)

	vshuff64x2 $27,%zmm21,%zmm21,%zmm0
	vaddpd %zmm0,%zmm21,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 40(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,40(%rdx)

	vshuff64x2 $27,%zmm22,%zmm22,%zmm0
	vaddpd %zmm0,%zmm22,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 48(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,48(%rdx)

	vshuff64x2 $27,%zmm23,%zmm23,%zmm0
	vaddpd %zmm0,%zmm23,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 56(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,56(%rdx)

	vshuff64x2 $27,%zmm24,%zmm24,%zmm0
	vaddpd %zmm0,%zmm24,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 64(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,64(%rdx)

	vshuff64x2 $27,%zmm25,%zmm25,%zmm0
	vaddpd %zmm0,%zmm25,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 72(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,72(%rdx)

	vshuff64x2 $27,%zmm26,%zmm26,%zmm0
	vaddpd %zmm0,%zmm26,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 80(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,80(%rdx)

	vshuff64x2 $27,%zmm27,%zmm27,%zmm0
	vaddpd %zmm0,%zmm27,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 88(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,88(%rdx)

	vshuff64x2 $27,%zmm28,%zmm28,%zmm0
	vaddpd %zmm0,%zmm28,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 96(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,96(%rdx)

	vshuff64x2 $27,%zmm29,%zmm29,%zmm0
	vaddpd %zmm0,%zmm29,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 104(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,104(%rdx)

	vshuff64x2 $27,%zmm30,%zmm30,%zmm0
	vaddpd %zmm0,%zmm30,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 112(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,112(%rdx)

	vshuff64x2 $27,%zmm31,%zmm31,%zmm0
	vaddpd %zmm0,%zmm31,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 120(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,120(%rdx)

	ret

# common for case5
LC5:
	vbroadcastsd   (%rsi),%zmm3  # AC1 
	vbroadcastsd  8(%rsi),%zmm4  # AC2 

#	vsubpd %zmm16,%zmm16,%zmm16
#	vsubpd %zmm17,%zmm17,%zmm17	
#	vsubpd %zmm18,%zmm18,%zmm18
#	vsubpd %zmm19,%zmm19,%zmm19
#	vsubpd %zmm20,%zmm20,%zmm20
#	vsubpd %zmm21,%zmm21,%zmm21
#	vsubpd %zmm22,%zmm22,%zmm22	
#	vsubpd %zmm23,%zmm23,%zmm23
#	vsubpd %zmm24,%zmm24,%zmm24
#	vsubpd %zmm25,%zmm25,%zmm25
#	vsubpd %zmm26,%zmm26,%zmm26
#	vsubpd %zmm27,%zmm27,%zmm27
#	vsubpd %zmm28,%zmm28,%zmm28
#	vsubpd %zmm29,%zmm29,%zmm29
#	vsubpd %zmm30,%zmm30,%zmm30	
#	vsubpd %zmm31,%zmm31,%zmm31
#	vsubpd %zmm8,%zmm8,%zmm8
#	vsubpd %zmm9,%zmm9,%zmm9
#	vsubpd %zmm10,%zmm10,%zmm10
#	vsubpd %zmm11,%zmm11,%zmm11
#	vsubpd %zmm12,%zmm12,%zmm12
#	vsubpd %zmm13,%zmm13,%zmm13
#	vsubpd %zmm14,%zmm14,%zmm14
#	vsubpd %zmm15,%zmm15,%zmm15
	
	vpxorq %zmm16,%zmm16,%zmm16
	vpxorq %zmm17,%zmm17,%zmm17	
	vpxorq %zmm18,%zmm18,%zmm18
	vpxorq %zmm19,%zmm19,%zmm19
	vpxorq %zmm20,%zmm20,%zmm20
	vpxorq %zmm21,%zmm21,%zmm21
	vpxorq %zmm22,%zmm22,%zmm22	
	vpxorq %zmm23,%zmm23,%zmm23
	vpxorq %zmm24,%zmm24,%zmm24
	vpxorq %zmm25,%zmm25,%zmm25
	vpxorq %zmm26,%zmm26,%zmm26
	vpxorq %zmm27,%zmm27,%zmm27
	vpxorq %zmm28,%zmm28,%zmm28
	vpxorq %zmm29,%zmm29,%zmm29
	vpxorq %zmm30,%zmm30,%zmm30	
	vpxorq %zmm31,%zmm31,%zmm31
	vpxorq %zmm8,%zmm8,%zmm8
	vpxorq %zmm9,%zmm9,%zmm9
	vpxorq %zmm10,%zmm10,%zmm10
	vpxorq %zmm11,%zmm11,%zmm11
	vpxorq %zmm12,%zmm12,%zmm12
	vpxorq %zmm13,%zmm13,%zmm13
	vpxorq %zmm14,%zmm14,%zmm14
	vpxorq %zmm15,%zmm15,%zmm15
	
	movq %rcx,%rax
LC50:
	vmovapd   (%rax),%zmm0 # X2					
	vmovapd 64(%rax),%zmm1 # Q1
	vmovapd 128(%rax),%zmm2 # Q2

	vfmadd213pd %zmm4,%zmm3,%zmm0
	vfmadd213pd %zmm1,%zmm2,%zmm0 # Q1new

	vmovapd 192(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm20
	vfmadd231pd %zmm2,%zmm5,%zmm28
	vfmadd231pd %zmm0,%zmm5,%zmm12	

	vmovapd 256(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm16
	vfmadd231pd %zmm2,%zmm5,%zmm24
	vfmadd231pd %zmm0,%zmm5,%zmm8
	
	vmovapd 320(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm21
	vfmadd231pd %zmm2,%zmm5,%zmm29
	vfmadd231pd %zmm0,%zmm5,%zmm13
	
	vmovapd 384(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm17
	vfmadd231pd %zmm2,%zmm5,%zmm25
	vfmadd231pd %zmm0,%zmm5,%zmm9	

	vmovapd 448(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm22
	vfmadd231pd %zmm2,%zmm5,%zmm30
	vfmadd231pd %zmm0,%zmm5,%zmm14	

	vmovapd 512(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm18
	vfmadd231pd %zmm2,%zmm5,%zmm26
	vfmadd231pd %zmm0,%zmm5,%zmm10	
	
	vmovapd 576(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm23
	vfmadd231pd %zmm2,%zmm5,%zmm31
	vfmadd231pd %zmm0,%zmm5,%zmm15	
		
	vmovapd 640(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm19
	vfmadd231pd %zmm2,%zmm5,%zmm27
	vfmadd231pd %zmm0,%zmm5,%zmm11

	addq $704,%rax
	cmpq %rax,%r10
	jne LC50

	vshuff64x2 $27,%zmm16,%zmm16,%zmm0
	vaddpd %zmm0,%zmm16,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd (%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)

	vshuff64x2 $27,%zmm17,%zmm17,%zmm0
	vaddpd %zmm0,%zmm17,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 8(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,8(%rdx)

	vshuff64x2 $27,%zmm18,%zmm18,%zmm0
	vaddpd %zmm0,%zmm18,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 16(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,16(%rdx)

	vshuff64x2 $27,%zmm19,%zmm19,%zmm0
	vaddpd %zmm0,%zmm19,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 24(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,24(%rdx)

	vshuff64x2 $27,%zmm20,%zmm20,%zmm0
	vaddpd %zmm0,%zmm20,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 32(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,32(%rdx)

	vshuff64x2 $27,%zmm21,%zmm21,%zmm0
	vaddpd %zmm0,%zmm21,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 40(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,40(%rdx)

	vshuff64x2 $27,%zmm22,%zmm22,%zmm0
	vaddpd %zmm0,%zmm22,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 48(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,48(%rdx)

	vshuff64x2 $27,%zmm23,%zmm23,%zmm0
	vaddpd %zmm0,%zmm23,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 56(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,56(%rdx)

	vshuff64x2 $27,%zmm24,%zmm24,%zmm0
	vaddpd %zmm0,%zmm24,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 64(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,64(%rdx)

	vshuff64x2 $27,%zmm25,%zmm25,%zmm0
	vaddpd %zmm0,%zmm25,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 72(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,72(%rdx)

	vshuff64x2 $27,%zmm26,%zmm26,%zmm0
	vaddpd %zmm0,%zmm26,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 80(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,80(%rdx)

	vshuff64x2 $27,%zmm27,%zmm27,%zmm0
	vaddpd %zmm0,%zmm27,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 88(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,88(%rdx)

	vshuff64x2 $27,%zmm28,%zmm28,%zmm0
	vaddpd %zmm0,%zmm28,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 96(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,96(%rdx)

	vshuff64x2 $27,%zmm29,%zmm29,%zmm0
	vaddpd %zmm0,%zmm29,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 104(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,104(%rdx)

	vshuff64x2 $27,%zmm30,%zmm30,%zmm0
	vaddpd %zmm0,%zmm30,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 112(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,112(%rdx)

	vshuff64x2 $27,%zmm31,%zmm31,%zmm0
	vaddpd %zmm0,%zmm31,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 120(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,120(%rdx)

	vshuff64x2 $27,%zmm8,%zmm8,%zmm0
	vaddpd %zmm0,%zmm8,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 128(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,128(%rdx)

	vshuff64x2 $27,%zmm9,%zmm9,%zmm0
	vaddpd %zmm0,%zmm9,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 136(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,136(%rdx)

	vshuff64x2 $27,%zmm10,%zmm10,%zmm0
	vaddpd %zmm0,%zmm10,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 144(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,144(%rdx)

	vshuff64x2 $27,%zmm11,%zmm11,%zmm0
	vaddpd %zmm0,%zmm11,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 152(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,152(%rdx)

	vshuff64x2 $27,%zmm12,%zmm12,%zmm0
	vaddpd %zmm0,%zmm12,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 160(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,160(%rdx)

	vshuff64x2 $27,%zmm13,%zmm13,%zmm0
	vaddpd %zmm0,%zmm13,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 168(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,168(%rdx)

	vshuff64x2 $27,%zmm14,%zmm14,%zmm0
	vaddpd %zmm0,%zmm14,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 176(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,176(%rdx)

	vshuff64x2 $27,%zmm15,%zmm15,%zmm0
	vaddpd %zmm0,%zmm15,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 184(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,184(%rdx)
	
	ret

# common for case4
LC4:
	vbroadcastsd   (%rsi),%zmm3  # AC1 
	vbroadcastsd  8(%rsi),%zmm4  # AC2 

#	vsubpd %zmm16,%zmm16,%zmm16
#	vsubpd %zmm17,%zmm17,%zmm17	
#	vsubpd %zmm18,%zmm18,%zmm18
#	vsubpd %zmm19,%zmm19,%zmm19
#	vsubpd %zmm20,%zmm20,%zmm20
#	vsubpd %zmm21,%zmm21,%zmm21
#	vsubpd %zmm22,%zmm22,%zmm22	
#	vsubpd %zmm23,%zmm23,%zmm23
#	vsubpd %zmm24,%zmm24,%zmm24
#	vsubpd %zmm25,%zmm25,%zmm25
#	vsubpd %zmm26,%zmm26,%zmm26
#	vsubpd %zmm27,%zmm27,%zmm27
#	vsubpd %zmm28,%zmm28,%zmm28
#	vsubpd %zmm29,%zmm29,%zmm29
#	vsubpd %zmm30,%zmm30,%zmm30	
#	vsubpd %zmm31,%zmm31,%zmm31
#	vsubpd %zmm8,%zmm8,%zmm8
#	vsubpd %zmm9,%zmm9,%zmm9
#	vsubpd %zmm10,%zmm10,%zmm10
#	vsubpd %zmm11,%zmm11,%zmm11
	
	vpxorq %zmm16,%zmm16,%zmm16
	vpxorq %zmm17,%zmm17,%zmm17	
	vpxorq %zmm18,%zmm18,%zmm18
	vpxorq %zmm19,%zmm19,%zmm19
	vpxorq %zmm20,%zmm20,%zmm20
	vpxorq %zmm21,%zmm21,%zmm21
	vpxorq %zmm22,%zmm22,%zmm22	
	vpxorq %zmm23,%zmm23,%zmm23
	vpxorq %zmm24,%zmm24,%zmm24
	vpxorq %zmm25,%zmm25,%zmm25
	vpxorq %zmm26,%zmm26,%zmm26
	vpxorq %zmm27,%zmm27,%zmm27
	vpxorq %zmm28,%zmm28,%zmm28
	vpxorq %zmm29,%zmm29,%zmm29
	vpxorq %zmm30,%zmm30,%zmm30	
	vpxorq %zmm31,%zmm31,%zmm31
	vpxorq %zmm8,%zmm8,%zmm8
	vpxorq %zmm9,%zmm9,%zmm9
	vpxorq %zmm10,%zmm10,%zmm10
	vpxorq %zmm11,%zmm11,%zmm11
	
	movq %rcx,%rax
LC40:
	vmovapd   (%rax),%zmm0 # X2					
	vmovapd 64(%rax),%zmm1 # Q1
	vmovapd 128(%rax),%zmm2 # Q2

	vfmadd213pd %zmm4,%zmm3,%zmm0
	vfmadd213pd %zmm1,%zmm2,%zmm0 # Q1new

	vmovapd 192(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm20
	vfmadd231pd %zmm2,%zmm5,%zmm28

	vmovapd 256(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm16
	vfmadd231pd %zmm2,%zmm5,%zmm24
	vfmadd231pd %zmm0,%zmm5,%zmm8
	
	vmovapd 320(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm21
	vfmadd231pd %zmm2,%zmm5,%zmm29
	
	vmovapd 384(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm17
	vfmadd231pd %zmm2,%zmm5,%zmm25
	vfmadd231pd %zmm0,%zmm5,%zmm9	

	vmovapd 448(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm22
	vfmadd231pd %zmm2,%zmm5,%zmm30

	vmovapd 512(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm18
	vfmadd231pd %zmm2,%zmm5,%zmm26
	vfmadd231pd %zmm0,%zmm5,%zmm10	
	
	vmovapd 576(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm23
	vfmadd231pd %zmm2,%zmm5,%zmm31
		
	vmovapd 640(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm19
	vfmadd231pd %zmm2,%zmm5,%zmm27
	vfmadd231pd %zmm0,%zmm5,%zmm11

	addq $704,%rax
	cmpq %rax,%r10
	jne LC40

	vshuff64x2 $27,%zmm16,%zmm16,%zmm0
	vaddpd %zmm0,%zmm16,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd (%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)

	vshuff64x2 $27,%zmm17,%zmm17,%zmm0
	vaddpd %zmm0,%zmm17,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 8(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,8(%rdx)

	vshuff64x2 $27,%zmm18,%zmm18,%zmm0
	vaddpd %zmm0,%zmm18,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 16(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,16(%rdx)

	vshuff64x2 $27,%zmm19,%zmm19,%zmm0
	vaddpd %zmm0,%zmm19,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 24(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,24(%rdx)

	vshuff64x2 $27,%zmm20,%zmm20,%zmm0
	vaddpd %zmm0,%zmm20,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 32(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,32(%rdx)

	vshuff64x2 $27,%zmm21,%zmm21,%zmm0
	vaddpd %zmm0,%zmm21,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 40(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,40(%rdx)

	vshuff64x2 $27,%zmm22,%zmm22,%zmm0
	vaddpd %zmm0,%zmm22,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 48(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,48(%rdx)

	vshuff64x2 $27,%zmm23,%zmm23,%zmm0
	vaddpd %zmm0,%zmm23,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 56(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,56(%rdx)

	vshuff64x2 $27,%zmm24,%zmm24,%zmm0
	vaddpd %zmm0,%zmm24,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 64(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,64(%rdx)

	vshuff64x2 $27,%zmm25,%zmm25,%zmm0
	vaddpd %zmm0,%zmm25,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 72(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,72(%rdx)

	vshuff64x2 $27,%zmm26,%zmm26,%zmm0
	vaddpd %zmm0,%zmm26,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 80(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,80(%rdx)

	vshuff64x2 $27,%zmm27,%zmm27,%zmm0
	vaddpd %zmm0,%zmm27,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 88(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,88(%rdx)

	vshuff64x2 $27,%zmm28,%zmm28,%zmm0
	vaddpd %zmm0,%zmm28,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 96(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,96(%rdx)

	vshuff64x2 $27,%zmm29,%zmm29,%zmm0
	vaddpd %zmm0,%zmm29,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 104(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,104(%rdx)

	vshuff64x2 $27,%zmm30,%zmm30,%zmm0
	vaddpd %zmm0,%zmm30,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 112(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,112(%rdx)

	vshuff64x2 $27,%zmm31,%zmm31,%zmm0
	vaddpd %zmm0,%zmm31,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 120(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,120(%rdx)

	vshuff64x2 $27,%zmm8,%zmm8,%zmm0
	vaddpd %zmm0,%zmm8,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 128(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,128(%rdx)

	vshuff64x2 $27,%zmm9,%zmm9,%zmm0
	vaddpd %zmm0,%zmm9,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 136(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,136(%rdx)

	vshuff64x2 $27,%zmm10,%zmm10,%zmm0
	vaddpd %zmm0,%zmm10,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 144(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,144(%rdx)

	vshuff64x2 $27,%zmm11,%zmm11,%zmm0
	vaddpd %zmm0,%zmm11,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 152(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,152(%rdx)

	ret

# common for case3
LC3:
#	vsubpd %zmm16,%zmm16,%zmm16
#	vsubpd %zmm17,%zmm17,%zmm17	
#	vsubpd %zmm18,%zmm18,%zmm18
#	vsubpd %zmm19,%zmm19,%zmm19
#	vsubpd %zmm20,%zmm20,%zmm20
#	vsubpd %zmm21,%zmm21,%zmm21
#	vsubpd %zmm22,%zmm22,%zmm22	
#	vsubpd %zmm23,%zmm23,%zmm23
#	vsubpd %zmm24,%zmm24,%zmm24
#	vsubpd %zmm25,%zmm25,%zmm25
#	vsubpd %zmm26,%zmm26,%zmm26
#	vsubpd %zmm27,%zmm27,%zmm27
#	vsubpd %zmm28,%zmm28,%zmm28
#	vsubpd %zmm29,%zmm29,%zmm29
#	vsubpd %zmm30,%zmm30,%zmm30	
#	vsubpd %zmm31,%zmm31,%zmm31
	
	vpxorq %zmm16,%zmm16,%zmm16
	vpxorq %zmm17,%zmm17,%zmm17	
	vpxorq %zmm18,%zmm18,%zmm18
	vpxorq %zmm19,%zmm19,%zmm19
	vpxorq %zmm20,%zmm20,%zmm20
	vpxorq %zmm21,%zmm21,%zmm21
	vpxorq %zmm22,%zmm22,%zmm22	
	vpxorq %zmm23,%zmm23,%zmm23
	vpxorq %zmm24,%zmm24,%zmm24
	vpxorq %zmm25,%zmm25,%zmm25
	vpxorq %zmm26,%zmm26,%zmm26
	vpxorq %zmm27,%zmm27,%zmm27
	vpxorq %zmm28,%zmm28,%zmm28
	vpxorq %zmm29,%zmm29,%zmm29
	vpxorq %zmm30,%zmm30,%zmm30	
	vpxorq %zmm31,%zmm31,%zmm31
	
	movq %rcx,%rax
LC30:
	vmovapd 64(%rax),%zmm1 # Q1
	vmovapd 128(%rax),%zmm2 # Q2

	vmovapd 192(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm20
	vfmadd231pd %zmm2,%zmm5,%zmm28

	vmovapd 256(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm16
	vfmadd231pd %zmm2,%zmm5,%zmm24
	
	vmovapd 320(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm21
	vfmadd231pd %zmm2,%zmm5,%zmm29
	
	vmovapd 384(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm17
	vfmadd231pd %zmm2,%zmm5,%zmm25

	vmovapd 448(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm22
	vfmadd231pd %zmm2,%zmm5,%zmm30

	vmovapd 512(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm18
	vfmadd231pd %zmm2,%zmm5,%zmm26
	
	vmovapd 576(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm23
	vfmadd231pd %zmm2,%zmm5,%zmm31
		
	vmovapd 640(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm19
	vfmadd231pd %zmm2,%zmm5,%zmm27

	addq $704,%rax
	cmpq %rax,%r10
	jne LC30

	vshuff64x2 $27,%zmm16,%zmm16,%zmm0
	vaddpd %zmm0,%zmm16,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd (%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)

	vshuff64x2 $27,%zmm17,%zmm17,%zmm0
	vaddpd %zmm0,%zmm17,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 8(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,8(%rdx)

	vshuff64x2 $27,%zmm18,%zmm18,%zmm0
	vaddpd %zmm0,%zmm18,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 16(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,16(%rdx)

	vshuff64x2 $27,%zmm19,%zmm19,%zmm0
	vaddpd %zmm0,%zmm19,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 24(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,24(%rdx)

	vshuff64x2 $27,%zmm20,%zmm20,%zmm0
	vaddpd %zmm0,%zmm20,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 32(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,32(%rdx)

	vshuff64x2 $27,%zmm21,%zmm21,%zmm0
	vaddpd %zmm0,%zmm21,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 40(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,40(%rdx)

	vshuff64x2 $27,%zmm22,%zmm22,%zmm0
	vaddpd %zmm0,%zmm22,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 48(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,48(%rdx)

	vshuff64x2 $27,%zmm23,%zmm23,%zmm0
	vaddpd %zmm0,%zmm23,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 56(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,56(%rdx)

	vshuff64x2 $27,%zmm24,%zmm24,%zmm0
	vaddpd %zmm0,%zmm24,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 64(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,64(%rdx)

	vshuff64x2 $27,%zmm25,%zmm25,%zmm0
	vaddpd %zmm0,%zmm25,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 72(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,72(%rdx)

	vshuff64x2 $27,%zmm26,%zmm26,%zmm0
	vaddpd %zmm0,%zmm26,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 80(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,80(%rdx)

	vshuff64x2 $27,%zmm27,%zmm27,%zmm0
	vaddpd %zmm0,%zmm27,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 88(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,88(%rdx)

	vshuff64x2 $27,%zmm28,%zmm28,%zmm0
	vaddpd %zmm0,%zmm28,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 96(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,96(%rdx)

	vshuff64x2 $27,%zmm29,%zmm29,%zmm0
	vaddpd %zmm0,%zmm29,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 104(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,104(%rdx)

	vshuff64x2 $27,%zmm30,%zmm30,%zmm0
	vaddpd %zmm0,%zmm30,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 112(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,112(%rdx)

	vshuff64x2 $27,%zmm31,%zmm31,%zmm0
	vaddpd %zmm0,%zmm31,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 120(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,120(%rdx)

	ret

# common for case2
LC2:
#	vsubpd %zmm16,%zmm16,%zmm16
#	vsubpd %zmm17,%zmm17,%zmm17	
#	vsubpd %zmm18,%zmm18,%zmm18
#	vsubpd %zmm19,%zmm19,%zmm19
#	vsubpd %zmm20,%zmm20,%zmm20
#	vsubpd %zmm21,%zmm21,%zmm21
#	vsubpd %zmm22,%zmm22,%zmm22	
#	vsubpd %zmm23,%zmm23,%zmm23
#	vsubpd %zmm24,%zmm24,%zmm24
#	vsubpd %zmm25,%zmm25,%zmm25
#	vsubpd %zmm26,%zmm26,%zmm26
#	vsubpd %zmm27,%zmm27,%zmm27
	
	vpxorq %zmm16,%zmm16,%zmm16
	vpxorq %zmm17,%zmm17,%zmm17	
	vpxorq %zmm18,%zmm18,%zmm18
	vpxorq %zmm19,%zmm19,%zmm19
	vpxorq %zmm20,%zmm20,%zmm20
	vpxorq %zmm21,%zmm21,%zmm21
	vpxorq %zmm22,%zmm22,%zmm22	
	vpxorq %zmm23,%zmm23,%zmm23
	vpxorq %zmm24,%zmm24,%zmm24
	vpxorq %zmm25,%zmm25,%zmm25
	vpxorq %zmm26,%zmm26,%zmm26
	vpxorq %zmm27,%zmm27,%zmm27
	
	movq %rcx,%rax
LC20:
	vmovapd 64(%rax),%zmm1 # Q1
	vmovapd 128(%rax),%zmm2 # Q2

	vmovapd 192(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm20

	vmovapd 256(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm16
	vfmadd231pd %zmm2,%zmm5,%zmm24
	
	vmovapd 320(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm21
	
	vmovapd 384(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm17
	vfmadd231pd %zmm2,%zmm5,%zmm25

	vmovapd 448(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm22

	vmovapd 512(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm18
	vfmadd231pd %zmm2,%zmm5,%zmm26
	
	vmovapd 576(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm23
		
	vmovapd 640(%rax),%zmm5
	vfmadd231pd %zmm1,%zmm5,%zmm19
	vfmadd231pd %zmm2,%zmm5,%zmm27

	addq $704,%rax
	cmpq %rax,%r10
	jne LC20

	vshuff64x2 $27,%zmm16,%zmm16,%zmm0
	vaddpd %zmm0,%zmm16,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd (%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)

	vshuff64x2 $27,%zmm17,%zmm17,%zmm0
	vaddpd %zmm0,%zmm17,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 8(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,8(%rdx)

	vshuff64x2 $27,%zmm18,%zmm18,%zmm0
	vaddpd %zmm0,%zmm18,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 16(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,16(%rdx)

	vshuff64x2 $27,%zmm19,%zmm19,%zmm0
	vaddpd %zmm0,%zmm19,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 24(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,24(%rdx)

	vshuff64x2 $27,%zmm20,%zmm20,%zmm0
	vaddpd %zmm0,%zmm20,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 32(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,32(%rdx)

	vshuff64x2 $27,%zmm21,%zmm21,%zmm0
	vaddpd %zmm0,%zmm21,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 40(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,40(%rdx)

	vshuff64x2 $27,%zmm22,%zmm22,%zmm0
	vaddpd %zmm0,%zmm22,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 48(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,48(%rdx)

	vshuff64x2 $27,%zmm23,%zmm23,%zmm0
	vaddpd %zmm0,%zmm23,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 56(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,56(%rdx)

	vshuff64x2 $27,%zmm24,%zmm24,%zmm0
	vaddpd %zmm0,%zmm24,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 64(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,64(%rdx)

	vshuff64x2 $27,%zmm25,%zmm25,%zmm0
	vaddpd %zmm0,%zmm25,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 72(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,72(%rdx)

	vshuff64x2 $27,%zmm26,%zmm26,%zmm0
	vaddpd %zmm0,%zmm26,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 80(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,80(%rdx)

	vshuff64x2 $27,%zmm27,%zmm27,%zmm0
	vaddpd %zmm0,%zmm27,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 88(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,88(%rdx)

	ret

