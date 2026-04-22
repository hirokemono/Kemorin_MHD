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
.globl lxogws_
.globl _lxogws_	
lxogws_:
_lxogws_:	
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
	shlq $3,%r10	
	subq %rdi,%r10	# r10 に JB*8*8*7 が入る
	addq %rcx,%r10

	cmpq $0,%r9
	je L01
	
	cmpq $2,%r9
	je L21
	cmpq $3,%r9
	je L31
	cmpq $4,%r9
	je L41
	cmpq $5,%r9
	je L51
	cmpq $6,%r9
	je L61
	cmpq $7,%r9
	je L71
	cmpq $8,%r9
	je L81
	cmpq $9,%r9
	je L91

# ILEV=0 case

L01:	
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 
	vbroadcastsd 32(%rsi),%zmm4 # AC5 
	vbroadcastsd 40(%rsi),%zmm5 # AC6
	vbroadcastsd 48(%rsi),%zmm6 # AC7
	vbroadcastsd 56(%rsi),%zmm7 # AC8

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
L00:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd %zmm11,%zmm1	
	vfmadd231pd %zmm14,%zmm10,%zmm1
	vfmadd213pd %zmm13,%zmm0,%zmm1 # Q2new

	vmovapd %zmm5,%zmm15
	vfmadd231pd %zmm14,%zmm4,%zmm15
	vfmadd213pd %zmm0,%zmm1,%zmm15	# Q1new2

	vfmadd213pd %zmm7,%zmm6,%zmm14
	vfmadd213pd %zmm1,%zmm15,%zmm14 # Q2new2

	vmovapd %zmm15,64(%rax) # Q1					
	vmovapd %zmm14,128(%rax) # Q2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22
	vfmadd231pd %zmm0,%zmm14,%zmm26
	vfmadd231pd %zmm1,%zmm14,%zmm30

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	vfmadd231pd %zmm0,%zmm14,%zmm24
	vfmadd231pd %zmm1,%zmm14,%zmm28
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
	vfmadd231pd %zmm0,%zmm14,%zmm27
	vfmadd231pd %zmm1,%zmm14,%zmm31
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21
	vfmadd231pd %zmm0,%zmm14,%zmm25
	vfmadd231pd %zmm1,%zmm14,%zmm29

	addq $448,%rax
	cmpq %rax,%r10
	jne L00

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

# ILEV=9 case
L91:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 32(%rsi),%zmm4 # AC5 

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
#	vsubpd %zmm5,%zmm5,%zmm5
#	vsubpd %zmm6,%zmm6,%zmm6
#	vsubpd %zmm7,%zmm7,%zmm7
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
	vpxorq %zmm5,%zmm5,%zmm5
	vpxorq %zmm6,%zmm6,%zmm6
	vpxorq %zmm7,%zmm7,%zmm7
	vpxorq %zmm11,%zmm11,%zmm11

	movq %rcx,%rax
L90:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vbroadcastsd 24(%rsi),%zmm1	
	vfmadd231pd %zmm14,%zmm10,%zmm1
	vfmadd213pd %zmm13,%zmm0,%zmm1 # Q2new

	vbroadcastsd 40(%rsi),%zmm15
	vfmadd231pd %zmm14,%zmm4,%zmm15
	vfmadd213pd %zmm0,%zmm1,%zmm15	# Q1new2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22
	vfmadd231pd %zmm0,%zmm14,%zmm26
	vfmadd231pd %zmm1,%zmm14,%zmm30
	vfmadd231pd %zmm15,%zmm14,%zmm7

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	vfmadd231pd %zmm0,%zmm14,%zmm24
	vfmadd231pd %zmm1,%zmm14,%zmm28
	vfmadd231pd %zmm15,%zmm14,%zmm5
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
	vfmadd231pd %zmm0,%zmm14,%zmm27
	vfmadd231pd %zmm1,%zmm14,%zmm31
	vfmadd231pd %zmm15,%zmm14,%zmm11
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21
	vfmadd231pd %zmm0,%zmm14,%zmm25
	vfmadd231pd %zmm1,%zmm14,%zmm29
	vfmadd231pd %zmm15,%zmm14,%zmm6

	addq $448,%rax
	cmpq %rax,%r10
	jne L90

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

	vshuff64x2 $27,%zmm5,%zmm5,%zmm0
	vaddpd %zmm0,%zmm5,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 128(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,128(%rdx)

	vshuff64x2 $27,%zmm6,%zmm6,%zmm0
	vaddpd %zmm0,%zmm6,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 136(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,136(%rdx)

	vshuff64x2 $27,%zmm7,%zmm7,%zmm0
	vaddpd %zmm0,%zmm7,%zmm0
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
	
# ILEV=8 case
L81:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 
	vbroadcastsd 32(%rsi),%zmm4 # AC5 
	vbroadcastsd 40(%rsi),%zmm7 # AC6

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
#	vsubpd %zmm5,%zmm5,%zmm5
#	vsubpd %zmm6,%zmm6,%zmm6
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
	vpxorq %zmm5,%zmm5,%zmm5
	vpxorq %zmm6,%zmm6,%zmm6

	movq %rcx,%rax
L80:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd %zmm11,%zmm1
	vfmadd231pd %zmm14,%zmm10,%zmm1
	vfmadd213pd %zmm13,%zmm0,%zmm1 # Q2new

	vmovapd %zmm7,%zmm15
	vfmadd231pd %zmm14,%zmm4,%zmm15
	vfmadd213pd %zmm0,%zmm1,%zmm15	# Q1new2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22
	vfmadd231pd %zmm0,%zmm14,%zmm26
	vfmadd231pd %zmm1,%zmm14,%zmm30

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	vfmadd231pd %zmm0,%zmm14,%zmm24
	vfmadd231pd %zmm1,%zmm14,%zmm28
	vfmadd231pd %zmm15,%zmm14,%zmm5
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
	vfmadd231pd %zmm0,%zmm14,%zmm27
	vfmadd231pd %zmm1,%zmm14,%zmm31
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21
	vfmadd231pd %zmm0,%zmm14,%zmm25
	vfmadd231pd %zmm1,%zmm14,%zmm29
	vfmadd231pd %zmm15,%zmm14,%zmm6

	addq $448,%rax
	cmpq %rax,%r10
	jne L80

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

	vshuff64x2 $27,%zmm5,%zmm5,%zmm0
	vaddpd %zmm0,%zmm5,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 128(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,128(%rdx)

	vshuff64x2 $27,%zmm6,%zmm6,%zmm0
	vaddpd %zmm0,%zmm6,%zmm0
	vhaddpd %ymm0,%ymm0,%ymm0	
	vextractf128 $1,%ymm0,%xmm1
	vaddsd 136(%rdx),%xmm1,%xmm1
	vaddsd %xmm1,%xmm0,%xmm0
	vmovsd %xmm0,136(%rdx)

	ret
	
# ILEV=7 case
L71:
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
L70:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd %zmm11,%zmm1
	vfmadd231pd %zmm14,%zmm10,%zmm1
	vfmadd213pd %zmm13,%zmm0,%zmm1 # Q2new

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22
	vfmadd231pd %zmm0,%zmm14,%zmm26
	vfmadd231pd %zmm1,%zmm14,%zmm30

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	vfmadd231pd %zmm0,%zmm14,%zmm24
	vfmadd231pd %zmm1,%zmm14,%zmm28
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
	vfmadd231pd %zmm0,%zmm14,%zmm27
	vfmadd231pd %zmm1,%zmm14,%zmm31
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21
	vfmadd231pd %zmm0,%zmm14,%zmm25
	vfmadd231pd %zmm1,%zmm14,%zmm29

	addq $448,%rax
	cmpq %rax,%r10
	jne L70

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
	
# ILEV=6 case
L61:
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

	movq %rcx,%rax
L60:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd %zmm11,%zmm1
	vfmadd231pd %zmm14,%zmm10,%zmm1
	vfmadd213pd %zmm13,%zmm0,%zmm1 # Q2new

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22
	vfmadd231pd %zmm0,%zmm14,%zmm26

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	vfmadd231pd %zmm0,%zmm14,%zmm24
	vfmadd231pd %zmm1,%zmm14,%zmm28
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
	vfmadd231pd %zmm0,%zmm14,%zmm27
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21
	vfmadd231pd %zmm0,%zmm14,%zmm25
	vfmadd231pd %zmm1,%zmm14,%zmm29

	addq $448,%rax
	cmpq %rax,%r10
	jne L60

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

	ret
	
# ILEV=5 case
L51:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 

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
L50:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22
	vfmadd231pd %zmm0,%zmm14,%zmm26

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	vfmadd231pd %zmm0,%zmm14,%zmm24
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
	vfmadd231pd %zmm0,%zmm14,%zmm27
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21
	vfmadd231pd %zmm0,%zmm14,%zmm25

	addq $448,%rax
	cmpq %rax,%r10
	jne L50

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
	
# ILEV=4 case
L41:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 

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
	
	movq %rcx,%rax
L40:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	vfmadd231pd %zmm0,%zmm14,%zmm24
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21
	vfmadd231pd %zmm0,%zmm14,%zmm25

	addq $448,%rax
	cmpq %rax,%r10
	jne L40

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

	ret
	
# ILEV=3 case
L31:
#	vsubpd %zmm16,%zmm16,%zmm16
#	vsubpd %zmm17,%zmm17,%zmm17	
#	vsubpd %zmm18,%zmm18,%zmm18
#	vsubpd %zmm19,%zmm19,%zmm19
#	vsubpd %zmm20,%zmm20,%zmm20
#	vsubpd %zmm21,%zmm21,%zmm21
#	vsubpd %zmm22,%zmm22,%zmm22	
#	vsubpd %zmm23,%zmm23,%zmm23
	vpxorq %zmm16,%zmm16,%zmm16
	vpxorq %zmm17,%zmm17,%zmm17	
	vpxorq %zmm18,%zmm18,%zmm18
	vpxorq %zmm19,%zmm19,%zmm19
	vpxorq %zmm20,%zmm20,%zmm20
	vpxorq %zmm21,%zmm21,%zmm21
	vpxorq %zmm22,%zmm22,%zmm22	
	vpxorq %zmm23,%zmm23,%zmm23

	movq %rcx,%rax
L30:
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18
	vfmadd231pd %zmm13,%zmm14,%zmm22

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
	vfmadd231pd %zmm13,%zmm14,%zmm23
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21

	addq $448,%rax
	cmpq %rax,%r10
	jne L30

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

	ret
	
# ILEV=2 case
L21:
#	vsubpd %zmm16,%zmm16,%zmm16
#	vsubpd %zmm17,%zmm17,%zmm17	
#	vsubpd %zmm18,%zmm18,%zmm18
#	vsubpd %zmm19,%zmm19,%zmm19
#	vsubpd %zmm20,%zmm20,%zmm20
#	vsubpd %zmm21,%zmm21,%zmm21
	vpxorq %zmm16,%zmm16,%zmm16
	vpxorq %zmm17,%zmm17,%zmm17	
	vpxorq %zmm18,%zmm18,%zmm18
	vpxorq %zmm19,%zmm19,%zmm19
	vpxorq %zmm20,%zmm20,%zmm20
	vpxorq %zmm21,%zmm21,%zmm21

	movq %rcx,%rax
L20:
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm18

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm16
	vfmadd231pd %zmm13,%zmm14,%zmm20
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm19
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm12,%zmm14,%zmm17
	vfmadd231pd %zmm13,%zmm14,%zmm21

	addq $448,%rax
	cmpq %rax,%r10
	jne L20

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

	ret
