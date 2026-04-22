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
.globl lxogzs_
.globl _lxogzs_	
lxogzs_:
_lxogzs_:	
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

	shlq $6,%rdi # JB*8*8 が rsi に
	movq %rdi,%r10	
	shlq $2,%r10
	addq %rdi,%r10	# r10 に JB*8*8*5 が入る
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
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3
#        vsubpd %zmm4,%zmm4,%zmm4
#        vsubpd %zmm5,%zmm5,%zmm5
#        vsubpd %zmm6,%zmm6,%zmm6
#        vsubpd %zmm7,%zmm7,%zmm7

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3
        vpxorq %zmm4,%zmm4,%zmm4
        vpxorq %zmm5,%zmm5,%zmm5
        vpxorq %zmm6,%zmm6,%zmm6
        vpxorq %zmm7,%zmm7,%zmm7

	vbroadcastsd   (%rsi),%zmm16  # AC1 
	vbroadcastsd  8(%rsi),%zmm17  # AC2 
	vbroadcastsd 16(%rsi),%zmm18 # AC3 
	vbroadcastsd 24(%rsi),%zmm19 # AC4 
	vbroadcastsd 32(%rsi),%zmm20 # AC5 
	vbroadcastsd 40(%rsi),%zmm21 # AC6 
	vbroadcastsd 48(%rsi),%zmm22 # AC7
	vbroadcastsd 56(%rsi),%zmm23 # AC8

	movq %rcx,%rax
L00:
	vmovapd   (%rax),%zmm24 # X2					
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vmovapd %zmm16,%zmm31
	vfmadd213pd %zmm17,%zmm24,%zmm31
	vfmadd231pd %zmm26,%zmm31,%zmm25 # Q1

	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	
	
	vmovapd %zmm18,%zmm31
	vfmadd213pd %zmm19,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm25,%zmm26 # Q2

	vfmadd231pd %zmm25,%zmm27,%zmm5 # SD6=SD6+G1R*Q1	
	vfmadd231pd %zmm25,%zmm28,%zmm4 # SD5=SD5+G2R*Q1	

	vmovapd %zmm20,%zmm31
	vfmadd213pd %zmm21,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm26,%zmm25 # Q1
	
	vfmadd231pd %zmm26,%zmm27,%zmm7	# SD8=SD8+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm6 # SD7=SD7+G2R*Q2
	
	vfmadd213pd %zmm23,%zmm22,%zmm24
	vfmadd231pd %zmm24,%zmm25,%zmm26 # Q2
	
	vmovapd %zmm25,64(%rax) # Q1					
	vmovapd %zmm26,128(%rax) # Q2
	
	addq $320,%rax
	cmpq %rax,%r10
	jne L00

	vshuff64x2 $11,%zmm0,%zmm0,%zmm8
	vshuff64x2 $11,%zmm1,%zmm1,%zmm9
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vshuff64x2 $11,%zmm3,%zmm3,%zmm11
	vshuff64x2 $11,%zmm4,%zmm4,%zmm12
	vshuff64x2 $11,%zmm5,%zmm5,%zmm13
	vshuff64x2 $11,%zmm6,%zmm6,%zmm14
	vshuff64x2 $11,%zmm7,%zmm7,%zmm15

	vaddpd %ymm8,%ymm0,%ymm0
	vaddpd %ymm9,%ymm1,%ymm1
	vaddpd %ymm10,%ymm2,%ymm2
	vaddpd %ymm11,%ymm3,%ymm3
	vaddpd %ymm12,%ymm4,%ymm4
	vaddpd %ymm13,%ymm5,%ymm5
	vaddpd %ymm14,%ymm6,%ymm6
	vaddpd %ymm15,%ymm7,%ymm7

	vhaddpd %ymm0,%ymm0,%ymm0
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vhaddpd %ymm3,%ymm3,%ymm3
	vhaddpd %ymm4,%ymm4,%ymm4
	vhaddpd %ymm5,%ymm5,%ymm5
	vhaddpd %ymm6,%ymm6,%ymm6
	vhaddpd %ymm7,%ymm7,%ymm7

	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vextractf128 $1,%ymm3,%xmm11
	vextractf128 $1,%ymm4,%xmm12
	vextractf128 $1,%ymm5,%xmm13
	vextractf128 $1,%ymm6,%xmm14
	vextractf128 $1,%ymm7,%xmm15

	vaddsd (%rdx),%xmm8,%xmm8
	vaddsd 8(%rdx),%xmm9,%xmm9
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd 24(%rdx),%xmm11,%xmm11
	vaddsd 32(%rdx),%xmm12,%xmm12
	vaddsd 40(%rdx),%xmm13,%xmm13
	vaddsd 48(%rdx),%xmm14,%xmm14
	vaddsd 56(%rdx),%xmm15,%xmm15
	
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vaddsd %xmm12,%xmm4,%xmm4
	vaddsd %xmm13,%xmm5,%xmm5
	vaddsd %xmm14,%xmm6,%xmm6
	vaddsd %xmm15,%xmm7,%xmm7

	vmovsd %xmm0,(%rdx)
	vmovsd %xmm1,8(%rdx)
	vmovsd %xmm2,16(%rdx)
	vmovsd %xmm3,24(%rdx)
	vmovsd %xmm4,32(%rdx)
	vmovsd %xmm5,40(%rdx)
	vmovsd %xmm6,48(%rdx)
	vmovsd %xmm7,56(%rdx)				
	
	ret

# ILEV=9 case
L91:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3
#        vsubpd %zmm4,%zmm4,%zmm4
#        vsubpd %zmm5,%zmm5,%zmm5
#        vsubpd %zmm6,%zmm6,%zmm6
#        vsubpd %zmm7,%zmm7,%zmm7
#        vsubpd %zmm8,%zmm8,%zmm8
#        vsubpd %zmm9,%zmm9,%zmm9

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3
        vpxorq %zmm4,%zmm4,%zmm4
        vpxorq %zmm5,%zmm5,%zmm5
        vpxorq %zmm6,%zmm6,%zmm6
        vpxorq %zmm7,%zmm7,%zmm7
        vpxorq %zmm8,%zmm8,%zmm8
        vpxorq %zmm9,%zmm9,%zmm9

	vbroadcastsd   (%rsi),%zmm16  # AC1 
	vbroadcastsd  8(%rsi),%zmm17  # AC2 
	vbroadcastsd 16(%rsi),%zmm18 # AC3 
	vbroadcastsd 24(%rsi),%zmm19 # AC4 
	vbroadcastsd 32(%rsi),%zmm20 # AC5 
	vbroadcastsd 40(%rsi),%zmm21 # AC6 

	movq %rcx,%rax
L90:
	vmovapd   (%rax),%zmm24 # X2					
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vmovapd %zmm16,%zmm31
	vfmadd213pd %zmm17,%zmm24,%zmm31
	vfmadd231pd %zmm26,%zmm31,%zmm25 # Q1

	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	
	
	vmovapd %zmm18,%zmm31
	vfmadd213pd %zmm19,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm25,%zmm26 # Q2

	vfmadd231pd %zmm25,%zmm27,%zmm5 # SD6=SD6+G1R*Q1	
	vfmadd231pd %zmm25,%zmm28,%zmm4 # SD5=SD5+G2R*Q1	

	vmovapd %zmm20,%zmm31
	vfmadd213pd %zmm21,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm26,%zmm25 # Q1
	
	vfmadd231pd %zmm26,%zmm27,%zmm7	# SD8=SD8+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm6 # SD7=SD7+G2R*Q2
	
	vfmadd231pd %zmm25,%zmm27,%zmm9 # SD10=SD10+G1R*Q1	
	vfmadd231pd %zmm25,%zmm28,%zmm8 # SD9=SD9+G2R*Q1	
	
	addq $320,%rax
	cmpq %rax,%r10
	jne L90

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vshuff64x2 $11,%zmm3,%zmm3,%zmm10
	vaddpd %ymm10,%ymm3,%ymm3
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm10
	vaddsd 24(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)
	
	vshuff64x2 $11,%zmm4,%zmm4,%zmm10
	vaddpd %ymm10,%ymm4,%ymm4
	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm10
	vaddsd 32(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)
	
	vshuff64x2 $11,%zmm5,%zmm5,%zmm10
	vaddpd %ymm10,%ymm5,%ymm5
	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm10
	vaddsd 40(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)
	
	vshuff64x2 $11,%zmm6,%zmm6,%zmm10
	vaddpd %ymm10,%ymm6,%ymm6
	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm10
	vaddsd 48(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm6,%xmm6
	vmovsd %xmm6,48(%rdx)
	
	vshuff64x2 $11,%zmm7,%zmm7,%zmm10
	vaddpd %ymm10,%ymm7,%ymm7
	vhaddpd %ymm7,%ymm7,%ymm7
	vextractf128 $1,%ymm7,%xmm10
	vaddsd 56(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm7,%xmm7
	vmovsd %xmm7,56(%rdx)
	
	vshuff64x2 $11,%zmm8,%zmm8,%zmm10
	vaddpd %ymm10,%ymm8,%ymm8
	vhaddpd %ymm8,%ymm8,%ymm8
	vextractf128 $1,%ymm8,%xmm10
	vaddsd 64(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm8,%xmm8
	vmovsd %xmm8,64(%rdx)
	
	vshuff64x2 $11,%zmm9,%zmm9,%zmm10
	vaddpd %ymm10,%ymm9,%ymm9
	vhaddpd %ymm9,%ymm9,%ymm9
	vextractf128 $1,%ymm9,%xmm10
	vaddsd 72(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm9,%xmm9
	vmovsd %xmm9,72(%rdx)
	
	ret

# ILEV=8 case
L81:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3
#        vsubpd %zmm4,%zmm4,%zmm4
#        vsubpd %zmm5,%zmm5,%zmm5
#        vsubpd %zmm6,%zmm6,%zmm6
#        vsubpd %zmm7,%zmm7,%zmm7
#        vsubpd %zmm8,%zmm8,%zmm8

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3
        vpxorq %zmm4,%zmm4,%zmm4
        vpxorq %zmm5,%zmm5,%zmm5
        vpxorq %zmm6,%zmm6,%zmm6
        vpxorq %zmm7,%zmm7,%zmm7
        vpxorq %zmm8,%zmm8,%zmm8

	vbroadcastsd   (%rsi),%zmm16  # AC1 
	vbroadcastsd  8(%rsi),%zmm17  # AC2 
	vbroadcastsd 16(%rsi),%zmm18 # AC3 
	vbroadcastsd 24(%rsi),%zmm19 # AC4 
	vbroadcastsd 32(%rsi),%zmm20 # AC5 
	vbroadcastsd 40(%rsi),%zmm21 # AC6 

	movq %rcx,%rax
L80:
	vmovapd   (%rax),%zmm24 # X2					
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vmovapd %zmm16,%zmm31
	vfmadd213pd %zmm17,%zmm24,%zmm31
	vfmadd231pd %zmm26,%zmm31,%zmm25 # Q1

	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	
	
	vmovapd %zmm18,%zmm31
	vfmadd213pd %zmm19,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm25,%zmm26 # Q2

	vfmadd231pd %zmm25,%zmm27,%zmm5 # SD6=SD6+G1R*Q1	
	vfmadd231pd %zmm25,%zmm28,%zmm4 # SD5=SD5+G2R*Q1	

	vmovapd %zmm20,%zmm31
	vfmadd213pd %zmm21,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm26,%zmm25 # Q1
	
	vfmadd231pd %zmm26,%zmm27,%zmm7	# SD8=SD8+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm6 # SD7=SD7+G2R*Q2
	
	vfmadd231pd %zmm25,%zmm28,%zmm8 # SD9=SD9+G2R*Q1	
	
	addq $320,%rax
	cmpq %rax,%r10
	jne L80

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vshuff64x2 $11,%zmm3,%zmm3,%zmm10
	vaddpd %ymm10,%ymm3,%ymm3
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm10
	vaddsd 24(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)
	
	vshuff64x2 $11,%zmm4,%zmm4,%zmm10
	vaddpd %ymm10,%ymm4,%ymm4
	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm10
	vaddsd 32(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)
	
	vshuff64x2 $11,%zmm5,%zmm5,%zmm10
	vaddpd %ymm10,%ymm5,%ymm5
	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm10
	vaddsd 40(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)
	
	vshuff64x2 $11,%zmm6,%zmm6,%zmm10
	vaddpd %ymm10,%ymm6,%ymm6
	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm10
	vaddsd 48(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm6,%xmm6
	vmovsd %xmm6,48(%rdx)
	
	vshuff64x2 $11,%zmm7,%zmm7,%zmm10
	vaddpd %ymm10,%ymm7,%ymm7
	vhaddpd %ymm7,%ymm7,%ymm7
	vextractf128 $1,%ymm7,%xmm10
	vaddsd 56(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm7,%xmm7
	vmovsd %xmm7,56(%rdx)
	
	vshuff64x2 $11,%zmm8,%zmm8,%zmm10
	vaddpd %ymm10,%ymm8,%ymm8
	vhaddpd %ymm8,%ymm8,%ymm8
	vextractf128 $1,%ymm8,%xmm10
	vaddsd 64(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm8,%xmm8
	vmovsd %xmm8,64(%rdx)
	
	ret
	
# ILEV=7 case
L71:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3
#        vsubpd %zmm4,%zmm4,%zmm4
#        vsubpd %zmm5,%zmm5,%zmm5
#        vsubpd %zmm6,%zmm6,%zmm6
#        vsubpd %zmm7,%zmm7,%zmm7

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3
        vpxorq %zmm4,%zmm4,%zmm4
        vpxorq %zmm5,%zmm5,%zmm5
        vpxorq %zmm6,%zmm6,%zmm6
        vpxorq %zmm7,%zmm7,%zmm7

	vbroadcastsd   (%rsi),%zmm16  # AC1 
	vbroadcastsd  8(%rsi),%zmm17  # AC2 
	vbroadcastsd 16(%rsi),%zmm18 # AC3 
	vbroadcastsd 24(%rsi),%zmm19 # AC4 

	movq %rcx,%rax
L70:
	vmovapd   (%rax),%zmm24 # X2					
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vmovapd %zmm16,%zmm31
	vfmadd213pd %zmm17,%zmm24,%zmm31
	vfmadd231pd %zmm26,%zmm31,%zmm25 # Q1

	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	
	
	vmovapd %zmm18,%zmm31
	vfmadd213pd %zmm19,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm25,%zmm26 # Q2

	vfmadd231pd %zmm25,%zmm27,%zmm5 # SD6=SD6+G1R*Q1	
	vfmadd231pd %zmm25,%zmm28,%zmm4 # SD5=SD5+G2R*Q1	

	vfmadd231pd %zmm26,%zmm27,%zmm7	# SD8=SD8+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm6 # SD7=SD7+G2R*Q2
	
	addq $320,%rax
	cmpq %rax,%r10
	jne L70

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vshuff64x2 $11,%zmm3,%zmm3,%zmm10
	vaddpd %ymm10,%ymm3,%ymm3
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm10
	vaddsd 24(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)
	
	vshuff64x2 $11,%zmm4,%zmm4,%zmm10
	vaddpd %ymm10,%ymm4,%ymm4
	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm10
	vaddsd 32(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)
	
	vshuff64x2 $11,%zmm5,%zmm5,%zmm10
	vaddpd %ymm10,%ymm5,%ymm5
	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm10
	vaddsd 40(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)
	
	vshuff64x2 $11,%zmm6,%zmm6,%zmm10
	vaddpd %ymm10,%ymm6,%ymm6
	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm10
	vaddsd 48(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm6,%xmm6
	vmovsd %xmm6,48(%rdx)
	
	vshuff64x2 $11,%zmm7,%zmm7,%zmm10
	vaddpd %ymm10,%ymm7,%ymm7
	vhaddpd %ymm7,%ymm7,%ymm7
	vextractf128 $1,%ymm7,%xmm10
	vaddsd 56(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm7,%xmm7
	vmovsd %xmm7,56(%rdx)
	
	ret
	
# ILEV=6 case
L61:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3
#        vsubpd %zmm4,%zmm4,%zmm4
#        vsubpd %zmm5,%zmm5,%zmm5
#        vsubpd %zmm6,%zmm6,%zmm6

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3
        vpxorq %zmm4,%zmm4,%zmm4
        vpxorq %zmm5,%zmm5,%zmm5
        vpxorq %zmm6,%zmm6,%zmm6

	vbroadcastsd   (%rsi),%zmm16  # AC1 
	vbroadcastsd  8(%rsi),%zmm17  # AC2 
	vbroadcastsd 16(%rsi),%zmm18 # AC3 
	vbroadcastsd 24(%rsi),%zmm19 # AC4 

	movq %rcx,%rax
L60:
	vmovapd   (%rax),%zmm24 # X2					
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vmovapd %zmm16,%zmm31
	vfmadd213pd %zmm17,%zmm24,%zmm31
	vfmadd231pd %zmm26,%zmm31,%zmm25 # Q1

	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	
	
	vmovapd %zmm18,%zmm31
	vfmadd213pd %zmm19,%zmm24,%zmm31
	vfmadd231pd %zmm31,%zmm25,%zmm26 # Q2

	vfmadd231pd %zmm25,%zmm27,%zmm5 # SD6=SD6+G1R*Q1	
	vfmadd231pd %zmm25,%zmm28,%zmm4 # SD5=SD5+G2R*Q1	

	vfmadd231pd %zmm26,%zmm28,%zmm6 # SD7=SD7+G2R*Q2
	
	addq $320,%rax
	cmpq %rax,%r10
	jne L60

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vshuff64x2 $11,%zmm3,%zmm3,%zmm10
	vaddpd %ymm10,%ymm3,%ymm3
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm10
	vaddsd 24(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)
	
	vshuff64x2 $11,%zmm4,%zmm4,%zmm10
	vaddpd %ymm10,%ymm4,%ymm4
	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm10
	vaddsd 32(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)
	
	vshuff64x2 $11,%zmm5,%zmm5,%zmm10
	vaddpd %ymm10,%ymm5,%ymm5
	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm10
	vaddsd 40(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)
	
	vshuff64x2 $11,%zmm6,%zmm6,%zmm10
	vaddpd %ymm10,%ymm6,%ymm6
	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm10
	vaddsd 48(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm6,%xmm6
	vmovsd %xmm6,48(%rdx)
	
	ret
	
# ILEV=5 case
L51:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3
#        vsubpd %zmm4,%zmm4,%zmm4
#        vsubpd %zmm5,%zmm5,%zmm5

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3
        vpxorq %zmm4,%zmm4,%zmm4
        vpxorq %zmm5,%zmm5,%zmm5

	vbroadcastsd   (%rsi),%zmm16  # AC1 
	vbroadcastsd  8(%rsi),%zmm17  # AC2 

	movq %rcx,%rax
L50:
	vmovapd   (%rax),%zmm24 # X2					
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vmovapd %zmm16,%zmm31
	vfmadd213pd %zmm17,%zmm24,%zmm31
	vfmadd231pd %zmm26,%zmm31,%zmm25 # Q1

	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	

	vfmadd231pd %zmm25,%zmm27,%zmm5 # SD6=SD6+G1R*Q1	
	vfmadd231pd %zmm25,%zmm28,%zmm4 # SD5=SD5+G2R*Q1	
	
	addq $320,%rax
	cmpq %rax,%r10
	jne L50

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vshuff64x2 $11,%zmm3,%zmm3,%zmm10
	vaddpd %ymm10,%ymm3,%ymm3
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm10
	vaddsd 24(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)
	
	vshuff64x2 $11,%zmm4,%zmm4,%zmm10
	vaddpd %ymm10,%ymm4,%ymm4
	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm10
	vaddsd 32(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)
	
	vshuff64x2 $11,%zmm5,%zmm5,%zmm10
	vaddpd %ymm10,%ymm5,%ymm5
	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm10
	vaddsd 40(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)
	
	ret
	
# ILEV=4 case
L41:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3
#        vsubpd %zmm4,%zmm4,%zmm4

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3
        vpxorq %zmm4,%zmm4,%zmm4

	vbroadcastsd   (%rsi),%zmm16  # AC1 
	vbroadcastsd  8(%rsi),%zmm17  # AC2 

	movq %rcx,%rax
L40:
	vmovapd   (%rax),%zmm24 # X2					
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vmovapd %zmm16,%zmm31
	vfmadd213pd %zmm17,%zmm24,%zmm31
	vfmadd231pd %zmm26,%zmm31,%zmm25 # Q1

	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	

	vfmadd231pd %zmm25,%zmm28,%zmm4 # SD5=SD5+G2R*Q1	
	
	addq $320,%rax
	cmpq %rax,%r10
	jne L40

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vshuff64x2 $11,%zmm3,%zmm3,%zmm10
	vaddpd %ymm10,%ymm3,%ymm3
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm10
	vaddsd 24(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)
	
	vshuff64x2 $11,%zmm4,%zmm4,%zmm10
	vaddpd %ymm10,%ymm4,%ymm4
	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm10
	vaddsd 32(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)
	
	ret
	
# ILEV=3 case
L31:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2
#        vsubpd %zmm3,%zmm3,%zmm3

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2
        vpxorq %zmm3,%zmm3,%zmm3

	movq %rcx,%rax
L30:
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vfmadd231pd %zmm26,%zmm27,%zmm3 # SD4=SD4+G1R*Q2
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	

	addq $320,%rax
	cmpq %rax,%r10
	jne L30

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vshuff64x2 $11,%zmm3,%zmm3,%zmm10
	vaddpd %ymm10,%ymm3,%ymm3
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm10
	vaddsd 24(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)
	
	ret

# ILEV=2 case
L21:
#        vsubpd %zmm0,%zmm0,%zmm0
#        vsubpd %zmm1,%zmm1,%zmm1
#        vsubpd %zmm2,%zmm2,%zmm2

	vpxorq %zmm0,%zmm0,%zmm0
        vpxorq %zmm1,%zmm1,%zmm1
        vpxorq %zmm2,%zmm2,%zmm2

	movq %rcx,%rax
L20:
	vmovapd 64(%rax),%zmm25 # Q1
	vmovapd 128(%rax),%zmm26 # Q2

	vmovapd 192(%rax),%zmm27 # G1R
	vmovapd 256(%rax),%zmm28 # G2R

	vfmadd231pd %zmm25,%zmm27,%zmm1 # SD2=SD2+G1R*Q1
	vfmadd231pd %zmm25,%zmm28,%zmm0 # SD1=SD1+G2R*Q1
	
	vfmadd231pd %zmm26,%zmm28,%zmm2 # SD3=SD3+G2R*Q2	

	addq $320,%rax
	cmpq %rax,%r10
	jne L20

	vshuff64x2 $11,%zmm0,%zmm0,%zmm10
	vaddpd %ymm10,%ymm0,%ymm0
	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm10
	vaddsd (%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)
	
	vshuff64x2 $11,%zmm1,%zmm1,%zmm10
	vaddpd %ymm10,%ymm1,%ymm1
	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm10
	vaddsd 8(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm1,%xmm1
	vmovsd %xmm1,8(%rdx)
	
	vshuff64x2 $11,%zmm2,%zmm2,%zmm10
	vaddpd %ymm10,%ymm2,%ymm2
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm10
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	ret
