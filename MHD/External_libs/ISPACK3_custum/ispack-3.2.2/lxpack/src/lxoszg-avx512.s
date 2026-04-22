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
.globl lxoszg_
.globl _lxoszg_	
lxoszg_:
_lxoszg_:	
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
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 
	vbroadcastsd 32(%rsi),%zmm4 # AC5 
	vbroadcastsd 40(%rsi),%zmm5 # AC6 
	vbroadcastsd 48(%rsi),%zmm6 # AC7
	vbroadcastsd 56(%rsi),%zmm7 # AC8

	vbroadcastsd   (%rdx),%zmm3 # SD1	
	vbroadcastsd  8(%rdx),%zmm2 # SD2
	
	vbroadcastsd 16(%rdx),%zmm16 # SD3
	vbroadcastsd 24(%rdx),%zmm17 # SD4
	vbroadcastsd 32(%rdx),%zmm18 # SD5
	vbroadcastsd 40(%rdx),%zmm19 # SD6
	vbroadcastsd 48(%rdx),%zmm20 # SD7
	vbroadcastsd 56(%rdx),%zmm21 # SD8
	
	movq %rcx,%rax
L00:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1
	
	vmovapd %zmm8,%zmm15
	vfmadd213pd %zmm9,%zmm14,%zmm15	
	vfmadd231pd %zmm13,%zmm15,%zmm12 # Q1

	vfmadd231pd %zmm16,%zmm13,%zmm1	# G2R+SD3*Q2	
	vfmadd231pd %zmm17,%zmm13,%zmm0 # G1R+SD4*Q2
	
	vmovapd %zmm11,%zmm15
	vfmadd231pd %zmm14,%zmm10,%zmm15	
	vfmadd231pd %zmm15,%zmm12,%zmm13 # Q2
	
	vfmadd231pd %zmm18,%zmm12,%zmm1	# G2R+SD5*Q1	
	vfmadd231pd %zmm19,%zmm12,%zmm0 # G1R+SD6*Q1
	
	vmovapd %zmm5,%zmm15
	vfmadd231pd %zmm14,%zmm4,%zmm15	
	vfmadd231pd %zmm15,%zmm13,%zmm12 # Q1
	
	vfmadd231pd %zmm20,%zmm13,%zmm1	# G2R+SD7*Q2
	vfmadd231pd %zmm21,%zmm13,%zmm0 # G1R+SD8*Q2
	
	vmovapd %zmm7,%zmm15
	vfmadd231pd %zmm14,%zmm6,%zmm15
	vfmadd231pd %zmm15,%zmm12,%zmm13 # Q2
	
	vmovapd %zmm12,64(%rax) # Q1					
	vmovapd %zmm13,128(%rax) # Q2
	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L00

	ret

# ILEV=9 case
L91:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 
	vbroadcastsd 32(%rsi),%zmm4 # AC5 
	vbroadcastsd 40(%rsi),%zmm5 # AC6 

	vbroadcastsd  8(%rdx),%zmm2 # SD2
	vbroadcastsd   (%rdx),%zmm3 # SD1

	vbroadcastsd 16(%rdx),%zmm16 # SD3
	vbroadcastsd 24(%rdx),%zmm17 # SD4
	vbroadcastsd 32(%rdx),%zmm18 # SD5
	vbroadcastsd 40(%rdx),%zmm19 # SD6
	vbroadcastsd 48(%rdx),%zmm20 # SD7
	vbroadcastsd 56(%rdx),%zmm21 # SD8
	
	vbroadcastsd 64(%rdx),%zmm6 # SD9
	vbroadcastsd 72(%rdx),%zmm7 # SD10
	
	movq %rcx,%rax
L90:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1
	
	vmovapd %zmm8,%zmm15
	vfmadd213pd %zmm9,%zmm14,%zmm15	
	vfmadd231pd %zmm13,%zmm15,%zmm12 # Q1

	vfmadd231pd %zmm16,%zmm13,%zmm1	# G2R+SD3*Q2
	vfmadd231pd %zmm17,%zmm13,%zmm0 # G1R+SD4*Q2
	
	vmovapd %zmm11,%zmm15
	vfmadd231pd %zmm14,%zmm10,%zmm15	
	vfmadd231pd %zmm15,%zmm12,%zmm13 # Q2
	
	vfmadd231pd %zmm18,%zmm12,%zmm1	# G2R+SD5*Q1
	vfmadd231pd %zmm19,%zmm12,%zmm0 # G1R+SD6*Q1
	
	vmovapd %zmm5,%zmm15
	vfmadd231pd %zmm14,%zmm4,%zmm15	
	vfmadd231pd %zmm15,%zmm13,%zmm12 # Q1
	
	vfmadd231pd %zmm20,%zmm13,%zmm1	# G2R+SD7*Q2
	vfmadd231pd %zmm21,%zmm13,%zmm0 # G1R+SD8*Q2
	
	vfmadd231pd %zmm6,%zmm12,%zmm1 # G2R+SD9*Q1
	vfmadd231pd %zmm7,%zmm12,%zmm0 # G1R+SD10*Q1

	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L90

	ret

# ILEV=8 case
L81:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 
	vbroadcastsd 32(%rsi),%zmm4 # AC5 
	vbroadcastsd 40(%rsi),%zmm5 # AC6 

	vbroadcastsd   (%rdx),%zmm3 # SD1	
	vbroadcastsd  8(%rdx),%zmm2 # SD2

	vbroadcastsd 16(%rdx),%zmm7 # SD3
	vbroadcastsd 24(%rdx),%zmm17 # SD4
	vbroadcastsd 32(%rdx),%zmm18 # SD5
	vbroadcastsd 40(%rdx),%zmm19 # SD6
	vbroadcastsd 48(%rdx),%zmm20 # SD7
	vbroadcastsd 56(%rdx),%zmm21 # SD8

	vbroadcastsd 64(%rdx),%zmm6 # SD9
	
	movq %rcx,%rax
L80:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1
	
	vmovapd %zmm8,%zmm15
	vfmadd213pd %zmm9,%zmm14,%zmm15	
	vfmadd231pd %zmm13,%zmm15,%zmm12 # Q1

	vfmadd231pd %zmm7,%zmm13,%zmm1	# G2R+SD3*Q2	
	vfmadd231pd %zmm17,%zmm13,%zmm0 # G1R+SD4*Q2
	
	vmovapd %zmm11,%zmm15
	vfmadd231pd %zmm14,%zmm10,%zmm15	
	vfmadd231pd %zmm15,%zmm12,%zmm13 # Q2
	
	vfmadd231pd %zmm18,%zmm12,%zmm1	# G2R+SD5*Q1
	vfmadd231pd %zmm19,%zmm12,%zmm0 # G1R+SD6*Q1
	
	vmovapd %zmm5,%zmm15
	vfmadd231pd %zmm14,%zmm4,%zmm15	
	vfmadd231pd %zmm15,%zmm13,%zmm12 # Q1
	
	vfmadd231pd %zmm20,%zmm13,%zmm1	# G2R+SD7*Q2	
	vfmadd231pd %zmm21,%zmm13,%zmm0 # G1R+SD8*Q2
	
	vfmadd231pd %zmm6,%zmm12,%zmm1 # G2R+SD9*Q1

	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L80

	ret
	
# ILEV=7 case
L71:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 

	vbroadcastsd   (%rdx),%zmm3 # SD1	
	vbroadcastsd  8(%rdx),%zmm2 # SD2
	vbroadcastsd 16(%rdx),%zmm7 # SD3
	vbroadcastsd 24(%rdx),%zmm6 # SD4
	vbroadcastsd 32(%rdx),%zmm4 # SD5
	vbroadcastsd 40(%rdx),%zmm5 # SD6

	vbroadcastsd 48(%rdx),%zmm20 # SD7
	vbroadcastsd 56(%rdx),%zmm21 # SD8
	
	movq %rcx,%rax
L70:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1
	
	vmovapd %zmm8,%zmm15
	vfmadd213pd %zmm9,%zmm14,%zmm15	
	vfmadd231pd %zmm13,%zmm15,%zmm12 # Q1

	vfmadd231pd %zmm7,%zmm13,%zmm1	# G2R+SD3*Q2	
	vfmadd231pd %zmm6,%zmm13,%zmm0 # G1R+SD4*Q2	

	vmovapd %zmm11,%zmm15
	vfmadd231pd %zmm14,%zmm10,%zmm15	
	vfmadd231pd %zmm15,%zmm12,%zmm13 # Q2
	
	vfmadd231pd %zmm4,%zmm12,%zmm1	# G2R+SD5*Q1	
	vfmadd231pd %zmm5,%zmm12,%zmm0 # G1R+SD6*Q1
	
	vfmadd231pd %zmm20,%zmm13,%zmm1	# G2R+SD7*Q2	
	vfmadd231pd %zmm21,%zmm13,%zmm0 # G1R+SD8*Q2
	
	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L70

	ret
	
# ILEV=6 case
L61:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 

	vbroadcastsd   (%rdx),%zmm3 # SD1
	vbroadcastsd  8(%rdx),%zmm2 # SD2
	vbroadcastsd 16(%rdx),%zmm7 # SD3
	vbroadcastsd 24(%rdx),%zmm6 # SD4
	vbroadcastsd 32(%rdx),%zmm4 # SD5
	vbroadcastsd 40(%rdx),%zmm5 # SD6

	vbroadcastsd 48(%rdx),%zmm20 # SD7	
	
	movq %rcx,%rax
L60:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1
	
	vmovapd %zmm8,%zmm15
	vfmadd213pd %zmm9,%zmm14,%zmm15	
	vfmadd231pd %zmm13,%zmm15,%zmm12 # Q1

	vfmadd231pd %zmm7,%zmm13,%zmm1	# G2R+SD3*Q2	
	vfmadd231pd %zmm6,%zmm13,%zmm0 # G1R+SD4*Q2	

	vmovapd %zmm11,%zmm15
	vfmadd231pd %zmm14,%zmm10,%zmm15	
	vfmadd231pd %zmm15,%zmm12,%zmm13 # Q2
	
	vfmadd231pd %zmm4,%zmm12,%zmm1	# G2R+SD5*Q1	
	vfmadd231pd %zmm5,%zmm12,%zmm0 # G1R+SD6*Q1
	
	vfmadd231pd %zmm20,%zmm13,%zmm1	# G2R+SD7*Q2
	
	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L60

	ret
	
# ILEV=5 case
L51:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 

	vbroadcastsd  8(%rdx),%zmm2 # SD2
	vbroadcastsd   (%rdx),%zmm3 # SD1
	vbroadcastsd 16(%rdx),%zmm7 # SD3
	vbroadcastsd 24(%rdx),%zmm6 # SD4
	vbroadcastsd 32(%rdx),%zmm4 # SD5
	vbroadcastsd 40(%rdx),%zmm5 # SD6
	
	movq %rcx,%rax
L50:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1
	
	vmovapd %zmm8,%zmm15
	vfmadd213pd %zmm9,%zmm14,%zmm15	
	vfmadd231pd %zmm13,%zmm15,%zmm12 # Q1

	vfmadd231pd %zmm7,%zmm13,%zmm1	# G2R+SD3*Q2	
	vfmadd231pd %zmm6,%zmm13,%zmm0 # G1R+SD4*Q2	

	vfmadd231pd %zmm4,%zmm12,%zmm1	# G2R+SD5*Q1	
	vfmadd231pd %zmm5,%zmm12,%zmm0 # G1R+SD6*Q1
	
	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L50

	ret
	
# ILEV=4 case
L41:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 

	vbroadcastsd  8(%rdx),%zmm2 # SD2
	vbroadcastsd   (%rdx),%zmm3 # SD1
	vbroadcastsd 16(%rdx),%zmm7 # SD3
	vbroadcastsd 24(%rdx),%zmm6 # SD4
	vbroadcastsd 32(%rdx),%zmm4 # SD5
	
	movq %rcx,%rax
L40:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1
	
	vmovapd %zmm8,%zmm15
	vfmadd213pd %zmm9,%zmm14,%zmm15	
	vfmadd231pd %zmm13,%zmm15,%zmm12 # Q1

	vfmadd231pd %zmm7,%zmm13,%zmm1	# G2R+SD3*Q2	
	vfmadd231pd %zmm6,%zmm13,%zmm0 # G1R+SD4*Q2	

	vfmadd231pd %zmm4,%zmm12,%zmm1	# G2R+SD5*Q1	
	
	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L40

	ret
	
# ILEV=3 case
L31:
	vbroadcastsd  8(%rdx),%zmm2 # SD2
	vbroadcastsd   (%rdx),%zmm3 # SD1
	vbroadcastsd 16(%rdx),%zmm7 # SD3
	vbroadcastsd 24(%rdx),%zmm6 # SD4
	
	movq %rcx,%rax
L30:
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1

	vfmadd231pd %zmm7,%zmm13,%zmm1	# G2R+SD3*Q2	
	vfmadd231pd %zmm6,%zmm13,%zmm0 # G1R+SD4*Q2	
	
	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L30

	ret

# ILEV=2 case
L21:
	vbroadcastsd  8(%rdx),%zmm2 # SD2
	vbroadcastsd   (%rdx),%zmm3 # SD1
	vbroadcastsd 16(%rdx),%zmm7 # SD3
	
	movq %rcx,%rax
L20:
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm0
	vfmadd231pd %zmm2,%zmm12,%zmm0 # G1R+SD2*Q1
	vmovapd 256(%rax),%zmm1	
	vfmadd231pd %zmm3,%zmm12,%zmm1 # G2R+SD1*Q1

	vfmadd231pd %zmm7,%zmm13,%zmm1	# G2R+SD3*Q2	
	
	vmovapd %zmm0,192(%rax) # G1R		
	vmovapd %zmm1,256(%rax) # G2R	
	addq $320,%rax
	cmpq %rax,%r10
	jne L20

	ret
