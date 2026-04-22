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
.globl lxqszg_
.globl _lxqszg_	
lxqszg_:
_lxqszg_:	
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

	shlq $5,%rdi # JB*8*4 が rsi に
	movq %rdi,%r10	
	shlq $2,%r10
	addq %rdi,%r10	# r10 に JB*8*4*5 が入る
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
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3 
	vbroadcastsd 24(%rsi),%ymm11 # AC4 
	vbroadcastsd 32(%rsi),%ymm4 # AC5 
	vbroadcastsd 40(%rsi),%ymm5 # AC6 
	vbroadcastsd 48(%rsi),%ymm6 # AC7
	vbroadcastsd 56(%rsi),%ymm7 # AC8

	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	
	movq %rcx,%rax
L00:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1
	
	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15	
	vfmadd231pd %ymm13,%ymm15,%ymm12 # Q1

	vbroadcastsd 16(%rdx),%ymm15 # SD3
	vfmadd231pd %ymm15,%ymm13,%ymm1	# G2R+SD3*Q2
	vbroadcastsd 24(%rdx),%ymm15 # SD4
	vfmadd231pd %ymm15,%ymm13,%ymm0 # G1R+SD4*Q2

	vmovapd %ymm11,%ymm15
	vfmadd231pd %ymm14,%ymm10,%ymm15	
	vfmadd231pd %ymm15,%ymm12,%ymm13 # Q2
	
	vbroadcastsd 32(%rdx),%ymm15 # SD5
	vfmadd231pd %ymm15,%ymm12,%ymm1	# G2R+SD5*Q1
	vbroadcastsd 40(%rdx),%ymm15 # SD6
	vfmadd231pd %ymm15,%ymm12,%ymm0 # G1R+SD6*Q1

	vmovapd %ymm5,%ymm15
	vfmadd231pd %ymm14,%ymm4,%ymm15	
	vfmadd231pd %ymm15,%ymm13,%ymm12 # Q1
	
	vbroadcastsd 48(%rdx),%ymm15 # SD7
	vfmadd231pd %ymm15,%ymm13,%ymm1	# G2R+SD7*Q2
	vbroadcastsd 56(%rdx),%ymm15 # SD8
	vfmadd231pd %ymm15,%ymm13,%ymm0 # G1R+SD8*Q2

	vmovapd %ymm7,%ymm15
	vfmadd231pd %ymm14,%ymm6,%ymm15
	vfmadd231pd %ymm15,%ymm12,%ymm13 # Q2
	
	vmovapd %ymm12,32(%rax) # Q1					
	vmovapd %ymm13,64(%rax) # Q2
	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L00

	ret

# ILEV=9 case
L91:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3 
	vbroadcastsd 24(%rsi),%ymm11 # AC4 
	vbroadcastsd 32(%rsi),%ymm4 # AC5 
	vbroadcastsd 40(%rsi),%ymm5 # AC6 

	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 64(%rdx),%ymm6 # SD9
	vbroadcastsd 72(%rdx),%ymm7 # SD10
	
	movq %rcx,%rax
L90:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1
	
	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15	
	vfmadd231pd %ymm13,%ymm15,%ymm12 # Q1

	vbroadcastsd 16(%rdx),%ymm15 # SD3
	vfmadd231pd %ymm15,%ymm13,%ymm1	# G2R+SD3*Q2
	vbroadcastsd 24(%rdx),%ymm15 # SD4
	vfmadd231pd %ymm15,%ymm13,%ymm0 # G1R+SD4*Q2

	vmovapd %ymm11,%ymm15
	vfmadd231pd %ymm14,%ymm10,%ymm15	
	vfmadd231pd %ymm15,%ymm12,%ymm13 # Q2
	
	vbroadcastsd 32(%rdx),%ymm15 # SD5
	vfmadd231pd %ymm15,%ymm12,%ymm1	# G2R+SD5*Q1
	vbroadcastsd 40(%rdx),%ymm15 # SD6
	vfmadd231pd %ymm15,%ymm12,%ymm0 # G1R+SD6*Q1

	vmovapd %ymm5,%ymm15
	vfmadd231pd %ymm14,%ymm4,%ymm15	
	vfmadd231pd %ymm15,%ymm13,%ymm12 # Q1
	
	vbroadcastsd 48(%rdx),%ymm15 # SD7
	vfmadd231pd %ymm15,%ymm13,%ymm1	# G2R+SD7*Q2
	vbroadcastsd 56(%rdx),%ymm15 # SD8
	vfmadd231pd %ymm15,%ymm13,%ymm0 # G1R+SD8*Q2

	vfmadd231pd %ymm6,%ymm12,%ymm1 # G2R+SD9*Q1
	vfmadd231pd %ymm7,%ymm12,%ymm0 # G1R+SD10*Q1

	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L90

	ret

# ILEV=8 case
L81:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3 
	vbroadcastsd 24(%rsi),%ymm11 # AC4 
	vbroadcastsd 32(%rsi),%ymm4 # AC5 
	vbroadcastsd 40(%rsi),%ymm5 # AC6 

	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 16(%rdx),%ymm7 # SD3
	vbroadcastsd 64(%rdx),%ymm6 # SD9
	
	movq %rcx,%rax
L80:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1
	
	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15	
	vfmadd231pd %ymm13,%ymm15,%ymm12 # Q1

	vfmadd231pd %ymm7,%ymm13,%ymm1	# G2R+SD3*Q2	
	vbroadcastsd 24(%rdx),%ymm15 # SD4
	vfmadd231pd %ymm15,%ymm13,%ymm0 # G1R+SD4*Q2

	vmovapd %ymm11,%ymm15
	vfmadd231pd %ymm14,%ymm10,%ymm15	
	vfmadd231pd %ymm15,%ymm12,%ymm13 # Q2
	
	vbroadcastsd 32(%rdx),%ymm15 # SD5
	vfmadd231pd %ymm15,%ymm12,%ymm1	# G2R+SD5*Q1
	vbroadcastsd 40(%rdx),%ymm15 # SD6
	vfmadd231pd %ymm15,%ymm12,%ymm0 # G1R+SD6*Q1

	vmovapd %ymm5,%ymm15
	vfmadd231pd %ymm14,%ymm4,%ymm15	
	vfmadd231pd %ymm15,%ymm13,%ymm12 # Q1
	
	vbroadcastsd 48(%rdx),%ymm15 # SD7
	vfmadd231pd %ymm15,%ymm13,%ymm1	# G2R+SD7*Q2
	vbroadcastsd 56(%rdx),%ymm15 # SD8
	vfmadd231pd %ymm15,%ymm13,%ymm0 # G1R+SD8*Q2

	vfmadd231pd %ymm6,%ymm12,%ymm1 # G2R+SD9*Q1

	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L80

	ret
	
# ILEV=7 case
L71:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3 
	vbroadcastsd 24(%rsi),%ymm11 # AC4 

	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 16(%rdx),%ymm7 # SD3
	vbroadcastsd 24(%rdx),%ymm6 # SD4
	vbroadcastsd 32(%rdx),%ymm4 # SD5
	vbroadcastsd 40(%rdx),%ymm5 # SD6
	
	movq %rcx,%rax
L70:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1
	
	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15	
	vfmadd231pd %ymm13,%ymm15,%ymm12 # Q1

	vfmadd231pd %ymm7,%ymm13,%ymm1	# G2R+SD3*Q2	
	vfmadd231pd %ymm6,%ymm13,%ymm0 # G1R+SD4*Q2	

	vmovapd %ymm11,%ymm15
	vfmadd231pd %ymm14,%ymm10,%ymm15	
	vfmadd231pd %ymm15,%ymm12,%ymm13 # Q2
	
	vfmadd231pd %ymm4,%ymm12,%ymm1	# G2R+SD5*Q1	
	vfmadd231pd %ymm5,%ymm12,%ymm0 # G1R+SD6*Q1
	
	vbroadcastsd 48(%rdx),%ymm15 # SD7
	vfmadd231pd %ymm15,%ymm13,%ymm1	# G2R+SD7*Q2
	vbroadcastsd 56(%rdx),%ymm15 # SD8
	vfmadd231pd %ymm15,%ymm13,%ymm0 # G1R+SD8*Q2

	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L70

	ret
	
# ILEV=6 case
L61:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3 
	vbroadcastsd 24(%rsi),%ymm11 # AC4 

	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 16(%rdx),%ymm7 # SD3
	vbroadcastsd 24(%rdx),%ymm6 # SD4
	vbroadcastsd 32(%rdx),%ymm4 # SD5
	vbroadcastsd 40(%rdx),%ymm5 # SD6
	
	movq %rcx,%rax
L60:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1
	
	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15	
	vfmadd231pd %ymm13,%ymm15,%ymm12 # Q1

	vfmadd231pd %ymm7,%ymm13,%ymm1	# G2R+SD3*Q2	
	vfmadd231pd %ymm6,%ymm13,%ymm0 # G1R+SD4*Q2	

	vmovapd %ymm11,%ymm15
	vfmadd231pd %ymm14,%ymm10,%ymm15	
	vfmadd231pd %ymm15,%ymm12,%ymm13 # Q2
	
	vfmadd231pd %ymm4,%ymm12,%ymm1	# G2R+SD5*Q1	
	vfmadd231pd %ymm5,%ymm12,%ymm0 # G1R+SD6*Q1
	
	vbroadcastsd 48(%rdx),%ymm15 # SD7
	vfmadd231pd %ymm15,%ymm13,%ymm1	# G2R+SD7*Q2

	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L60

	ret
	
# ILEV=5 case
L51:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 16(%rdx),%ymm7 # SD3
	vbroadcastsd 24(%rdx),%ymm6 # SD4
	vbroadcastsd 32(%rdx),%ymm4 # SD5
	vbroadcastsd 40(%rdx),%ymm5 # SD6
	
	movq %rcx,%rax
L50:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1
	
	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15	
	vfmadd231pd %ymm13,%ymm15,%ymm12 # Q1

	vfmadd231pd %ymm7,%ymm13,%ymm1	# G2R+SD3*Q2	
	vfmadd231pd %ymm6,%ymm13,%ymm0 # G1R+SD4*Q2	

	vfmadd231pd %ymm4,%ymm12,%ymm1	# G2R+SD5*Q1	
	vfmadd231pd %ymm5,%ymm12,%ymm0 # G1R+SD6*Q1
	
	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L50

	ret
	
# ILEV=4 case
L41:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 16(%rdx),%ymm7 # SD3
	vbroadcastsd 24(%rdx),%ymm6 # SD4
	vbroadcastsd 32(%rdx),%ymm4 # SD5
	
	movq %rcx,%rax
L40:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1
	
	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15	
	vfmadd231pd %ymm13,%ymm15,%ymm12 # Q1

	vfmadd231pd %ymm7,%ymm13,%ymm1	# G2R+SD3*Q2	
	vfmadd231pd %ymm6,%ymm13,%ymm0 # G1R+SD4*Q2	

	vfmadd231pd %ymm4,%ymm12,%ymm1	# G2R+SD5*Q1	
	
	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L40

	ret
	
# ILEV=3 case
L31:
	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 16(%rdx),%ymm7 # SD3
	vbroadcastsd 24(%rdx),%ymm6 # SD4
	
	movq %rcx,%rax
L30:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1

	vfmadd231pd %ymm7,%ymm13,%ymm1	# G2R+SD3*Q2	
	vfmadd231pd %ymm6,%ymm13,%ymm0 # G1R+SD4*Q2	
	
	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L30

	ret

# ILEV=2 case
L21:
	vbroadcastsd  8(%rdx),%ymm2 # SD2
	vbroadcastsd   (%rdx),%ymm3 # SD1
	vbroadcastsd 16(%rdx),%ymm7 # SD3
	
	movq %rcx,%rax
L20:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm0
	vfmadd231pd %ymm2,%ymm12,%ymm0 # G1R+SD2*Q1
	vmovapd 128(%rax),%ymm1	
	vfmadd231pd %ymm3,%ymm12,%ymm1 # G2R+SD1*Q1

	vfmadd231pd %ymm7,%ymm13,%ymm1	# G2R+SD3*Q2	
	
	vmovapd %ymm0,96(%rax) # G1R		
	vmovapd %ymm1,128(%rax) # G2R	
	addq $160,%rax
	cmpq %rax,%r10
	jne L20

	ret
