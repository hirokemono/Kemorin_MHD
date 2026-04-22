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
.globl lxqswg_
.globl _lxqswg_	
lxqswg_:
_lxqswg_:	
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

	shlq $5,%rdi # JB*8*4 が rdi に
	movq %rdi,%r10
	shlq $3,%r10	
	subq %rdi,%r10	# r10 に JB*8*4*7 が入る
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
	vbroadcastsd 32(%rsi),%ymm7 # AC5 
	vbroadcastsd 48(%rsi),%ymm6 # AC7
	vbroadcastsd 56(%rsi),%ymm11 # AC8

	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I
	
	movq %rcx,%rax
L00:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm9,%ymm0	
	vfmadd231pd %ymm14,%ymm8,%ymm0
	vfmadd213pd %ymm12,%ymm13,%ymm0	# Q1new

	vbroadcastsd 24(%rsi),%ymm1
	vfmadd231pd %ymm14,%ymm10,%ymm1
	vfmadd213pd %ymm13,%ymm0,%ymm1 # Q2new

	vbroadcastsd 40(%rsi),%ymm15
	vfmadd231pd %ymm14,%ymm7,%ymm15
	vfmadd213pd %ymm0,%ymm1,%ymm15	# Q1new2

	vfmadd213pd %ymm11,%ymm6,%ymm14
	vfmadd213pd %ymm1,%ymm15,%ymm14 # Q2new2

	vmovapd %ymm15,32(%rax) # Q1					
	vmovapd %ymm14,64(%rax) # Q2

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vbroadcastsd  48(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  80(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  112(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vbroadcastsd  32(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  64(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  96(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vbroadcastsd  56(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  88(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  120(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vbroadcastsd  40(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  72(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  104(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L00

	ret

# ILEV=9 case
L91:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3
	vbroadcastsd 24(%rsi),%ymm6 # AC4
	vbroadcastsd 32(%rsi),%ymm7 # AC5 

	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I
	
	movq %rcx,%rax
L90:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm9,%ymm0	
	vfmadd231pd %ymm14,%ymm8,%ymm0
	vfmadd213pd %ymm12,%ymm13,%ymm0	# Q1new

	vmovapd %ymm6,%ymm1	
	vfmadd231pd %ymm14,%ymm10,%ymm1
	vfmadd213pd %ymm13,%ymm0,%ymm1 # Q2new

	vbroadcastsd 40(%rsi),%ymm11
	vfmadd231pd %ymm14,%ymm7,%ymm11
	vfmadd213pd %ymm0,%ymm1,%ymm11	# Q1new2

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vbroadcastsd  48(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  80(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  112(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vbroadcastsd 144(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm11,%ymm14	
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vbroadcastsd  32(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  64(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  96(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vbroadcastsd 128(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm11,%ymm14	
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vbroadcastsd  56(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  88(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  120(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vbroadcastsd 152(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm11,%ymm14	
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vbroadcastsd  40(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  72(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  104(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vbroadcastsd 136(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm11,%ymm14	
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L90

	ret
	
# ILEV=8 case
L81:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3
	vbroadcastsd 24(%rsi),%ymm6 # AC4
	vbroadcastsd 32(%rsi),%ymm7 # AC5 

	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I
	
	movq %rcx,%rax
L80:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm9,%ymm0	
	vfmadd231pd %ymm14,%ymm8,%ymm0
	vfmadd213pd %ymm12,%ymm13,%ymm0	# Q1new

	vmovapd %ymm6,%ymm1	
	vfmadd231pd %ymm14,%ymm10,%ymm1
	vfmadd213pd %ymm13,%ymm0,%ymm1 # Q2new

	vbroadcastsd 40(%rsi),%ymm11
	vfmadd231pd %ymm14,%ymm7,%ymm11
	vfmadd213pd %ymm0,%ymm1,%ymm11	# Q1new2

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vbroadcastsd  48(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  80(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  112(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vbroadcastsd  32(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  64(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  96(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vbroadcastsd 128(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm11,%ymm14	
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vbroadcastsd  56(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  88(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  120(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vbroadcastsd  40(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  72(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  104(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vbroadcastsd 136(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm11,%ymm14	
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L80

	ret
	
# ILEV=7 case
L71:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3
	vbroadcastsd 24(%rsi),%ymm6 # AC4

	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I

	vbroadcastsd 32(%rdx),%ymm7
	vbroadcastsd 40(%rdx),%ymm11

	movq %rcx,%rax
L70:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm9,%ymm0	
	vfmadd231pd %ymm14,%ymm8,%ymm0
	vfmadd213pd %ymm12,%ymm13,%ymm0	# Q1new

	vmovapd %ymm6,%ymm1	
	vfmadd231pd %ymm14,%ymm10,%ymm1
	vfmadd213pd %ymm13,%ymm0,%ymm1 # Q2new

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vbroadcastsd  48(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  80(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  112(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vfmadd231pd %ymm7,%ymm13,%ymm14	
	vbroadcastsd  64(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  96(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vbroadcastsd  56(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  88(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  120(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vfmadd231pd %ymm11,%ymm13,%ymm14	
	vbroadcastsd  72(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  104(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L70

	ret
	
# ILEV=6 case
L61:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3
	vbroadcastsd 24(%rsi),%ymm6 # AC4

	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I

	vbroadcastsd 32(%rdx),%ymm7
	vbroadcastsd 40(%rdx),%ymm11

	movq %rcx,%rax
L60:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm9,%ymm0	
	vfmadd231pd %ymm14,%ymm8,%ymm0
	vfmadd213pd %ymm12,%ymm13,%ymm0	# Q1new

	vmovapd %ymm6,%ymm1	
	vfmadd231pd %ymm14,%ymm10,%ymm1
	vfmadd213pd %ymm13,%ymm0,%ymm1 # Q2new

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vbroadcastsd  48(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  80(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vfmadd231pd %ymm7,%ymm13,%ymm14	
	vbroadcastsd  64(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  96(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vbroadcastsd  56(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm13,%ymm14
	vbroadcastsd  88(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vfmadd231pd %ymm11,%ymm13,%ymm14	
	vbroadcastsd  72(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vbroadcastsd  104(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm1,%ymm14
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L60

	ret
	
# ILEV=5 case
L51:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I

	vbroadcastsd 32(%rdx),%ymm7
	vbroadcastsd 40(%rdx),%ymm11
	vbroadcastsd 48(%rdx),%ymm1
	vbroadcastsd 56(%rdx),%ymm6
	vbroadcastsd 64(%rdx),%ymm10
	
	movq %rcx,%rax
L50:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm9,%ymm0	
	vfmadd231pd %ymm14,%ymm8,%ymm0
	vfmadd213pd %ymm12,%ymm13,%ymm0	# Q1new

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vfmadd231pd %ymm1,%ymm13,%ymm14	
	vbroadcastsd  80(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vfmadd231pd %ymm7,%ymm13,%ymm14
	vfmadd231pd %ymm10,%ymm0,%ymm14		
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vfmadd231pd %ymm6,%ymm13,%ymm14	
	vbroadcastsd  88(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vfmadd231pd %ymm11,%ymm13,%ymm14	
	vbroadcastsd  72(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L50

	ret
	
# ILEV=4 case
L41:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I

	vbroadcastsd 32(%rdx),%ymm7
	vbroadcastsd 40(%rdx),%ymm11
	vbroadcastsd 48(%rdx),%ymm1
	vbroadcastsd 56(%rdx),%ymm6
	vbroadcastsd 64(%rdx),%ymm10

	movq %rcx,%rax
L40:
	vmovapd   (%rax),%ymm14 # X2					
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm9,%ymm0	
	vfmadd231pd %ymm14,%ymm8,%ymm0
	vfmadd213pd %ymm12,%ymm13,%ymm0	# Q1new

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vfmadd231pd %ymm1,%ymm13,%ymm14	
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vfmadd231pd %ymm7,%ymm13,%ymm14	
	vfmadd231pd %ymm10,%ymm0,%ymm14	
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vfmadd231pd %ymm6,%ymm13,%ymm14	
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vfmadd231pd %ymm11,%ymm13,%ymm14	
	vbroadcastsd  72(%rdx),%ymm15
	vfmadd231pd %ymm15,%ymm0,%ymm14
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L40

	ret
	
# ILEV=3 case
L31:
	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I

	vbroadcastsd 32(%rdx),%ymm7
	vbroadcastsd 40(%rdx),%ymm11
	vbroadcastsd 48(%rdx),%ymm1
	vbroadcastsd 56(%rdx),%ymm6

	movq %rcx,%rax
L30:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vfmadd231pd %ymm1,%ymm13,%ymm14	
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vfmadd231pd %ymm7,%ymm13,%ymm14	
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vfmadd231pd %ymm6,%ymm13,%ymm14	
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vfmadd231pd %ymm11,%ymm13,%ymm14	
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L30

	ret
	
# ILEV=2 case
L21:
	vbroadcastsd   (%rdx),%ymm3 # SD1R 	
	vbroadcastsd  8(%rdx),%ymm5 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R
	vbroadcastsd 24(%rdx),%ymm4 # SD2I

	vbroadcastsd 32(%rdx),%ymm7
	vbroadcastsd 40(%rdx),%ymm11

	movq %rcx,%rax
L20:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm2,%ymm12,%ymm14	
	vmovapd %ymm14,96(%rax) # G1R	

	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm3,%ymm12,%ymm14
	vfmadd231pd %ymm7,%ymm13,%ymm14	
	vmovapd %ymm14,128(%rax) # G2R	
	
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm4,%ymm12,%ymm14
	vmovapd %ymm14,160(%rax) # G1I	
		
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm5,%ymm12,%ymm14
	vfmadd231pd %ymm11,%ymm13,%ymm14	
	vmovapd %ymm14,192(%rax) # G2I	

	addq $224,%rax
	cmpq %rax,%r10
	jne L20

	ret
