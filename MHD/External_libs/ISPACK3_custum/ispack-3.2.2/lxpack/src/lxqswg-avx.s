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

	shlq $5,%rdi # JB*8*4 が rsi に
	movq %rdi,%r10	
	shlq $3,%r10
	subq %rdi,%r10	# r10 に JB*8*4*7 が入る
	addq %rcx,%r10

	cmpq $0,%r9
	je L0
	
	cmpq $2,%r9
	je L2
	cmpq $3,%r9
	je L3
	cmpq $4,%r9
	je L4
	cmpq $5,%r9
	je L5
	cmpq $6,%r9
	je L6
	cmpq $7,%r9
	je L7
	cmpq $8,%r9
	je L8
	cmpq $9,%r9
	je L9

# ILEV=0 case
L0:
	call LC
	addq $32,%rsi
	addq $64,%rdx
	call LC
	ret

# ILEV=9 case
L9:		
	call LC
	addq $32,%rsi
	addq $64,%rdx
	call LC5
	ret
	
# ILEV=8 case
L8:
	call LC
	addq $32,%rsi
	addq $64,%rdx
	call LC4
	ret
	
# ILEV=7 case
L7:
	call LC
	addq $64,%rdx
	call LC3
	ret
	
# ILEV=6 case
L6:
	call LC
	addq $64,%rdx
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
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 
	vbroadcastsd 16(%rsi),%ymm10 # AC3 
	vbroadcastsd 24(%rsi),%ymm11 # AC4 

	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R 
	vbroadcastsd 24(%rdx),%ymm3 # SD2I
	vbroadcastsd 32(%rdx),%ymm4 # SD3R 
	vbroadcastsd 40(%rdx),%ymm5 # SD3I
	vbroadcastsd 48(%rdx),%ymm6 # SD4R 
	vbroadcastsd 56(%rdx),%ymm7 # SD4I

	movq %rcx,%rax
LC0:
	vmovapd (%rax),%ymm14 # X2
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm14,%ymm8,%ymm15
	vaddpd %ymm15,%ymm9,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm15 # Q1
	vmovapd %ymm15,32(%rax) # Q1
	
	vmulpd %ymm14,%ymm10,%ymm14
	vaddpd %ymm14,%ymm11,%ymm14
	vmulpd %ymm14,%ymm15,%ymm14
	vaddpd %ymm14,%ymm13,%ymm14 # Q2
	vmovapd %ymm14,64(%rax) # Q2

	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD2R
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1		
	vmulpd %ymm6,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2	
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm4,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vmulpd %ymm7,%ymm13,%ymm14 # Q2*SD4I	
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vmulpd %ymm5,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vmovapd %ymm15,192(%rax)

	addq $224,%rax
	cmpq %rax,%r10
	jne LC0
	
	ret

# common for case5	
LC5:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R 
	vbroadcastsd 24(%rdx),%ymm3 # SD2I
	vbroadcastsd 32(%rdx),%ymm4 # SD3R 
	vbroadcastsd 40(%rdx),%ymm5 # SD3I
	vbroadcastsd 48(%rdx),%ymm6 # SD4R 
	vbroadcastsd 56(%rdx),%ymm7 # SD4I

	vbroadcastsd 64(%rdx),%ymm10 # SD5R のみ	
	
	movq %rcx,%rax
LC50:
	vmovapd (%rax),%ymm14 # X2
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm14,%ymm8,%ymm15
	vaddpd %ymm15,%ymm9,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm11 # Q1new

	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD2R
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1		
	vmulpd %ymm6,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2	
	vbroadcastsd 80(%rdx),%ymm14 # SD6R
	vmulpd %ymm14,%ymm11,%ymm14 # Q1new*SD6R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD6R*Q1new
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm4,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmulpd %ymm10,%ymm11,%ymm14 # Q1new*SD5R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD5R*Q1new
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vmulpd %ymm7,%ymm13,%ymm14 # Q2*SD4I	
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vbroadcastsd 88(%rdx),%ymm14 # SD6I	
	vmulpd %ymm14,%ymm11,%ymm14 # Q1new*SD6I
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD6I*Q1new
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vmulpd %ymm5,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vbroadcastsd 72(%rdx),%ymm14 # SD5I		
	vmulpd %ymm14,%ymm11,%ymm14 # Q1new*SD5I
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD5I*Q1new
	vmovapd %ymm15,192(%rax)

	addq $224,%rax
	cmpq %rax,%r10
	jne LC50

	ret
	
# common for case4	
LC4:	
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R 
	vbroadcastsd 24(%rdx),%ymm3 # SD2I
	vbroadcastsd 32(%rdx),%ymm4 # SD3R 
	vbroadcastsd 40(%rdx),%ymm5 # SD3I
	vbroadcastsd 48(%rdx),%ymm6 # SD4R 
	vbroadcastsd 56(%rdx),%ymm7 # SD4I

	vbroadcastsd 64(%rdx),%ymm10 # SD5R のみ	
	
	movq %rcx,%rax
LC40:
	vmovapd (%rax),%ymm14 # X2
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm14,%ymm8,%ymm15
	vaddpd %ymm15,%ymm9,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm11 # Q1new

	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD2R
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1		
	vmulpd %ymm6,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2	
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm4,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmulpd %ymm10,%ymm11,%ymm14 # Q1new*SD5R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD5R*Q1new
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vmulpd %ymm7,%ymm13,%ymm14 # Q2*SD4I	
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vmulpd %ymm5,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vbroadcastsd 72(%rdx),%ymm14 # SD5I		
	vmulpd %ymm14,%ymm11,%ymm14 # Q1new*SD5I
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD5I*Q1new
	vmovapd %ymm15,192(%rax)

	addq $224,%rax
	cmpq %rax,%r10
	jne LC40

	ret
	
# common for case3	
LC3:	
	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R 
	vbroadcastsd 24(%rdx),%ymm3 # SD2I
	vbroadcastsd 32(%rdx),%ymm4 # SD3R 
	vbroadcastsd 40(%rdx),%ymm5 # SD3I
	vbroadcastsd 48(%rdx),%ymm6 # SD4R 
	vbroadcastsd 56(%rdx),%ymm7 # SD4I
	
	movq %rcx,%rax
LC30:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD2R
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1		
	vmulpd %ymm6,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2	
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm4,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vmulpd %ymm7,%ymm13,%ymm14 # Q2*SD4I	
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vmulpd %ymm5,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vmovapd %ymm15,192(%rax)

	addq $224,%rax
	cmpq %rax,%r10
	jne LC30

	ret

# common for case2	
LC2:	
	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD2R 
	vbroadcastsd 24(%rdx),%ymm3 # SD2I
	vbroadcastsd 32(%rdx),%ymm4 # SD3R 
	vbroadcastsd 40(%rdx),%ymm5 # SD3I
	
	movq %rcx,%rax
LC20:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD2R
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1		
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm4,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vmulpd %ymm5,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vbroadcastsd 72(%rdx),%ymm14 # SD5I		
	vmovapd %ymm15,192(%rax)

	addq $224,%rax
	cmpq %rax,%r10
	jne LC20

	ret
	
