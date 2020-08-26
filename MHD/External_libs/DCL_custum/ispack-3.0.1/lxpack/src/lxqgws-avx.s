########################################################################
# ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
# Copyright (C) 1998--2019 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
.text
.globl lxqgws_
.globl _lxqgws_	
lxqgws_:
_lxqgws_:	
	movq   (%rdi), %rdi  # : JB �� rdi ��
	movq   (%r8), %r8  # : IL �� r8 ��
	movq   (%r9), %r9  # : ILEV �� r9 ��	
	
	# JB: rdi
        # AC: rsi	
	# SD: rdx	
	# Q: rcx
	# IL: r8
	# ILEV: r9

	subq $1,%r8
	shlq $3,%r8
	addq %r8,%rsi

	shlq $5,%rdi # JB*8*4 �� rsi ��
	movq %rdi,%r10	
	shlq $3,%r10
	subq %rdi,%r10	# r10 �� JB*8*4*7 ������
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
	vzeroall
	vbroadcastsd   (%rsi),%ymm8 # AC5 
	vbroadcastsd  8(%rsi),%ymm9 # AC6 
	vbroadcastsd 16(%rsi),%ymm10 # AC7 
	vbroadcastsd 24(%rsi),%ymm11 # AC8 

	movq %rcx,%rax
LC0:
	vmovapd   (%rax),%ymm14 # X2	
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmulpd %ymm14,%ymm8,%ymm15
	vaddpd %ymm15,%ymm9,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm15
	vmovapd %ymm15,32(%rax) # Q1
	
	vmulpd %ymm14,%ymm10,%ymm14	
	vaddpd %ymm14,%ymm11,%ymm14
	vmulpd %ymm14,%ymm15,%ymm14
	vaddpd %ymm14,%ymm13,%ymm14
	vmovapd %ymm14,64(%rax) # Q2
	
	vmovapd 96(%rax),%ymm14 # G1R
	vmulpd %ymm12,%ymm14,%ymm15 # Q1*G1R
	vaddpd %ymm2,%ymm15,%ymm2 # SD2R=SD2R+Q1*G1R
	vmulpd %ymm13,%ymm14,%ymm15 # Q2*G1R
	vaddpd %ymm6,%ymm15,%ymm6 # SD4R=SD4R+Q2*G1R

	vmovapd 128(%rax),%ymm14 # G2R	
	vmulpd %ymm12,%ymm14,%ymm15 # Q1*G2R
	vaddpd %ymm0,%ymm15,%ymm0 # SD1R=SD1R+Q1*G2R	
	vmulpd %ymm13,%ymm14,%ymm15 # Q2*G2R
	vaddpd %ymm4,%ymm15,%ymm4 # SD3R=SD3R+Q2*G2R
	
	vmovapd 160(%rax),%ymm14 # G1I
	vmulpd %ymm12,%ymm14,%ymm15 # Q1*G1I
	vaddpd %ymm3,%ymm15,%ymm3 # SD2I=SD2I+Q1*G1I	
	vmulpd %ymm13,%ymm14,%ymm15 # Q2*G1I
	vaddpd %ymm7,%ymm15,%ymm7 # SD4I=SD4I+Q2*G1I

	vmovapd 192(%rax),%ymm14 # G2I			
	vmulpd %ymm12,%ymm14,%ymm15 # Q1*G2I
	vaddpd %ymm1,%ymm15,%ymm1 # SD1I=SD1I+Q1*G2I	
	vmulpd %ymm13,%ymm14,%ymm15 # Q2*G2I
	vaddpd %ymm5,%ymm15,%ymm5 # SD3I=SD3I+Q2*G2I
	
	addq $224,%rax
	cmpq %rax,%r10
	jne LC0

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
	vaddsd   (%rdx),%xmm8,%xmm8
	vaddsd  8(%rdx),%xmm9,%xmm9
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
	vmovsd %xmm0,  (%rdx)
	vmovsd %xmm1, 8(%rdx)
	vmovsd %xmm2,16(%rdx)			
	vmovsd %xmm3,24(%rdx)
	vmovsd %xmm4,32(%rdx)
	vmovsd %xmm5,40(%rdx)
	vmovsd %xmm6,48(%rdx)
	vmovsd %xmm7,56(%rdx)				

	ret

# common for case5	
LC5:
	vzeroall

	movq %rcx,%rax
LC50:
	vmovapd   (%rax),%ymm14 # X2	
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vbroadcastsd   (%rsi),%ymm15
	vmulpd %ymm14,%ymm15,%ymm14
	vbroadcastsd  8(%rsi),%ymm15	
	vaddpd %ymm15,%ymm14,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm14 # Q1new

	vmulpd 96(%rax),%ymm12,%ymm15 # Q1*G1R
	vaddpd %ymm2,%ymm15,%ymm2 # SD2R=SD2R+Q1*G1R
	vmulpd 96(%rax),%ymm13,%ymm15 # Q2*G1R
	vaddpd %ymm6,%ymm15,%ymm6 # SD4R=SD4R+Q2*G1R
	vmulpd 96(%rax),%ymm14,%ymm15 # Q1new*G1R
	vaddpd %ymm10,%ymm15,%ymm10 # SD6R=SD6R+Q1new*G1R

	vmulpd 128(%rax),%ymm12,%ymm15 # Q1*G2R
	vaddpd %ymm0,%ymm15,%ymm0 # SD1R=SD1R+Q1*G2R	
	vmulpd 128(%rax),%ymm13,%ymm15 # Q2*G2R
	vaddpd %ymm4,%ymm15,%ymm4 # SD3R=SD3R+Q2*G2R
	vmulpd 128(%rax),%ymm14,%ymm15 # Q1new*G2R
	vaddpd %ymm8,%ymm15,%ymm8 # SD5R=SD5R+Q1new*G2R	
	
	vmulpd 160(%rax),%ymm12,%ymm15 # Q1*G1I
	vaddpd %ymm3,%ymm15,%ymm3 # SD2I=SD2I+Q1*G1I	
	vmulpd 160(%rax),%ymm13,%ymm15 # Q2*G1I
	vaddpd %ymm7,%ymm15,%ymm7 # SD4I=SD4I+Q2*G1I
	vmulpd 160(%rax),%ymm14,%ymm15 # Q1new*G1I
	vaddpd %ymm11,%ymm15,%ymm11 # SD6I=SD6I+Q2*G1I

	vmulpd 192(%rax),%ymm12,%ymm15 # Q1*G2I
	vaddpd %ymm1,%ymm15,%ymm1 # SD1I=SD1I+Q1*G2I	
	vmulpd 192(%rax),%ymm13,%ymm15 # Q2*G2I
	vaddpd %ymm5,%ymm15,%ymm5 # SD3I=SD3I+Q2*G2I
	vmulpd 192(%rax),%ymm14,%ymm15 # Q1new*G2I
	vaddpd %ymm9,%ymm15,%ymm9 # SD5I=SD3I+Q2*G2I
	
	addq $224,%rax
	cmpq %rax,%r10
	jne LC50

	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm15
	vaddsd   (%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm0,%xmm0
	vmovsd %xmm0,  (%rdx)

	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm15
	vaddsd   8(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm1,%xmm1
	vmovsd %xmm1,  8(%rdx)

	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm15
	vaddsd 16(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm15
	vaddsd 24(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)

	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm15
	vaddsd 32(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)

	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm15
	vaddsd 40(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)

	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm15
	vaddsd 48(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm6,%xmm6
	vmovsd %xmm6,48(%rdx)

	vhaddpd %ymm7,%ymm7,%ymm7
	vextractf128 $1,%ymm7,%xmm15
	vaddsd 56(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm7,56(%rdx)

	vhaddpd %ymm8,%ymm8,%ymm8
	vextractf128 $1,%ymm8,%xmm15
	vaddsd 64(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm8,%xmm8
	vmovsd %xmm8,64(%rdx)
	
	vhaddpd %ymm9,%ymm9,%ymm9
	vextractf128 $1,%ymm9,%xmm15
	vaddsd 72(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm9,%xmm9
	vmovsd %xmm9,72(%rdx)

	vhaddpd %ymm10,%ymm10,%ymm10
	vextractf128 $1,%ymm10,%xmm15
	vaddsd 80(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm10,%xmm10
	vmovsd %xmm10,80(%rdx)

	vhaddpd %ymm11,%ymm11,%ymm11
	vextractf128 $1,%ymm11,%xmm15
	vaddsd 88(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm11,%xmm11
	vmovsd %xmm11,88(%rdx)

	ret
# common for case4	
LC4:	
	vzeroall

	vbroadcastsd   (%rsi),%ymm10	
	movq %rcx,%rax
LC40:
	vmovapd   (%rax),%ymm14 # X2	
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmulpd %ymm14,%ymm10,%ymm14
	vbroadcastsd  8(%rsi),%ymm15	
	vaddpd %ymm15,%ymm14,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm14 # Q1new

	vmovapd 96(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G1R
	vaddpd %ymm2,%ymm15,%ymm2 # SD2R=SD2R+Q1*G1R
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G1R
	vaddpd %ymm6,%ymm15,%ymm6 # SD4R=SD4R+Q2*G1R

	vmovapd 128(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G2R
	vaddpd %ymm0,%ymm15,%ymm0 # SD1R=SD1R+Q1*G2R	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G2R
	vaddpd %ymm4,%ymm15,%ymm4 # SD3R=SD3R+Q2*G2R
	vmulpd %ymm11,%ymm14,%ymm15 # Q1new*G2R
	vaddpd %ymm8,%ymm15,%ymm8 # SD5R=SD5R+Q1new*G2R	

	vmovapd 160(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G1I
	vaddpd %ymm3,%ymm15,%ymm3 # SD2I=SD2I+Q1*G1I	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G1I
	vaddpd %ymm7,%ymm15,%ymm7 # SD4I=SD4I+Q2*G1I

	vmovapd 192(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G2I
	vaddpd %ymm1,%ymm15,%ymm1 # SD1I=SD1I+Q1*G2I	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G2I
	vaddpd %ymm5,%ymm15,%ymm5 # SD3I=SD3I+Q2*G2I
	vmulpd %ymm11,%ymm14,%ymm15 # Q1new*G2I
	vaddpd %ymm9,%ymm15,%ymm9 # SD5I=SD3I+Q2*G2I
	
	addq $224,%rax
	cmpq %rax,%r10
	jne LC40

	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm15
	vaddsd   (%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm0,%xmm0
	vmovsd %xmm0,  (%rdx)

	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm15
	vaddsd   8(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm1,%xmm1
	vmovsd %xmm1,  8(%rdx)

	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm15
	vaddsd 16(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm15
	vaddsd 24(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)

	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm15
	vaddsd 32(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)

	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm15
	vaddsd 40(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)

	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm15
	vaddsd 48(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm6,%xmm6
	vmovsd %xmm6,48(%rdx)

	vhaddpd %ymm7,%ymm7,%ymm7
	vextractf128 $1,%ymm7,%xmm15
	vaddsd 56(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm7,56(%rdx)

	vhaddpd %ymm8,%ymm8,%ymm8
	vextractf128 $1,%ymm8,%xmm15
	vaddsd 64(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm8,%xmm8
	vmovsd %xmm8,64(%rdx)
	
	vhaddpd %ymm9,%ymm9,%ymm9
	vextractf128 $1,%ymm9,%xmm15
	vaddsd 72(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm9,%xmm9
	vmovsd %xmm9,72(%rdx)

	ret
# common for case3	
LC3:	
	vzeroall

	movq %rcx,%rax
LC30:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G1R
	vaddpd %ymm2,%ymm15,%ymm2 # SD2R=SD2R+Q1*G1R
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G1R
	vaddpd %ymm6,%ymm15,%ymm6 # SD4R=SD4R+Q2*G1R

	vmovapd 128(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G2R
	vaddpd %ymm0,%ymm15,%ymm0 # SD1R=SD1R+Q1*G2R	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G2R
	vaddpd %ymm4,%ymm15,%ymm4 # SD3R=SD3R+Q2*G2R

	vmovapd 160(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G1I
	vaddpd %ymm3,%ymm15,%ymm3 # SD2I=SD2I+Q1*G1I	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G1I
	vaddpd %ymm7,%ymm15,%ymm7 # SD4I=SD4I+Q2*G1I

	vmovapd 192(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G2I
	vaddpd %ymm1,%ymm15,%ymm1 # SD1I=SD1I+Q1*G2I	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G2I
	vaddpd %ymm5,%ymm15,%ymm5 # SD3I=SD3I+Q2*G2I
	
	addq $224,%rax
	cmpq %rax,%r10
	jne LC30

	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm15
	vaddsd   (%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm0,%xmm0
	vmovsd %xmm0,  (%rdx)

	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm15
	vaddsd   8(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm1,%xmm1
	vmovsd %xmm1,  8(%rdx)

	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm15
	vaddsd 16(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm15
	vaddsd 24(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)

	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm15
	vaddsd 32(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)

	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm15
	vaddsd 40(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)

	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm15
	vaddsd 48(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm6,%xmm6
	vmovsd %xmm6,48(%rdx)

	vhaddpd %ymm7,%ymm7,%ymm7
	vextractf128 $1,%ymm7,%xmm15
	vaddsd 56(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm7,56(%rdx)

	ret
# common for case2	
LC2:	
	vzeroall

	movq %rcx,%rax
LC20:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G1R
	vaddpd %ymm2,%ymm15,%ymm2 # SD2R=SD2R+Q1*G1R

	vmovapd 128(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G2R
	vaddpd %ymm0,%ymm15,%ymm0 # SD1R=SD1R+Q1*G2R	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G2R
	vaddpd %ymm4,%ymm15,%ymm4 # SD3R=SD3R+Q2*G2R

	vmovapd 160(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G1I
	vaddpd %ymm3,%ymm15,%ymm3 # SD2I=SD2I+Q1*G1I	

	vmovapd 192(%rax),%ymm11
	vmulpd %ymm11,%ymm12,%ymm15 # Q1*G2I
	vaddpd %ymm1,%ymm15,%ymm1 # SD1I=SD1I+Q1*G2I	
	vmulpd %ymm11,%ymm13,%ymm15 # Q2*G2I
	vaddpd %ymm5,%ymm15,%ymm5 # SD3I=SD3I+Q2*G2I
	
	addq $224,%rax
	cmpq %rax,%r10
	jne LC20

	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm15
	vaddsd   (%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm0,%xmm0
	vmovsd %xmm0,  (%rdx)

	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm15
	vaddsd   8(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm1,%xmm1
	vmovsd %xmm1,  8(%rdx)

	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm15
	vaddsd 16(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm15
	vaddsd 24(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm3,%xmm3
	vmovsd %xmm3,24(%rdx)

	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm15
	vaddsd 32(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm4,%xmm4
	vmovsd %xmm4,32(%rdx)

	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm15
	vaddsd 40(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm5,%xmm5
	vmovsd %xmm5,40(%rdx)

	ret
