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
.globl lxqswv_
.globl _lxqswv_	
lxqswv_:
_lxqswv_:	
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
	movq %rdi,%r11	
	shlq $3,%r10
	shlq $2,%r11
	addq %r11,%r10
	subq %rdi,%r10	# r10 に JB*8*4*11 が入る
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
	addq $128,%rdx
	call LC
	ret

# ILEV=9 case
L9:		
	call LC
	addq $32,%rsi
	addq $128,%rdx
	call LC5
	ret
	
# ILEV=8 case
L8:
	call LC
	addq $32,%rsi
	addq $128,%rdx
	call LC4
	ret
	
# ILEV=7 case
L7:
	call LC
	addq $128,%rdx
	call LC3
	ret
	
# ILEV=6 case
L6:
	call LC
	addq $128,%rdx
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
	vbroadcastsd 16(%rdx),%ymm2 # SD1A 
	vbroadcastsd 24(%rdx),%ymm3 # SD1B
	vbroadcastsd 32(%rdx),%ymm4 # SD2R 
	vbroadcastsd 40(%rdx),%ymm5 # SD2I
	vbroadcastsd 48(%rdx),%ymm6 # SD2A 
	vbroadcastsd 56(%rdx),%ymm7 # SD2B
	
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

	vmulpd %ymm4,%ymm12,%ymm15 # Q1*SD2R	
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1
	vbroadcastsd 96(%rdx),%ymm14 # SD4R
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vbroadcastsd 64(%rdx),%ymm14 # SD3R
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3R
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm5,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vbroadcastsd 104(%rdx),%ymm14 # SD4I
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4I
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vbroadcastsd 72(%rdx),%ymm14 # SD3I
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vmovapd %ymm15,192(%rax)

	vmulpd %ymm6,%ymm12,%ymm15 # Q1*SD2A
	vaddpd 224(%rax),%ymm15,%ymm15 # G1A+SD2A*Q1
	vbroadcastsd 112(%rdx),%ymm14 # SD4A
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4A
	vaddpd %ymm14,%ymm15,%ymm15 # G1A+SD4A*Q2
	vmovapd %ymm15,224(%rax)
	
	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD1A
	vaddpd 256(%rax),%ymm15,%ymm15 # G2A+SD1A*Q1
	vbroadcastsd 80(%rdx),%ymm14 # SD3A
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3A
	vaddpd %ymm14,%ymm15,%ymm15 # G2A+SD3A*Q2
	vmovapd %ymm15,256(%rax)

	vmulpd %ymm7,%ymm12,%ymm15 # Q1*SD2B
	vaddpd 288(%rax),%ymm15,%ymm15 # G1B+SD2B*Q1
	vbroadcastsd 120(%rdx),%ymm14 # SD4B
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4B
	vaddpd %ymm14,%ymm15,%ymm15 # G1B+SD4B*Q2
	vmovapd %ymm15,288(%rax)
	
	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD1B
	vaddpd 320(%rax),%ymm15,%ymm15 # G2B+SD1B*Q1
	vbroadcastsd 88(%rdx),%ymm14 # SD3B
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3B
	vaddpd %ymm14,%ymm15,%ymm15 # G2B+SD3B*Q2
	vmovapd %ymm15,320(%rax)

	addq $352,%rax
	cmpq %rax,%r10
	jne LC0
	
	ret

# common for case5	
LC5:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD1A 
	vbroadcastsd 24(%rdx),%ymm3 # SD1B
	vbroadcastsd 32(%rdx),%ymm4 # SD2R 
	vbroadcastsd 40(%rdx),%ymm5 # SD2I
	vbroadcastsd 48(%rdx),%ymm6 # SD2A 
	vbroadcastsd 56(%rdx),%ymm7 # SD2B

	vbroadcastsd 64(%rdx),%ymm10 # SD3R 	
	
	movq %rcx,%rax
LC50:
	vmovapd (%rax),%ymm14 # X2
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm14,%ymm8,%ymm15
	vaddpd %ymm15,%ymm9,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm11 # Q1new

	vmulpd %ymm4,%ymm12,%ymm15 # Q1*SD2R	
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1
	vbroadcastsd 96(%rdx),%ymm14 # SD4R
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2
	vbroadcastsd 160(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm10,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vbroadcastsd 128(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm5,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vbroadcastsd 104(%rdx),%ymm14 # SD4I
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4I
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vbroadcastsd 168(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vbroadcastsd 72(%rdx),%ymm14 # SD3I
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vbroadcastsd 136(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,192(%rax)

	vmulpd %ymm6,%ymm12,%ymm15 # Q1*SD2A
	vaddpd 224(%rax),%ymm15,%ymm15 # G1A+SD2A*Q1
	vbroadcastsd 112(%rdx),%ymm14 # SD4A
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4A
	vaddpd %ymm14,%ymm15,%ymm15 # G1A+SD4A*Q2
	vbroadcastsd 176(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,224(%rax)
	
	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD1A
	vaddpd 256(%rax),%ymm15,%ymm15 # G2A+SD1A*Q1
	vbroadcastsd 80(%rdx),%ymm14 # SD3A
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3A
	vaddpd %ymm14,%ymm15,%ymm15 # G2A+SD3A*Q2
	vbroadcastsd 144(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,256(%rax)

	vmulpd %ymm7,%ymm12,%ymm15 # Q1*SD2B
	vaddpd 288(%rax),%ymm15,%ymm15 # G1B+SD2B*Q1
	vbroadcastsd 120(%rdx),%ymm14 # SD4B
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4B
	vaddpd %ymm14,%ymm15,%ymm15 # G1B+SD4B*Q2
	vbroadcastsd 184(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,288(%rax)
	
	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD1B
	vaddpd 320(%rax),%ymm15,%ymm15 # G2B+SD1B*Q1
	vbroadcastsd 88(%rdx),%ymm14 # SD3B
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3B
	vaddpd %ymm14,%ymm15,%ymm15 # G2B+SD3B*Q2
	vbroadcastsd 152(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,320(%rax)

	addq $352,%rax
	cmpq %rax,%r10
	jne LC50
	
	ret
	
# common for case4
LC4:
	vbroadcastsd   (%rsi),%ymm8  # AC1 
	vbroadcastsd  8(%rsi),%ymm9  # AC2 

	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD1A 
	vbroadcastsd 24(%rdx),%ymm3 # SD1B
	vbroadcastsd 32(%rdx),%ymm4 # SD2R 
	vbroadcastsd 40(%rdx),%ymm5 # SD2I
	vbroadcastsd 48(%rdx),%ymm6 # SD2A 
	vbroadcastsd 56(%rdx),%ymm7 # SD2B

	vbroadcastsd 64(%rdx),%ymm10 # SD3R 	
	
	movq %rcx,%rax
LC40:
	vmovapd (%rax),%ymm14 # X2
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm14,%ymm8,%ymm15
	vaddpd %ymm15,%ymm9,%ymm15
	vmulpd %ymm15,%ymm13,%ymm15
	vaddpd %ymm15,%ymm12,%ymm11 # Q1new

	vmulpd %ymm4,%ymm12,%ymm15 # Q1*SD2R	
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1
	vbroadcastsd 96(%rdx),%ymm14 # SD4R
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm10,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vbroadcastsd 128(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm5,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vbroadcastsd 104(%rdx),%ymm14 # SD4I
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4I
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vbroadcastsd 72(%rdx),%ymm14 # SD3I
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3I	
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vbroadcastsd 136(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,192(%rax)

	vmulpd %ymm6,%ymm12,%ymm15 # Q1*SD2A
	vaddpd 224(%rax),%ymm15,%ymm15 # G1A+SD2A*Q1
	vbroadcastsd 112(%rdx),%ymm14 # SD4A
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4A
	vaddpd %ymm14,%ymm15,%ymm15 # G1A+SD4A*Q2
	vmovapd %ymm15,224(%rax)
	
	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD1A
	vaddpd 256(%rax),%ymm15,%ymm15 # G2A+SD1A*Q1
	vbroadcastsd 80(%rdx),%ymm14 # SD3A
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3A
	vaddpd %ymm14,%ymm15,%ymm15 # G2A+SD3A*Q2
	vbroadcastsd 144(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,256(%rax)

	vmulpd %ymm7,%ymm12,%ymm15 # Q1*SD2B
	vaddpd 288(%rax),%ymm15,%ymm15 # G1B+SD2B*Q1
	vbroadcastsd 120(%rdx),%ymm14 # SD4B
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4B
	vaddpd %ymm14,%ymm15,%ymm15 # G1B+SD4B*Q2
	vmovapd %ymm15,288(%rax)
	
	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD1B
	vaddpd 320(%rax),%ymm15,%ymm15 # G2B+SD1B*Q1
	vbroadcastsd 88(%rdx),%ymm14 # SD3B
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD3B
	vaddpd %ymm14,%ymm15,%ymm15 # G2B+SD3B*Q2
	vbroadcastsd 152(%rdx),%ymm14
	vmulpd %ymm14,%ymm11,%ymm14
	vaddpd %ymm14,%ymm15,%ymm15
	vmovapd %ymm15,320(%rax)

	addq $352,%rax
	cmpq %rax,%r10
	jne LC40
	
	ret
	
# common for case3
LC3:
	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD1A 
	vbroadcastsd 24(%rdx),%ymm3 # SD1B
	vbroadcastsd 32(%rdx),%ymm4 # SD2R 
	vbroadcastsd 40(%rdx),%ymm5 # SD2I
	vbroadcastsd 48(%rdx),%ymm6 # SD2A 
	vbroadcastsd 56(%rdx),%ymm7 # SD2B

	vbroadcastsd 64(%rdx),%ymm8 # SD3R
	vbroadcastsd 72(%rdx),%ymm9 # SD3I
	vbroadcastsd 80(%rdx),%ymm10 # SD3A
	vbroadcastsd 88(%rdx),%ymm11 # SD3B	
	
	movq %rcx,%rax
LC30:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm4,%ymm12,%ymm15 # Q1*SD2R	
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1
	vbroadcastsd 96(%rdx),%ymm14 # SD4R
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4R
	vaddpd %ymm14,%ymm15,%ymm15 # G1R+SD4R*Q2
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm8,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm5,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vbroadcastsd 104(%rdx),%ymm14 # SD4I
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4I
	vaddpd %ymm14,%ymm15,%ymm15 # G1I+SD4I*Q2
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vmulpd %ymm9,%ymm13,%ymm14 # Q2*SD3I		
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vmovapd %ymm15,192(%rax)

	vmulpd %ymm6,%ymm12,%ymm15 # Q1*SD2A
	vaddpd 224(%rax),%ymm15,%ymm15 # G1A+SD2A*Q1
	vbroadcastsd 112(%rdx),%ymm14 # SD4A
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4A
	vaddpd %ymm14,%ymm15,%ymm15 # G1A+SD4A*Q2
	vmovapd %ymm15,224(%rax)
	
	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD1A
	vaddpd 256(%rax),%ymm15,%ymm15 # G2A+SD1A*Q1
	vmulpd %ymm10,%ymm13,%ymm14 # Q2*SD3A	
	vaddpd %ymm14,%ymm15,%ymm15 # G2A+SD3A*Q2
	vmovapd %ymm15,256(%rax)

	vmulpd %ymm7,%ymm12,%ymm15 # Q1*SD2B
	vaddpd 288(%rax),%ymm15,%ymm15 # G1B+SD2B*Q1
	vbroadcastsd 120(%rdx),%ymm14 # SD4B
	vmulpd %ymm14,%ymm13,%ymm14 # Q2*SD4B
	vaddpd %ymm14,%ymm15,%ymm15 # G1B+SD4B*Q2
	vmovapd %ymm15,288(%rax)
	
	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD1B
	vaddpd 320(%rax),%ymm15,%ymm15 # G2B+SD1B*Q1
	vmulpd %ymm11,%ymm13,%ymm14 # Q2*SD3B
	vaddpd %ymm14,%ymm15,%ymm15 # G2B+SD3B*Q2
	vmovapd %ymm15,320(%rax)

	addq $352,%rax
	cmpq %rax,%r10
	jne LC30
	
	ret
	
# common for case3
LC2:
	vbroadcastsd   (%rdx),%ymm0 # SD1R 
	vbroadcastsd  8(%rdx),%ymm1 # SD1I
	vbroadcastsd 16(%rdx),%ymm2 # SD1A 
	vbroadcastsd 24(%rdx),%ymm3 # SD1B
	vbroadcastsd 32(%rdx),%ymm4 # SD2R 
	vbroadcastsd 40(%rdx),%ymm5 # SD2I
	vbroadcastsd 48(%rdx),%ymm6 # SD2A 
	vbroadcastsd 56(%rdx),%ymm7 # SD2B

	vbroadcastsd 64(%rdx),%ymm8 # SD3R
	vbroadcastsd 72(%rdx),%ymm9 # SD3I
	vbroadcastsd 80(%rdx),%ymm10 # SD3A
	vbroadcastsd 88(%rdx),%ymm11 # SD3B	
	
	movq %rcx,%rax
LC20:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2
	
	vmulpd %ymm4,%ymm12,%ymm15 # Q1*SD2R	
	vaddpd 96(%rax),%ymm15,%ymm15 # G1R+SD2R*Q1
	vmovapd %ymm15,96(%rax)

	vmulpd %ymm0,%ymm12,%ymm15 # Q1*SD1R
	vaddpd 128(%rax),%ymm15,%ymm15 # G2R+SD1R*Q1	
	vmulpd %ymm8,%ymm13,%ymm14 # Q2*SD3R	
	vaddpd %ymm14,%ymm15,%ymm15 # G2R+SD3R*Q2
	vmovapd %ymm15,128(%rax)

	vmulpd %ymm5,%ymm12,%ymm15 # Q1*SD2I
	vaddpd 160(%rax),%ymm15,%ymm15 # G1I+SD2I*Q1	
	vmovapd %ymm15,160(%rax)

	vmulpd %ymm1,%ymm12,%ymm15 # Q1*SD1I
	vaddpd 192(%rax),%ymm15,%ymm15 # G2I+SD1I*Q1	
	vmulpd %ymm9,%ymm13,%ymm14 # Q2*SD3I		
	vaddpd %ymm14,%ymm15,%ymm15 # G2I+SD3I*Q2
	vmovapd %ymm15,192(%rax)

	vmulpd %ymm6,%ymm12,%ymm15 # Q1*SD2A
	vaddpd 224(%rax),%ymm15,%ymm15 # G1A+SD2A*Q1
	vmovapd %ymm15,224(%rax)
	
	vmulpd %ymm2,%ymm12,%ymm15 # Q1*SD1A
	vaddpd 256(%rax),%ymm15,%ymm15 # G2A+SD1A*Q1
	vmulpd %ymm10,%ymm13,%ymm14 # Q2*SD3A	
	vaddpd %ymm14,%ymm15,%ymm15 # G2A+SD3A*Q2
	vmovapd %ymm15,256(%rax)

	vmulpd %ymm7,%ymm12,%ymm15 # Q1*SD2B
	vaddpd 288(%rax),%ymm15,%ymm15 # G1B+SD2B*Q1
	vmovapd %ymm15,288(%rax)
	
	vmulpd %ymm3,%ymm12,%ymm15 # Q1*SD1B
	vaddpd 320(%rax),%ymm15,%ymm15 # G2B+SD1B*Q1
	vmulpd %ymm11,%ymm13,%ymm14 # Q2*SD3B
	vaddpd %ymm14,%ymm15,%ymm15 # G2B+SD3B*Q2
	vmovapd %ymm15,320(%rax)

	addq $352,%rax
	cmpq %rax,%r10
	jne LC20
	
	ret
	
