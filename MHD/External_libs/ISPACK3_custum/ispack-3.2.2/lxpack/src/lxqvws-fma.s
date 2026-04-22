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
.globl lxqvws_
.globl _lxqvws_	
lxqvws_:
_lxqvws_:	
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
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7
	vpxor %ymm8,%ymm8,%ymm8
	vpxor %ymm9,%ymm9,%ymm9
	vpxor %ymm10,%ymm10,%ymm10
	vpxor %ymm11,%ymm11,%ymm11
	
	movq %rcx,%rax
LC0:
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd 96(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm3
	vfmadd231pd %ymm14,%ymm13,%ymm9
	vmovapd 128(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm0
	vfmadd231pd %ymm14,%ymm13,%ymm6
	vmovapd 160(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm4
	vfmadd231pd %ymm14,%ymm13,%ymm10
	vmovapd 192(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm1
	vfmadd231pd %ymm14,%ymm13,%ymm7
	vmovapd 224(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm5
	vfmadd231pd %ymm14,%ymm13,%ymm11
	vmovapd 256(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm2
	vfmadd231pd %ymm14,%ymm13,%ymm8

	addq $352,%rax
	cmpq %rax,%r10
	jne LC0

	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm15
	vaddsd (%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm0,%xmm0
	vmovsd %xmm0,(%rdx)

	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm15
	vaddsd  8(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm1,%xmm1
	vmovsd %xmm1, 8(%rdx)

	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm15
	vaddsd 16(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm2,%xmm2
	vmovsd %xmm2,16(%rdx)

	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm15
	vaddsd 32(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm3,%xmm3
	vmovsd %xmm3,32(%rdx)

	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm4,%xmm15
	vaddsd 40(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm4,%xmm4
	vmovsd %xmm4,40(%rdx)

	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm5,%xmm15
	vaddsd 48(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm5,%xmm5
	vmovsd %xmm5,48(%rdx)

	vhaddpd %ymm6,%ymm6,%ymm6
	vextractf128 $1,%ymm6,%xmm15
	vaddsd 64(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm6,%xmm6
	vmovsd %xmm6,64(%rdx)
	
	vhaddpd %ymm7,%ymm7,%ymm7
	vextractf128 $1,%ymm7,%xmm15
	vaddsd 72(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm7,72(%rdx)

	vhaddpd %ymm8,%ymm8,%ymm8
	vextractf128 $1,%ymm8,%xmm15
	vaddsd 80(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm8,%xmm8
	vmovsd %xmm8,80(%rdx)

	vhaddpd %ymm9,%ymm9,%ymm9
	vextractf128 $1,%ymm9,%xmm15
	vaddsd 96(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm9,%xmm9
	vmovsd %xmm9,96(%rdx)
	
	vhaddpd %ymm10,%ymm10,%ymm10
	vextractf128 $1,%ymm10,%xmm15
	vaddsd 104(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm10,%xmm10
	vmovsd %xmm10,104(%rdx)

	vhaddpd %ymm11,%ymm11,%ymm11
	vextractf128 $1,%ymm11,%xmm15
	vaddsd 112(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm11,%xmm11
	vmovsd %xmm11,112(%rdx)

#-------------
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3

	vbroadcastsd   (%rsi),%ymm8 # AC5 
	vbroadcastsd  8(%rsi),%ymm9 # AC6 
	
	vbroadcastsd 16(%rsi),%ymm10 # AC5 
	vbroadcastsd 24(%rsi),%ymm11 # AC6 

	movq %rcx,%rax
LD0:
	vmovapd   (%rax),%ymm14 # X2	
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vmovapd %ymm8,%ymm15
	vfmadd213pd %ymm9,%ymm14,%ymm15
	vfmadd213pd %ymm12,%ymm13,%ymm15
	vmovapd %ymm15,32(%rax) # Q1		
	
	vfmadd213pd %ymm11,%ymm10,%ymm14
	vfmadd213pd %ymm13,%ymm15,%ymm14	
	vmovapd %ymm14,64(%rax) # Q2			
	
	vmovapd 288(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm1
	vfmadd231pd %ymm14,%ymm13,%ymm3
	
	vmovapd 320(%rax),%ymm14
	vfmadd231pd %ymm14,%ymm12,%ymm0
	vfmadd231pd %ymm14,%ymm13,%ymm2
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LD0

	vhaddpd %ymm0,%ymm0,%ymm0
	vextractf128 $1,%ymm0,%xmm15
	vaddsd 24(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm0,%xmm0
	vmovsd %xmm0,24(%rdx)

	vhaddpd %ymm1,%ymm1,%ymm1
	vextractf128 $1,%ymm1,%xmm15
	vaddsd 56(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm1,%xmm1
	vmovsd %xmm1,56(%rdx)

	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm2,%xmm15
	vaddsd 88(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm2,%xmm2
	vmovsd %xmm2,88(%rdx)

	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm3,%xmm15
	vaddsd 120(%rdx),%xmm15,%xmm15
	vaddsd %xmm15,%xmm3,%xmm3
	vmovsd %xmm3,120(%rdx)

	ret
	
# common for case5	
LC5:
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7
	
	vbroadcastsd   (%rsi),%ymm8 # AC5 
	vbroadcastsd  8(%rsi),%ymm9 # AC6 

	movq %rcx,%rax
LC50:
	vmovapd   (%rax),%ymm14 # X2	
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vfmadd213pd %ymm9,%ymm8,%ymm14
	vfmadd132pd 64(%rax),%ymm12,%ymm14	
	vmovapd %ymm14,32(%rax) # Q1	

	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LC50

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

#-------------	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7

	movq %rcx,%rax
LD50:
	vmovapd 64(%rax),%ymm12 # Q2

	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LD50

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
	vaddsd 64(%rdx),%xmm8,%xmm8
	vaddsd 72(%rdx),%xmm9,%xmm9
	vaddsd 80(%rdx),%xmm10,%xmm10
	vaddsd 88(%rdx),%xmm11,%xmm11
	vaddsd 96(%rdx),%xmm12,%xmm12
	vaddsd 104(%rdx),%xmm13,%xmm13
	vaddsd 112(%rdx),%xmm14,%xmm14
	vaddsd 120(%rdx),%xmm15,%xmm15
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vaddsd %xmm12,%xmm4,%xmm4
	vaddsd %xmm13,%xmm5,%xmm5
	vaddsd %xmm14,%xmm6,%xmm6
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm0, 64(%rdx)
	vmovsd %xmm1, 72(%rdx)
	vmovsd %xmm2, 80(%rdx)			
	vmovsd %xmm3, 88(%rdx)
	vmovsd %xmm4, 96(%rdx)
	vmovsd %xmm5,104(%rdx)
	vmovsd %xmm6,112(%rdx)
	vmovsd %xmm7,120(%rdx)				

#-------------	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7

	movq %rcx,%rax
LE50:
	vmovapd 32(%rax),%ymm12 # Q1

	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LE50

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
	vaddsd 128(%rdx),%xmm8,%xmm8
	vaddsd 136(%rdx),%xmm9,%xmm9
	vaddsd 144(%rdx),%xmm10,%xmm10
	vaddsd 152(%rdx),%xmm11,%xmm11
	vaddsd 160(%rdx),%xmm12,%xmm12
	vaddsd 168(%rdx),%xmm13,%xmm13
	vaddsd 176(%rdx),%xmm14,%xmm14
	vaddsd 184(%rdx),%xmm15,%xmm15
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vaddsd %xmm12,%xmm4,%xmm4
	vaddsd %xmm13,%xmm5,%xmm5
	vaddsd %xmm14,%xmm6,%xmm6
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm0,128(%rdx)
	vmovsd %xmm1,136(%rdx)
	vmovsd %xmm2,144(%rdx)			
	vmovsd %xmm3,152(%rdx)
	vmovsd %xmm4,160(%rdx)
	vmovsd %xmm5,168(%rdx)
	vmovsd %xmm6,176(%rdx)
	vmovsd %xmm7,184(%rdx)				

	ret

# common for case4	
LC4:	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7

	vbroadcastsd   (%rsi),%ymm8 # AC5 
	vbroadcastsd  8(%rsi),%ymm9 # AC6 

	movq %rcx,%rax
LC40:
	vmovapd   (%rax),%ymm14 # X2	
	vmovapd 32(%rax),%ymm12 # Q1
	vmovapd 64(%rax),%ymm13 # Q2

	vfmadd213pd %ymm9,%ymm8,%ymm14
	vfmadd132pd 64(%rax),%ymm12,%ymm14	
	vmovapd %ymm14,32(%rax) # Q1	
	
	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LC40

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

#-------------	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7

	movq %rcx,%rax
LD40:
	vmovapd 64(%rax),%ymm12 # Q2

	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LD40

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
	vaddsd 64(%rdx),%xmm8,%xmm8
	vaddsd 72(%rdx),%xmm9,%xmm9
	vaddsd 80(%rdx),%xmm10,%xmm10
	vaddsd 88(%rdx),%xmm11,%xmm11
	vaddsd 96(%rdx),%xmm12,%xmm12
	vaddsd 104(%rdx),%xmm13,%xmm13
	vaddsd 112(%rdx),%xmm14,%xmm14
	vaddsd 120(%rdx),%xmm15,%xmm15
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vaddsd %xmm12,%xmm4,%xmm4
	vaddsd %xmm13,%xmm5,%xmm5
	vaddsd %xmm14,%xmm6,%xmm6
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm0, 64(%rdx)
	vmovsd %xmm1, 72(%rdx)
	vmovsd %xmm2, 80(%rdx)			
	vmovsd %xmm3, 88(%rdx)
	vmovsd %xmm4, 96(%rdx)
	vmovsd %xmm5,104(%rdx)
	vmovsd %xmm6,112(%rdx)
	vmovsd %xmm7,120(%rdx)				

#-------------	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3

	movq %rcx,%rax
LE40:
	vmovapd 32(%rax),%ymm12 # Q1

	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LE40

	vhaddpd %ymm0,%ymm0,%ymm0 
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vextractf128 $1,%ymm3,%xmm11
	vaddsd 128(%rdx),%xmm8,%xmm8
	vaddsd 136(%rdx),%xmm9,%xmm9
	vaddsd 144(%rdx),%xmm10,%xmm10
	vaddsd 152(%rdx),%xmm11,%xmm11
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vmovsd %xmm0,128(%rdx)
	vmovsd %xmm1,136(%rdx)
	vmovsd %xmm2,144(%rdx)			
	vmovsd %xmm3,152(%rdx)

	ret

# common for case3	
LC3:
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7

	movq %rcx,%rax
LC30:
	vmovapd 32(%rax),%ymm12 # Q1
	
	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LC30

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

#-------------	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7

	movq %rcx,%rax
LD30:
	vmovapd 64(%rax),%ymm12 # Q2

	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LD30

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
	vaddsd 64(%rdx),%xmm8,%xmm8
	vaddsd 72(%rdx),%xmm9,%xmm9
	vaddsd 80(%rdx),%xmm10,%xmm10
	vaddsd 88(%rdx),%xmm11,%xmm11
	vaddsd 96(%rdx),%xmm12,%xmm12
	vaddsd 104(%rdx),%xmm13,%xmm13
	vaddsd 112(%rdx),%xmm14,%xmm14
	vaddsd 120(%rdx),%xmm15,%xmm15
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vaddsd %xmm12,%xmm4,%xmm4
	vaddsd %xmm13,%xmm5,%xmm5
	vaddsd %xmm14,%xmm6,%xmm6
	vaddsd %xmm15,%xmm7,%xmm7
	vmovsd %xmm0, 64(%rdx)
	vmovsd %xmm1, 72(%rdx)
	vmovsd %xmm2, 80(%rdx)			
	vmovsd %xmm3, 88(%rdx)
	vmovsd %xmm4, 96(%rdx)
	vmovsd %xmm5,104(%rdx)
	vmovsd %xmm6,112(%rdx)
	vmovsd %xmm7,120(%rdx)				

	ret

# common for case2	
LC2:	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5
	vpxor %ymm6,%ymm6,%ymm6
	vpxor %ymm7,%ymm7,%ymm7

	movq %rcx,%rax
LC20:
	vmovapd 32(%rax),%ymm12 # Q1
	
	vfmadd231pd 96(%rax),%ymm12,%ymm4 # SD2R=SD2R+Q1*G1R
	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 160(%rax),%ymm12,%ymm5 # SD2I=SD2I+Q1*G1I
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 224(%rax),%ymm12,%ymm6 # SD2A=SD2A+Q1*G1A
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 288(%rax),%ymm12,%ymm7 # SD2B=SD2B+Q1*G1B
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LC20

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

#-------------	
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3

	movq %rcx,%rax
LD20:
	vmovapd 64(%rax),%ymm12 # Q2

	vfmadd231pd 128(%rax),%ymm12,%ymm0 # SD1R=SD1R+Q1*G2R	
	vfmadd231pd 192(%rax),%ymm12,%ymm1 # SD1I=SD1I+Q1*G2I
	vfmadd231pd 256(%rax),%ymm12,%ymm2 # SD1A=SD1A+Q1*G2A
	vfmadd231pd 320(%rax),%ymm12,%ymm3 # SD1B=SD1B+Q1*G2B
	
	addq $352,%rax
	cmpq %rax,%r10
	jne LD20

	vhaddpd %ymm0,%ymm0,%ymm0 
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vextractf128 $1,%ymm3,%xmm11
	vaddsd 64(%rdx),%xmm8,%xmm8
	vaddsd 72(%rdx),%xmm9,%xmm9
	vaddsd 80(%rdx),%xmm10,%xmm10
	vaddsd 88(%rdx),%xmm11,%xmm11
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vmovsd %xmm0, 64(%rdx)
	vmovsd %xmm1, 72(%rdx)
	vmovsd %xmm2, 80(%rdx)			
	vmovsd %xmm3, 88(%rdx)

	ret
