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
.globl lxqgzs_
.globl _lxqgzs_	
lxqgzs_:
_lxqgzs_:	
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
	shlq $2,%r10
	addq %rdi,%r10	# r10 �� JB*8*4*5 ������
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

	ret

# ILEV=0 case
L0:	
	call LC
	addq $32,%rsi
	addq $32,%rdx
	call LC
	ret
	
# ILEV=9 case
L9:	
	call LC
	addq $32,%rsi
	addq $32,%rdx
	call LC5
	ret
	
# ILEV=8 case
L8:	
	call LC
	addq $32,%rsi
	addq $32,%rdx
	call LC4
	ret
	
# ILEV=7 case
L7:	
	call LC
	addq $32,%rsi
	addq $32,%rdx
	call LC3
	ret
	
# ILEV=6 case
L6:	
	call LC
	addq $32,%rsi
	addq $32,%rdx
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

	vbroadcastsd   (%rsi),%ymm8
	vbroadcastsd  8(%rsi),%ymm9
	vbroadcastsd 16(%rsi),%ymm10
	vbroadcastsd 24(%rsi),%ymm11

	movq %rcx,%rax	
LC0:
	vmovapd   (%rax),%ymm5
	vmovapd   %ymm5,%ymm4
	vmovapd 32(%rax),%ymm12
	vmovapd 64(%rax),%ymm13

	vfmadd213pd %ymm9,%ymm8,%ymm5
	vfmadd213pd %ymm11,%ymm10,%ymm4
	vfmadd213pd %ymm12,%ymm13,%ymm5
	vfmadd213pd %ymm13,%ymm5,%ymm4	
	
	vmovapd 96(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm1
	vfmadd231pd %ymm13,%ymm15,%ymm3
	
	vmovapd 128(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm0
	vfmadd231pd %ymm13,%ymm15,%ymm2

	vmovapd %ymm5,32(%rax)
	vmovapd %ymm4,64(%rax)
	
	addq $160,%rax
	cmpq %rax,%r10
	jne LC0

	vhaddpd %ymm0,%ymm0,%ymm0
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vextractf128 $1,%ymm3,%xmm11
	vaddsd   (%rdx),%xmm8,%xmm8
	vaddsd  8(%rdx),%xmm9,%xmm9
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd 24(%rdx),%xmm11,%xmm11
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vmovsd %xmm0,  (%rdx)
	vmovsd %xmm1, 8(%rdx)
	vmovsd %xmm2,16(%rdx)			
	vmovsd %xmm3,24(%rdx)

	ret
	
# common for case5
LC5:
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4
	vpxor %ymm5,%ymm5,%ymm5

	vbroadcastsd   (%rsi),%ymm8
	vbroadcastsd  8(%rsi),%ymm9

	movq %rcx,%rax	
LC50:
	vmovapd   (%rax),%ymm11
	vmovapd 32(%rax),%ymm12
	vmovapd 64(%rax),%ymm13

	vfmadd213pd %ymm9,%ymm8,%ymm11
	vfmadd213pd %ymm12,%ymm13,%ymm11 # Q1new
	
	vmovapd 96(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm1
	vfmadd231pd %ymm13,%ymm15,%ymm3
	vfmadd231pd %ymm11,%ymm15,%ymm5
	
	vmovapd 128(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm0
	vfmadd231pd %ymm13,%ymm15,%ymm2
	vfmadd231pd %ymm11,%ymm15,%ymm4

	addq $160,%rax
	cmpq %rax,%r10
	jne LC50

	vhaddpd %ymm0,%ymm0,%ymm0
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vhaddpd %ymm3,%ymm3,%ymm3
	vhaddpd %ymm4,%ymm4,%ymm4
	vhaddpd %ymm5,%ymm5,%ymm5
	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vextractf128 $1,%ymm3,%xmm11
	vextractf128 $1,%ymm4,%xmm12
	vextractf128 $1,%ymm5,%xmm13		
	vaddsd   (%rdx),%xmm8,%xmm8
	vaddsd  8(%rdx),%xmm9,%xmm9
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd 24(%rdx),%xmm11,%xmm11
	vaddsd 32(%rdx),%xmm12,%xmm12
	vaddsd 40(%rdx),%xmm13,%xmm13
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vaddsd %xmm12,%xmm4,%xmm4
	vaddsd %xmm13,%xmm5,%xmm5
	vmovsd %xmm0,  (%rdx)
	vmovsd %xmm1, 8(%rdx)
	vmovsd %xmm2,16(%rdx)			
	vmovsd %xmm3,24(%rdx)
	vmovsd %xmm4,32(%rdx)
	vmovsd %xmm5,40(%rdx)		

	ret
	
# common for case4
LC4:
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3
	vpxor %ymm4,%ymm4,%ymm4

	vbroadcastsd   (%rsi),%ymm8
	vbroadcastsd  8(%rsi),%ymm9

	movq %rcx,%rax	
LC40:
	vmovapd   (%rax),%ymm11
	vmovapd 32(%rax),%ymm12
	vmovapd 64(%rax),%ymm13

	vfmadd213pd %ymm9,%ymm8,%ymm11
	vfmadd213pd %ymm12,%ymm13,%ymm11 # Q1new
	
	vmovapd 96(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm1
	vfmadd231pd %ymm13,%ymm15,%ymm3
	
	vmovapd 128(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm0
	vfmadd231pd %ymm13,%ymm15,%ymm2
	vfmadd231pd %ymm11,%ymm15,%ymm4

	addq $160,%rax
	cmpq %rax,%r10
	jne LC40

	vhaddpd %ymm0,%ymm0,%ymm0
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vhaddpd %ymm3,%ymm3,%ymm3
	vhaddpd %ymm4,%ymm4,%ymm4
	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vextractf128 $1,%ymm3,%xmm11
	vextractf128 $1,%ymm4,%xmm12
	vaddsd   (%rdx),%xmm8,%xmm8
	vaddsd  8(%rdx),%xmm9,%xmm9
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd 24(%rdx),%xmm11,%xmm11
	vaddsd 32(%rdx),%xmm12,%xmm12
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vaddsd %xmm12,%xmm4,%xmm4
	vmovsd %xmm0,  (%rdx)
	vmovsd %xmm1, 8(%rdx)
	vmovsd %xmm2,16(%rdx)			
	vmovsd %xmm3,24(%rdx)
	vmovsd %xmm4,32(%rdx)

	ret
	
# common for case3
LC3:
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2
	vpxor %ymm3,%ymm3,%ymm3

	movq %rcx,%rax	
LC30:
	vmovapd 32(%rax),%ymm12
	vmovapd 64(%rax),%ymm13

	vmovapd 96(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm1
	vfmadd231pd %ymm13,%ymm15,%ymm3
	
	vmovapd 128(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm0
	vfmadd231pd %ymm13,%ymm15,%ymm2

	addq $160,%rax
	cmpq %rax,%r10
	jne LC30

	vhaddpd %ymm0,%ymm0,%ymm0
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vhaddpd %ymm3,%ymm3,%ymm3
	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vextractf128 $1,%ymm3,%xmm11
	vaddsd   (%rdx),%xmm8,%xmm8
	vaddsd  8(%rdx),%xmm9,%xmm9
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd 24(%rdx),%xmm11,%xmm11
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vaddsd %xmm11,%xmm3,%xmm3
	vmovsd %xmm0,  (%rdx)
	vmovsd %xmm1, 8(%rdx)
	vmovsd %xmm2,16(%rdx)			
	vmovsd %xmm3,24(%rdx)

	ret
	
# common for case2
LC2:
	vpxor %ymm0,%ymm0,%ymm0
	vpxor %ymm1,%ymm1,%ymm1
	vpxor %ymm2,%ymm2,%ymm2

	movq %rcx,%rax	
LC20:
	vmovapd 32(%rax),%ymm12
	vmovapd 64(%rax),%ymm13

	vmovapd 96(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm1
	
	vmovapd 128(%rax),%ymm15
	vfmadd231pd %ymm12,%ymm15,%ymm0
	vfmadd231pd %ymm13,%ymm15,%ymm2

	addq $160,%rax
	cmpq %rax,%r10
	jne LC20

	vhaddpd %ymm0,%ymm0,%ymm0
	vhaddpd %ymm1,%ymm1,%ymm1
	vhaddpd %ymm2,%ymm2,%ymm2
	vextractf128 $1,%ymm0,%xmm8
	vextractf128 $1,%ymm1,%xmm9
	vextractf128 $1,%ymm2,%xmm10
	vaddsd   (%rdx),%xmm8,%xmm8
	vaddsd  8(%rdx),%xmm9,%xmm9
	vaddsd 16(%rdx),%xmm10,%xmm10
	vaddsd %xmm8,%xmm0,%xmm0
	vaddsd %xmm9,%xmm1,%xmm1
	vaddsd %xmm10,%xmm2,%xmm2
	vmovsd %xmm0,  (%rdx)
	vmovsd %xmm1, 8(%rdx)
	vmovsd %xmm2,16(%rdx)			

	ret
