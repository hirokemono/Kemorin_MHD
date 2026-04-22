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
.globl lxoswv_
.globl _lxoswv_	
lxoswv_:
_lxoswv_:	
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
	movq %rdi,%r11			
	shlq $3,%r10
	shlq $2,%r11
	addq %r11,%r10
	subq %rdi,%r10	# r10 に JB*8*4*11 が入る
	addq %rcx,%r10

	cmpq $0,%r9
	je L01
	cmpq $9,%r9
	je L91
	cmpq $8,%r9
	je L81
	cmpq $7,%r9
	je L71
	cmpq $6,%r9
	je L61
	cmpq $5,%r9
	je L51
	cmpq $4,%r9
	je L41
	cmpq $3,%r9
	je L31
	cmpq $2,%r9
	je L21

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

	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	vbroadcastsd 128(%rdx),%zmm2 # SD5R
	vbroadcastsd 136(%rdx),%zmm3 # SD5I
	
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
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vfmadd231pd 160(%rdx){1to8},%zmm0,%zmm14	
	vfmadd231pd 224(%rdx){1to8},%zmm1,%zmm14		
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vfmadd231pd %zmm2,%zmm0,%zmm14
	vfmadd231pd 192(%rdx){1to8},%zmm1,%zmm14			
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vfmadd231pd 168(%rdx){1to8},%zmm0,%zmm14
	vfmadd231pd 232(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vfmadd231pd %zmm3,%zmm0,%zmm14	
	vfmadd231pd 200(%rdx){1to8},%zmm1,%zmm14	
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vfmadd231pd 176(%rdx){1to8},%zmm0,%zmm14		
	vfmadd231pd 240(%rdx){1to8},%zmm1,%zmm14		
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vfmadd231pd 144(%rdx){1to8},%zmm0,%zmm14
	vfmadd231pd 208(%rdx){1to8},%zmm1,%zmm14			
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vfmadd231pd 184(%rdx){1to8},%zmm0,%zmm14	
	vfmadd231pd 248(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vfmadd231pd 152(%rdx){1to8},%zmm0,%zmm14		
	vfmadd231pd 216(%rdx){1to8},%zmm1,%zmm14	
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
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

	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	vbroadcastsd 128(%rdx),%zmm2 # SD5R
	vbroadcastsd 136(%rdx),%zmm3 # SD5I
	vbroadcastsd 144(%rdx),%zmm7 # SD5A
	vbroadcastsd 152(%rdx),%zmm15 # SD5B

	movq %rcx,%rax
L90:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd %zmm11,%zmm1	
	vfmadd231pd %zmm14,%zmm10,%zmm1
	vfmadd213pd %zmm13,%zmm0,%zmm1 # Q2new

	vmovapd %zmm5,%zmm6
	vfmadd231pd %zmm14,%zmm4,%zmm6
	vfmadd213pd %zmm0,%zmm1,%zmm6	# Q1new2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vfmadd231pd 160(%rdx){1to8},%zmm0,%zmm14	
	vfmadd231pd 224(%rdx){1to8},%zmm1,%zmm14
	vfmadd231pd 288(%rdx){1to8},%zmm6,%zmm14			
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vfmadd231pd %zmm2,%zmm0,%zmm14
	vfmadd231pd 192(%rdx){1to8},%zmm1,%zmm14
	vfmadd231pd 256(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vfmadd231pd 168(%rdx){1to8},%zmm0,%zmm14
	vfmadd231pd 232(%rdx){1to8},%zmm1,%zmm14
	vfmadd231pd 296(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vfmadd231pd %zmm3,%zmm0,%zmm14	
	vfmadd231pd 200(%rdx){1to8},%zmm1,%zmm14	
	vfmadd231pd 264(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vfmadd231pd 176(%rdx){1to8},%zmm0,%zmm14		
	vfmadd231pd 240(%rdx){1to8},%zmm1,%zmm14		
	vfmadd231pd 304(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vfmadd231pd %zmm7,%zmm0,%zmm14
	vfmadd231pd 208(%rdx){1to8},%zmm1,%zmm14			
	vfmadd231pd 272(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vfmadd231pd 184(%rdx){1to8},%zmm0,%zmm14	
	vfmadd231pd 248(%rdx){1to8},%zmm1,%zmm14
	vfmadd231pd 312(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vfmadd231pd %zmm15,%zmm0,%zmm14		
	vfmadd231pd 216(%rdx){1to8},%zmm1,%zmm14
	vfmadd231pd 280(%rdx){1to8},%zmm6,%zmm14	
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
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

	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	vbroadcastsd 128(%rdx),%zmm2 # SD5R
	vbroadcastsd 136(%rdx),%zmm3 # SD5I
	vbroadcastsd 144(%rdx),%zmm7 # SD5A
	vbroadcastsd 152(%rdx),%zmm15 # SD5B

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

	vmovapd %zmm5,%zmm6
	vfmadd231pd %zmm14,%zmm4,%zmm6
	vfmadd213pd %zmm0,%zmm1,%zmm6	# Q1new2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vfmadd231pd 160(%rdx){1to8},%zmm0,%zmm14	
	vfmadd231pd 224(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vfmadd231pd %zmm2,%zmm0,%zmm14
	vfmadd231pd 192(%rdx){1to8},%zmm1,%zmm14
	vfmadd231pd 256(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vfmadd231pd 168(%rdx){1to8},%zmm0,%zmm14
	vfmadd231pd 232(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vfmadd231pd %zmm3,%zmm0,%zmm14	
	vfmadd231pd 200(%rdx){1to8},%zmm1,%zmm14	
	vfmadd231pd 264(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vfmadd231pd 176(%rdx){1to8},%zmm0,%zmm14		
	vfmadd231pd 240(%rdx){1to8},%zmm1,%zmm14		
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vfmadd231pd %zmm7,%zmm0,%zmm14
	vfmadd231pd 208(%rdx){1to8},%zmm1,%zmm14			
	vfmadd231pd 272(%rdx){1to8},%zmm6,%zmm14
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vfmadd231pd 184(%rdx){1to8},%zmm0,%zmm14	
	vfmadd231pd 248(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vfmadd231pd %zmm15,%zmm0,%zmm14		
	vfmadd231pd 216(%rdx){1to8},%zmm1,%zmm14
	vfmadd231pd 280(%rdx){1to8},%zmm6,%zmm14	
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
	cmpq %rax,%r10
	jne L80

	ret
	
# ILEV=7 case
L71:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 

	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	vbroadcastsd 128(%rdx),%zmm2 # SD5R
	vbroadcastsd 136(%rdx),%zmm3 # SD5I
	vbroadcastsd 144(%rdx),%zmm7 # SD5A
	vbroadcastsd 152(%rdx),%zmm15 # SD5B
	vbroadcastsd 160(%rdx),%zmm4 # SD6R
	vbroadcastsd 168(%rdx),%zmm5 # SD6I
	vbroadcastsd 176(%rdx),%zmm6 # SD6A

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
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vfmadd231pd %zmm4,%zmm0,%zmm14	
	vfmadd231pd 224(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vfmadd231pd %zmm2,%zmm0,%zmm14
	vfmadd231pd 192(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vfmadd231pd %zmm5,%zmm0,%zmm14
	vfmadd231pd 232(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vfmadd231pd %zmm3,%zmm0,%zmm14	
	vfmadd231pd 200(%rdx){1to8},%zmm1,%zmm14	
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vfmadd231pd %zmm6,%zmm0,%zmm14		
	vfmadd231pd 240(%rdx){1to8},%zmm1,%zmm14		
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vfmadd231pd %zmm7,%zmm0,%zmm14
	vfmadd231pd 208(%rdx){1to8},%zmm1,%zmm14			
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vfmadd231pd 184(%rdx){1to8},%zmm0,%zmm14	
	vfmadd231pd 248(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vfmadd231pd %zmm15,%zmm0,%zmm14		
	vfmadd231pd 216(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
	cmpq %rax,%r10
	jne L70

	ret
	
# ILEV=6 case
L61:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 
	vbroadcastsd 16(%rsi),%zmm10 # AC3 
	vbroadcastsd 24(%rsi),%zmm11 # AC4 

	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	vbroadcastsd 128(%rdx),%zmm2 # SD5R
	vbroadcastsd 136(%rdx),%zmm3 # SD5I
	vbroadcastsd 144(%rdx),%zmm7 # SD5A
	vbroadcastsd 152(%rdx),%zmm15 # SD5B
	vbroadcastsd 160(%rdx),%zmm4 # SD6R
	vbroadcastsd 168(%rdx),%zmm5 # SD6I
	vbroadcastsd 176(%rdx),%zmm6 # SD6A

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
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vfmadd231pd %zmm4,%zmm0,%zmm14	
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vfmadd231pd %zmm2,%zmm0,%zmm14
	vfmadd231pd 192(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vfmadd231pd %zmm5,%zmm0,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vfmadd231pd %zmm3,%zmm0,%zmm14	
	vfmadd231pd 200(%rdx){1to8},%zmm1,%zmm14	
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vfmadd231pd %zmm6,%zmm0,%zmm14		
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vfmadd231pd %zmm7,%zmm0,%zmm14
	vfmadd231pd 208(%rdx){1to8},%zmm1,%zmm14			
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vfmadd231pd 184(%rdx){1to8},%zmm0,%zmm14	
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vfmadd231pd %zmm15,%zmm0,%zmm14		
	vfmadd231pd 216(%rdx){1to8},%zmm1,%zmm14
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
	cmpq %rax,%r10
	jne L60

	ret
	
# ILEV=5 case
L51:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 

	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	vbroadcastsd 128(%rdx),%zmm2 # SD5R
	vbroadcastsd 136(%rdx),%zmm3 # SD5I
	vbroadcastsd 144(%rdx),%zmm7 # SD5A
	vbroadcastsd 152(%rdx),%zmm15 # SD5B
	vbroadcastsd 160(%rdx),%zmm4 # SD6R
	vbroadcastsd 168(%rdx),%zmm5 # SD6I
	vbroadcastsd 176(%rdx),%zmm6 # SD6A
	vbroadcastsd 184(%rdx),%zmm1 # SD6B

	movq %rcx,%rax
L50:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vfmadd231pd %zmm4,%zmm0,%zmm14	
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vfmadd231pd %zmm2,%zmm0,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vfmadd231pd %zmm5,%zmm0,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vfmadd231pd %zmm3,%zmm0,%zmm14	
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vfmadd231pd %zmm6,%zmm0,%zmm14		
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vfmadd231pd %zmm7,%zmm0,%zmm14
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vfmadd231pd %zmm1,%zmm0,%zmm14	
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vfmadd231pd %zmm15,%zmm0,%zmm14		
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
	cmpq %rax,%r10
	jne L50

	ret
	
# ILEV=4 case
L41:
	vbroadcastsd   (%rsi),%zmm8  # AC1 
	vbroadcastsd  8(%rsi),%zmm9  # AC2 

	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	vbroadcastsd 128(%rdx),%zmm2 # SD5R
	vbroadcastsd 136(%rdx),%zmm3 # SD5I
	vbroadcastsd 144(%rdx),%zmm7 # SD5A
	vbroadcastsd 152(%rdx),%zmm15 # SD5B

	movq %rcx,%rax
L40:
	vmovapd   (%rax),%zmm14 # X2					
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd %zmm9,%zmm0	
	vfmadd231pd %zmm14,%zmm8,%zmm0
	vfmadd213pd %zmm12,%zmm13,%zmm0	# Q1new

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vfmadd231pd %zmm2,%zmm0,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vfmadd231pd %zmm3,%zmm0,%zmm14	
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vfmadd231pd %zmm7,%zmm0,%zmm14
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vfmadd231pd %zmm15,%zmm0,%zmm14		
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
	cmpq %rax,%r10
	jne L40

	ret
	
# ILEV=3 case
L31:
	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B
	vbroadcastsd  96(%rdx),%zmm28 # SD4R 
	vbroadcastsd 104(%rdx),%zmm29 # SD4I
	vbroadcastsd 112(%rdx),%zmm30 # SD4A 
	vbroadcastsd 120(%rdx),%zmm31 # SD4B

	movq %rcx,%rax
L30:
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vfmadd231pd %zmm28,%zmm13,%zmm14
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vfmadd231pd %zmm29,%zmm13,%zmm14
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vfmadd231pd %zmm30,%zmm13,%zmm14
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vfmadd231pd %zmm31,%zmm13,%zmm14
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
	cmpq %rax,%r10
	jne L30

	ret
	
# ILEV=2 case
L21:
	vbroadcastsd    (%rdx),%zmm16 # SD1R 	
	vbroadcastsd   8(%rdx),%zmm17 # SD1I
	vbroadcastsd  16(%rdx),%zmm18 # SD1A
	vbroadcastsd  24(%rdx),%zmm19 # SD1B
	vbroadcastsd  32(%rdx),%zmm20 # SD2R 
	vbroadcastsd  40(%rdx),%zmm21 # SD2I
	vbroadcastsd  48(%rdx),%zmm22 # SD2A 
	vbroadcastsd  56(%rdx),%zmm23 # SD2B
	vbroadcastsd  64(%rdx),%zmm24 # SD3R 
	vbroadcastsd  72(%rdx),%zmm25 # SD3I
	vbroadcastsd  80(%rdx),%zmm26 # SD3A 
	vbroadcastsd  88(%rdx),%zmm27 # SD3B

	movq %rcx,%rax
L20:
	vmovapd 64(%rax),%zmm12 # Q1
	vmovapd 128(%rax),%zmm13 # Q2

	vmovapd 192(%rax),%zmm14
	vfmadd231pd %zmm20,%zmm12,%zmm14		
	vmovapd %zmm14,192(%rax) # G1R	

	vmovapd 256(%rax),%zmm14
	vfmadd231pd %zmm16,%zmm12,%zmm14	
	vfmadd231pd %zmm24,%zmm13,%zmm14
	vmovapd %zmm14,256(%rax) # G2R	
	
	vmovapd 320(%rax),%zmm14
	vfmadd231pd %zmm21,%zmm12,%zmm14	
	vmovapd %zmm14,320(%rax) # G1I	
		
	vmovapd 384(%rax),%zmm14
	vfmadd231pd %zmm17,%zmm12,%zmm14
	vfmadd231pd %zmm25,%zmm13,%zmm14
	vmovapd %zmm14,384(%rax) # G2I

	vmovapd 448(%rax),%zmm14
	vfmadd231pd %zmm22,%zmm12,%zmm14		
	vmovapd %zmm14,448(%rax) # G1A

	vmovapd 512(%rax),%zmm14
	vfmadd231pd %zmm18,%zmm12,%zmm14	
	vfmadd231pd %zmm26,%zmm13,%zmm14
	vmovapd %zmm14,512(%rax) # G2A
	
	vmovapd 576(%rax),%zmm14
	vfmadd231pd %zmm23,%zmm12,%zmm14	
	vmovapd %zmm14,576(%rax) # G1B
		
	vmovapd 640(%rax),%zmm14
	vfmadd231pd %zmm19,%zmm12,%zmm14
	vfmadd231pd %zmm27,%zmm13,%zmm14
	vmovapd %zmm14,640(%rax) # G2I

	addq $704,%rax
	cmpq %rax,%r10
	jne L20

	ret
