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
.globl fxrqfp_
.globl _fxrqfp_
fxrqfp_:
_fxrqfp_:	
	movq   (%rdi), %rdi  # N が rdi に	
	# X の先頭アドレスは rsi に
	# T の先頭アドレスは rdx に

	movq %rdi,%r9 # r9 に N をコピー

	movq $3,%r8
	andq %rdi,%r8

	shlq $4,%r9 # r9 に 16*N

	vbroadcastsd C2(%rip),%ymm11 # 倍精度不動小数点の 2 を ymm11 の4箇所に
	vbroadcastsd C05(%rip),%ymm12 # 倍精度不動小数点の 0.5 を ymm12 の4箇所に

	movq %rdi,%r10
	addq %r10,%r10	
        cvtsi2sdq  %r10, %xmm0		
	movsd C2(%rip),%xmm1
	divsd %xmm0, %xmm1
	movsd %xmm1,-8(%rsp)	
	vbroadcastsd -8(%rsp),%ymm13
	
	vmovapd   (%rsi), %ymm0 # X(I,1,0)
	vmovapd 32(%rsi), %ymm1 # X(I,2,0)
#-- scaling --
	vmulpd %ymm13,%ymm0,%ymm0
	vmulpd %ymm13,%ymm1,%ymm1
#-------------
	vaddpd %ymm1,%ymm0,%ymm2
	vsubpd %ymm1,%ymm0,%ymm0
	vmovapd %ymm2,(%rsi)
	vmovapd %ymm0,32(%rsi)

	cmpq $0,%r8 # MOD(N,4) が 0かどうか
	jne L0

	vsubpd %ymm2,%ymm2,%ymm2
	vmovapd   (%rsi,%r9), %ymm0 # X(I,1,N/4)
	vmovapd 32(%rsi,%r9), %ymm1 # X(I,2,N/4)
#-- scaling --
	vmulpd %ymm13,%ymm0,%ymm0
	vmulpd %ymm13,%ymm1,%ymm1
#-------------
	vmovapd  %ymm0,(%rsi,%r9)  # X(I,1,N/4)
	vsubpd %ymm1,%ymm2,%ymm2
	vmovapd %ymm2,32(%rsi,%r9)  # X(I,2,N/4)

L0:	

	cmpq $6,%rdi
	jl LE # N<6 の場合はこれで終了

	movq %rdi,%r9
	subq $2,%r9 # r9 に N-2
	shrq $2,%r9 # r9 に (N-2)/4
	shlq $4,%r9 # r9 に (N-2)/4*16
	
	movq %rdx,%r8
        addq %r9,%r8	# r8 は終了判定に使う

	shlq $5,%rdi # rdi に 32*N
	addq %rsi,%rdi # rdi に rsi + 32*N

	addq $64,%rsi
	subq $64,%rdi

#-- scaling ファクターを 0.5 にまとめてしまう.
	vmulpd %ymm13,%ymm12,%ymm12
#----	
	
L1:
	vbroadcastsd (%rdx), %ymm8 # T(1,K)
	vbroadcastsd 8(%rdx), %ymm9 # T(2,K)
	
	vmovapd   (%rsi), %ymm0 # X(I,1,K)
	vmovapd 32(%rsi), %ymm1 # X(I,2,K)	
	vmovapd   (%rdi), %ymm2 # X(I,1,N/2-K)
	vmovapd 32(%rdi), %ymm3 # X(I,2,N/2-K)

	vaddpd %ymm2,%ymm0,%ymm4 # addr
	vmulpd %ymm12,%ymm4,%ymm4 # addr*0.5
	vsubpd %ymm0,%ymm2,%ymm0 # subr
	vmulpd %ymm12,%ymm0,%ymm0 # subr*0.5	
	vaddpd %ymm3,%ymm1,%ymm5 # addi
	vmulpd %ymm12,%ymm5,%ymm5 # addi*0.5	
	vsubpd %ymm1,%ymm3,%ymm1 # subi
	vmulpd %ymm12,%ymm1,%ymm1 # subi*0.5

	vmulpd %ymm9,%ymm0,%ymm2 #subr*tr2
	vsubpd %ymm2,%ymm4,%ymm2 #addr-subr*tr2
	vmulpd %ymm8,%ymm5,%ymm3 #addi*tr1
	vaddpd %ymm3,%ymm2,%ymm2 #addr-subr*tr2+addi*tr1
	vmovapd %ymm2,  (%rsi) # X(I,1,K)

	vmulpd %ymm9,%ymm5,%ymm6 #addi*tr2
	vsubpd %ymm6,%ymm1,%ymm6 #subi-addi*tr2
	vmulpd %ymm8,%ymm0,%ymm7 #subr*tr1
	vsubpd %ymm7,%ymm6,%ymm6 #subi-addi*tr2-subr*tr1
	vmovapd %ymm6,  32(%rsi) # X(I,2,K)

	vmulpd %ymm11,%ymm4,%ymm4
	vsubpd %ymm2,%ymm4,%ymm4
	vmovapd %ymm4,(%rdi) # X(I,1,N/2-K)	

	vmulpd %ymm11,%ymm1,%ymm1
	vsubpd %ymm1,%ymm6,%ymm1
	vmovapd %ymm1,32(%rdi) # X(I,2,N/2-K)	

	addq $16,%rdx
	addq $64,%rsi
	subq $64,%rdi
	cmpq %rdx,%r8
	jne L1
	
LE:
	
	ret

C2: # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000
C05: # 倍精度不動小数点の 0.5
	.long   0x00000000,0x3fe00000
	
