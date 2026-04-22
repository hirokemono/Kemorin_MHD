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
.globl fxrofp_
.globl _fxrofp_
fxrofp_:
_fxrofp_:	
	movq   (%rdi), %rdi  # N が rdi に	
	# X の先頭アドレスは rsi に
	# T の先頭アドレスは rdx に

	movq %rdi,%r9 # r9 に N をコピー

	movq $3,%r8
	andq %rdi,%r8

	shlq $5,%r9 # r9 に 32*N

	vbroadcastsd C2(%rip),%zmm11 # 倍精度不動小数点の 2 を zmm11 の4箇所に
	vbroadcastsd C05(%rip),%zmm12 # 倍精度不動小数点の 0.5 を zmm12 の4箇所に

	movq %rdi,%r10
	addq %r10,%r10	
        cvtsi2sdq  %r10, %xmm0		
	movsd C2(%rip),%xmm1
	divsd %xmm0, %xmm1
	movsd %xmm1,-8(%rsp)	
	vbroadcastsd -8(%rsp),%zmm13
	
	vmovapd   (%rsi), %zmm0 # X(I,1,0)
	vmovapd 64(%rsi), %zmm1 # X(I,2,0)
#-- scaling --
	vmulpd %zmm13,%zmm0,%zmm0
	vmulpd %zmm13,%zmm1,%zmm1
#-------------
	vaddpd %zmm1,%zmm0,%zmm2
	vsubpd %zmm1,%zmm0,%zmm0
	vmovapd %zmm2,(%rsi)
	vmovapd %zmm0,64(%rsi)

	cmpq $0,%r8 # MOD(N,4) が 0かどうか
	jne L0

#	vsubpd %zmm2,%zmm2,%zmm2
	vpxorq %zmm2,%zmm2,%zmm2	
	vmovapd   (%rsi,%r9), %zmm0 # X(I,1,N/4)
	vmovapd 64(%rsi,%r9), %zmm1 # X(I,2,N/4)
#-- scaling --
	vmulpd %zmm13,%zmm0,%zmm0
##	vmulpd %zmm13,%zmm1,%zmm1
#-------------
	vmovapd  %zmm0,(%rsi,%r9)  # X(I,1,N/4)
##	vsubpd %zmm1,%zmm2,%zmm2
	vfnmadd231pd %zmm13,%zmm1,%zmm2
##	
	vmovapd %zmm2,64(%rsi,%r9)  # X(I,2,N/4)

L0:	

	cmpq $6,%rdi
	jl LE # N<6 の場合はこれで終了

	movq %rdi,%r9
	subq $2,%r9 # r9 に N-2
	shrq $2,%r9 # r9 に (N-2)/4
	shlq $4,%r9 # r9 に (N-2)/4*16
	
	movq %rdx,%r8
        addq %r9,%r8	# r8 は終了判定に使う

	shlq $6,%rdi # rdi に 64*N
	addq %rsi,%rdi # rdi に rsi + 64*N

	addq $128,%rsi
	subq $128,%rdi

#-- scaling ファクターを 0.5 にまとめてしまう.
	vmulpd %zmm13,%zmm12,%zmm12
#----	
	
L1:
	vbroadcastsd (%rdx), %zmm8 # T(1,K)
	vbroadcastsd 8(%rdx), %zmm9 # T(2,K)
	
	vmovapd   (%rsi), %zmm0 # X(I,1,K)
	vmovapd 64(%rsi), %zmm1 # X(I,2,K)	
	vmovapd   (%rdi), %zmm2 # X(I,1,N/2-K)
	vmovapd 64(%rdi), %zmm3 # X(I,2,N/2-K)

	vaddpd %zmm2,%zmm0,%zmm4 # addr
	vmulpd %zmm12,%zmm4,%zmm4 # addr*0.5
	vsubpd %zmm0,%zmm2,%zmm0 # subr
	vmulpd %zmm12,%zmm0,%zmm0 # subr*0.5	
	vaddpd %zmm3,%zmm1,%zmm5 # addi
	vmulpd %zmm12,%zmm5,%zmm5 # addi*0.5	
	vsubpd %zmm1,%zmm3,%zmm1 # subi
	vmulpd %zmm12,%zmm1,%zmm1 # subi*0.5

##	vmulpd %zmm9,%zmm0,%zmm2 #subr*tr2
##	vsubpd %zmm2,%zmm4,%zmm2 #addr-subr*tr2
	vmovapd %zmm0,%zmm2
	vfnmadd213pd %zmm4,%zmm9,%zmm2
##
##	vmulpd %zmm8,%zmm5,%zmm3 #addi*tr1
##	vaddpd %zmm3,%zmm2,%zmm2 #addr-subr*tr2+addi*tr1
	vfmadd231pd %zmm5,%zmm8,%zmm2
##	
	vmovapd %zmm2,  (%rsi) # X(I,1,K)

##	vmulpd %zmm9,%zmm5,%zmm6 #addi*tr2
##	vsubpd %zmm6,%zmm1,%zmm6 #subi-addi*tr2
	vmovapd %zmm5,%zmm6
	vfnmadd213pd %zmm1,%zmm9,%zmm6
##
##	vmulpd %zmm8,%zmm0,%zmm7 #subr*tr1
##	vsubpd %zmm7,%zmm6,%zmm6 #subi-addi*tr2-subr*tr1
	vfnmadd231pd %zmm0,%zmm8,%zmm6
##
	vmovapd %zmm6,  64(%rsi) # X(I,2,K)

##	vmulpd %zmm11,%zmm4,%zmm4
##	vsubpd %zmm2,%zmm4,%zmm4
	vfmsub213pd %zmm2,%zmm11,%zmm4
##
	vmovapd %zmm4,(%rdi) # X(I,1,N/2-K)	

##	vmulpd %zmm11,%zmm1,%zmm1
##	vsubpd %zmm1,%zmm6,%zmm1
	vfnmadd213pd %zmm6,%zmm11,%zmm1
##	
	vmovapd %zmm1,64(%rdi) # X(I,2,N/2-K)	

	addq $16,%rdx
	addq $128,%rsi
	subq $128,%rdi
	cmpq %rdx,%r8
	jne L1
	
LE:
	
	ret

C2: # 倍精度不動小数点の 2
	.long   0x00000000,0x40000000
C05: # 倍精度不動小数点の 0.5
	.long   0x00000000,0x3fe00000
	
