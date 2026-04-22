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
.globl sxqtb2_
.globl _sxqtb2_	
sxqtb2_:
_sxqtb2_:	
      # rdi, rsi, rdx
	movq (%rdi),%rdi # rdi に IM
	shlq $3,%rdi # rdi に IM*8
	lea (%rdi,%rdi),%r8
	lea (%r8,%rdi),%r9
	
	lea (%rdx,%rdi),%rcx	
L0:	

	vmovapd   (%rsi), %ymm0
	vmovapd 32(%rsi), %ymm1
	vmovapd 64(%rsi), %ymm2
	vmovapd 96(%rsi), %ymm3
	vunpcklpd %ymm1,%ymm0,%ymm14
	vunpckhpd %ymm1,%ymm0,%ymm0
	vunpcklpd %ymm3,%ymm2,%ymm15	
	vunpckhpd %ymm3,%ymm2,%ymm2
	vperm2f128 $32,%ymm2,%ymm0,%ymm1
	vperm2f128 $49,%ymm2,%ymm0,%ymm3
	vperm2f128 $32,%ymm15,%ymm14,%ymm0
	vperm2f128 $49,%ymm15,%ymm14,%ymm2

	vmovapd %ymm0,(%rdx) # ymm0 空き
	vmovapd %ymm1,(%rdx,%rdi)
	vmovapd %ymm2,(%rdx,%r8)
	vmovapd %ymm3,(%rdx,%r9)
	
	addq $128,%rsi
	addq $32,%rdx	
	cmpq %rdx,%rcx	
	jne L0
       
	ret
