########################################################################
# ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
# Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
.globl ljnswg_
ljnswg_:

       movl   (%rdi), %edi  # : JH が rdi に
       movhpd (%rsi), %xmm0 # S1R を xmm0 の上位に
       movlpd (%rsi), %xmm0 # S1R を xmm0 の下位にも
       movhpd 8(%rsi), %xmm1 # S1R を xmm1 の上位に
       movlpd 8(%rsi), %xmm1 # S1R を xmm1 の下位にも
       movhpd (%rdx), %xmm2 # S2R を xmm2 の上位に
       movlpd (%rdx), %xmm2 # S2R を xmm2 の下位にも
       movhpd 8(%rdx), %xmm3 # S2R を xmm3 の上位に
       movlpd 8(%rdx), %xmm3 # S2R を xmm3 の下位にも
       movhpd (%rcx), %xmm4 # R を xmm4 の上位に
       movlpd (%rcx), %xmm4 # R を xmm4 の下位にも
       movq  8(%rsp), %r10  # : QB のベースアドレス       
       movq 16(%rsp), %r11  # : W1R のベースアドレス
       movq 24(%rsp), %rsi  # : W2R のベースアドレス                     
       
      # Y : r8, QA: r9, QB: r10, W1R: r11, W2R: rsi
      # W1I: rcx, W2I: rdi

       shlq $3,%rdi # JH*8 が rdi に
       
       movq %r8,%rdx
       addq %rdi,%rdx
       
       movq %r11,%rcx
       addq %rdi,%rcx
       
       addq %rsi,%rdi

.align 16
.L0:
       movaps (%r8), %xmm5 # Y
       movaps (%r9), %xmm6 # QA
       movaps (%r10), %xmm10 # QB をロード
       movaps %xmm0, %xmm7 # 
       movaps (%r11), %xmm11 # W1R	
       movaps %xmm1, %xmm8 # 
       movaps (%rcx), %xmm12 # W1I
       movaps %xmm2, %xmm9 # 
       movaps (%rsi), %xmm13 # W2R
       movaps (%rdi), %xmm14 # W2I	
       
       mulpd %xmm4,%xmm5 # Y*R
       mulpd %xmm6,%xmm5 # R*Y*QA
       addpd %xmm5,%xmm10 # 更新された QB が xmm5 に
       
       mulpd %xmm6,%xmm7 # S1R*QA
       addpd %xmm7,%xmm11 # 更新された W1R が xmm5 に
       
       mulpd %xmm6,%xmm8 # S1I*QA
       addpd %xmm8,%xmm12 # 更新された W1I が xmm5 に
       
       mulpd %xmm6,%xmm9 # S2R*QA
       addpd %xmm9,%xmm13 # 更新された W2R が xmm5 に

       mulpd %xmm3,%xmm6 # S2I*QA
       addpd %xmm6,%xmm14 # 更新された W1I が xmm5 に

       movaps %xmm10,(%r10) # 更新された QB をストア	
       movaps %xmm11,(%r11)	
       movaps %xmm12,(%rcx)	
       movaps %xmm13,(%rsi)	
       movaps %xmm14,(%rdi)

       addq $16,%r8       
       addq $16,%r9
       addq $16,%r10       
       addq $16,%r11
       addq $16,%rcx	
       addq $16,%rsi
       addq $16,%rdi
       cmpq %r8,%rdx
       jne .L0
       
       ret
       
