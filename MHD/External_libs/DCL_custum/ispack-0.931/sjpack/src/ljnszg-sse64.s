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
.globl ljnszg_
ljnszg_:

       movl   (%rdi), %edi  # : JH が rdi に
       movhpd (%rsi), %xmm0 # S1 を xmm0 の上位に
       movlpd (%rsi), %xmm0 # S1 を xmm0 の下位にも
       movhpd (%rdx), %xmm1 # S2 を xmm1 の上位に
       movlpd (%rdx), %xmm1 # S2 を xmm1 の下位にも
       movhpd (%rcx), %xmm2 # R を xmm1 の上位に
       movlpd (%rcx), %xmm2 # R を xmm1 の下位にも
       # %r8: Y, %r9: QA
       movq  8(%rsp), %r10  # : QB のベースアドレス       
       movq 16(%rsp), %r11  # : W1 のベースアドレス
       movq 24(%rsp), %rsi  # : W2 のベースアドレス                     
       
       shlq $3,%rdi # JH*8 が rdi に
       
       movq %r8,%rdx
       addq %rdi,%rdx
       
.align 16
.L0:
       movaps (%r8), %xmm3 # Y
       movaps (%r9), %xmm4  # QA
       movaps (%r10), %xmm5  # QB
       movaps (%r11), %xmm6 # W1
       movaps %xmm0, %xmm8 # S1を xmm8 にも       
       movaps (%rsi), %xmm7 # W2      

       mulpd %xmm2,%xmm3 # Y*R
       mulpd %xmm4,%xmm3 # R*Y*QA
       addpd %xmm3,%xmm5 # 更新された QB が xmm3 に
       
       mulpd %xmm4,%xmm8 # S1*QA
       addpd %xmm8,%xmm6 # 更新された W1 が xmm6 に
       
       mulpd %xmm1,%xmm4 # S2*QA
       addpd %xmm4,%xmm7 # 更新された W2 が xmm3 に
       
       movaps %xmm5,(%r10)  
       movaps %xmm6,(%r11)
       movaps %xmm7,(%rsi)       
       
       addq $16,%r8
       addq $16,%r9       
       addq $16,%r10
       addq $16,%r11
       addq $16,%rsi       
       cmpq %r8,%rdx
       jne .L0
       
#------------------------------------

       ret
       
