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
.globl ljlgws_
ljlgws_:

       movl   (%rdi), %edi  # : JH が rdi に
       movhpd (%rdx), %xmm1 # R を xmm1 の上位に
       movlpd (%rdx), %xmm1 # R を xmm1 の下位にも
       movq  8(%rsp), %r10  # : W1 のベースアドレス
       
       pushq %rbx
       pushq %rbp
       
       shlq $3,%rdi # JH*8 が rdi に
       
       movq $0,%rdx
       
       movq %r10,%r11
       addq %rdi,%r11 # : W2 のベースアドレス
       
       pxor %xmm0,%xmm0 # xmm0 を 0 にセット       
       pxor %xmm6,%xmm6 # xmm6 を 0 にセット
       
       movq %rdi,%rbp
       
       movq %r8,%rax
       movq %r10,%rbx
       movq %r11,%rdi
       
       subq %rbp,%rdx
       addq %rbp,%rcx
       addq %rbp,%rax
       addq %rbp,%r9
       addq %rbp,%rbx
       addq %rbp,%rdi
       
       movq %r9,%rbp
       movq %rsi,%r9
       movq %rbp,%rsi

.align 16
.L0:
       movaps (%rax,%rdx), %xmm2  # QA
       movaps (%rbx,%rdx), %xmm5 # W1
       movaps (%rdi,%rdx),%xmm3 # W2
       
       mulpd %xmm2,%xmm5 # W1*QA
       addpd %xmm5,%xmm0 # S1=S1+W1*QA
       mulpd %xmm2,%xmm3 # W2*QA
       addpd %xmm3,%xmm6 # S2=S2+W2*QA
       
       mulpd %xmm1,%xmm2 # R*QA       
       mulpd (%rcx,%rdx), %xmm2 # Y*R*QA
       
       addpd (%rsi,%rdx),%xmm2 # 更新された QB が xmm2 に
       movaps %xmm2,(%rsi,%rdx)
       
       addq $16,%rdx
       jnz .L0
       
       movaps %xmm0,%xmm1
       shufpd $0x1,%xmm1,%xmm1
       addpd %xmm1,%xmm0
       movlpd %xmm0,(%r9)  # xmm0 の下位を S1 に
       
       movaps %xmm6,%xmm1
       shufpd $0x1,%xmm1,%xmm1
       addpd %xmm1,%xmm6
       movlpd %xmm6,8(%r9)  # xmm0 の下位を S2 に
       
#------------------------------------
       popq %rbp
       popq %rbx

       ret
