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
.globl ljlswg_
ljlswg_:

       movl   (%rdi), %edi  # : JH が rdi に
       movhpd (%rsi), %xmm0 # S1 を xmm0 の上位に
       movlpd (%rsi), %xmm0 # S1 を xmm0 の下位にも
       movhpd 8(%rsi), %xmm6 # S2 を xmm6 の上位に
       movlpd 8(%rsi), %xmm6 # S2 を xmm6 の下位にも
       movhpd (%rdx), %xmm1 # R を xmm1 の上位に
       movlpd (%rdx), %xmm1 # R を xmm1 の下位にも
       movq  8(%rsp), %r10  # : W1 のベースアドレス
       
       shlq $3,%rdi # JH*8 が rdi に
       
       pushq %rbx
       pushq %rbp
       
       movq %rdi,%rbp # JH*8 が rbp に
       
       xorq %rsi,%rsi
       
       movq %r10,%r11
       addq %rdi, %r11  # : W2 のベースアドレス
       
       movq %r8,%rax
       movq %r9,%rdx
       movq %r10,%rbx
       movq %r11,%rdi
       
       addq %rbp,%rax
       addq %rbp,%rdx       
       addq %rbp,%rbx              
       addq %rbp,%rdi                     
       addq %rbp,%rcx
       
       subq %rbp,%rsi
       
.align 16
.L0:
       movaps (%rcx,%rsi), %xmm4 # Y
       movaps (%rax,%rsi), %xmm2 # QA
       
       mulpd %xmm1,%xmm4 # Y*R
       mulpd %xmm2,%xmm4 # R*Y*QA
       movaps %xmm0, %xmm3        
       addpd (%rdx,%rsi),%xmm4 # 更新された QB が xmm4 に
       
       mulpd %xmm2,%xmm3 # S1*QA
       mulpd %xmm6,%xmm2 # S2*QA	       
       addpd (%rbx,%rsi),%xmm3 # 更新された W1 が xmm3 に
       addpd (%rdi,%rsi),%xmm2 # 更新された W2 が xmm2 に

       movaps %xmm4,(%rdx,%rsi)
       movaps %xmm3,(%rbx,%rsi)
       movaps %xmm2,(%rdi,%rsi)
       
       addq $16,%rsi
       jnz .L0
       
#------------------------------------

       popq %rbp
       popq %rbx

       ret
       
