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

       movl   (%rdi), %edi  # : JH �� rdi ��
       movhpd (%rsi), %xmm0 # S1 �� xmm0 �ξ�̤�
       movlpd (%rsi), %xmm0 # S1 �� xmm0 �β��̤ˤ�
       movhpd 8(%rsi), %xmm6 # S2 �� xmm6 �ξ�̤�
       movlpd 8(%rsi), %xmm6 # S2 �� xmm6 �β��̤ˤ�
       movhpd (%rdx), %xmm1 # R �� xmm1 �ξ�̤�
       movlpd (%rdx), %xmm1 # R �� xmm1 �β��̤ˤ�
       movq  8(%rsp), %r10  # : W1 �Υ١������ɥ쥹
       
       shlq $3,%rdi # JH*8 �� rdi ��
       
       pushq %rbx
       pushq %rbp
       
       movq %rdi,%rbp # JH*8 �� rbp ��
       
       xorq %rsi,%rsi
       
       movq %r10,%r11
       addq %rdi, %r11  # : W2 �Υ١������ɥ쥹
       
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
       addpd (%rdx,%rsi),%xmm4 # �������줿 QB �� xmm4 ��
       
       mulpd %xmm2,%xmm3 # S1*QA
       mulpd %xmm6,%xmm2 # S2*QA	       
       addpd (%rbx,%rsi),%xmm3 # �������줿 W1 �� xmm3 ��
       addpd (%rdi,%rsi),%xmm2 # �������줿 W2 �� xmm2 ��

       movaps %xmm4,(%rdx,%rsi)
       movaps %xmm3,(%rbx,%rsi)
       movaps %xmm2,(%rdi,%rsi)
       
       addq $16,%rsi
       jnz .L0
       
#------------------------------------

       popq %rbp
       popq %rbx

       ret
       
