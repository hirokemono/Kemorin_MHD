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

       movl   (%rdi), %edi  # : JH �� rdi ��
       movhpd (%rsi), %xmm0 # S1 �� xmm0 �ξ�̤�
       movlpd (%rsi), %xmm0 # S1 �� xmm0 �β��̤ˤ�
       movhpd (%rdx), %xmm1 # S2 �� xmm1 �ξ�̤�
       movlpd (%rdx), %xmm1 # S2 �� xmm1 �β��̤ˤ�
       movhpd (%rcx), %xmm2 # R �� xmm1 �ξ�̤�
       movlpd (%rcx), %xmm2 # R �� xmm1 �β��̤ˤ�
       # %r8: Y, %r9: QA
       movq  8(%rsp), %r10  # : QB �Υ١������ɥ쥹       
       movq 16(%rsp), %r11  # : W1 �Υ١������ɥ쥹
       movq 24(%rsp), %rsi  # : W2 �Υ١������ɥ쥹                     
       
       shlq $3,%rdi # JH*8 �� rdi ��
       
       movq %r8,%rdx
       addq %rdi,%rdx
       
.align 16
.L0:
       movaps (%r8), %xmm3 # Y
       movaps (%r9), %xmm4  # QA
       movaps (%r10), %xmm5  # QB
       movaps (%r11), %xmm6 # W1
       movaps %xmm0, %xmm8 # S1�� xmm8 �ˤ�       
       movaps (%rsi), %xmm7 # W2      

       mulpd %xmm2,%xmm3 # Y*R
       mulpd %xmm4,%xmm3 # R*Y*QA
       addpd %xmm3,%xmm5 # �������줿 QB �� xmm3 ��
       
       mulpd %xmm4,%xmm8 # S1*QA
       addpd %xmm8,%xmm6 # �������줿 W1 �� xmm6 ��
       
       mulpd %xmm1,%xmm4 # S2*QA
       addpd %xmm4,%xmm7 # �������줿 W2 �� xmm3 ��
       
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
       
