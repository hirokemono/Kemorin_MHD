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
.globl ljngzs_
ljngzs_:

       movl   (%rdi), %edi  # : JH �� rdi ��
       movhpd (%rcx), %xmm4 # R �� xmm4 �ξ�̤�
       movlpd (%rcx), %xmm4 # R �� xmm4 �β��̤ˤ�
       movq  8(%rsp), %r10  # : QB �Υ١������ɥ쥹       
       movq 16(%rsp), %r11  # : W1 �Υ١������ɥ쥹
       movq 24(%rsp), %rcx  # : W2 �Υ١������ɥ쥹

       pxor %xmm0,%xmm0 # xmm0 �� 0 �˥��å�       
       pxor %xmm1,%xmm1 # xmm1 �� 0 �˥��å�              
       
      # Y : r8, QA: r9, QB: r10, W1: r11, W2: rcx

       shlq $3,%rdi # JH*8 �� rdi ��
       
       movq %r8,%rax
       addq %rdi,%rax
       
.align 16
.L0:
       movaps (%r8), %xmm5 # Y
       movaps (%r9), %xmm6 # QA
       movaps (%r10), %xmm10 # QB �����
       movaps (%r11), %xmm11 # W1
       movaps (%rcx), %xmm12 # W2
       
       mulpd %xmm4,%xmm5 # Y*R
       mulpd %xmm6,%xmm5 # R*Y*QA
       addpd %xmm5,%xmm10 # �������줿 QB �� xmm5 ��

       mulpd %xmm6,%xmm11 # W1*QA
       addpd %xmm11,%xmm0 # S1=S1+W1*QA
       
       mulpd %xmm6,%xmm12 # W2*QA
       addpd %xmm12,%xmm1 # S2=S2+W2*QA

       movaps %xmm10,(%r10) # �������줿 QB �򥹥ȥ�	

       addq $16,%r8       
       addq $16,%r9
       addq $16,%r10       
       addq $16,%r11
       addq $16,%rcx	
       cmpq %r8,%rax
       jne .L0
       
       movaps %xmm0,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm0
       movlpd %xmm0,(%rsi)  # xmm0 �β��̤� S1 ��
       
       movaps %xmm1,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm1
       movlpd %xmm1,(%rdx)  # xmm1 �β��̤� S2 ��

       ret
       
