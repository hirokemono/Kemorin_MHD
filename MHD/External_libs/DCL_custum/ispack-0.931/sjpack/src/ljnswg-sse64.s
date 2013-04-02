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

       movl   (%rdi), %edi  # : JH �� rdi ��
       movhpd (%rsi), %xmm0 # S1R �� xmm0 �ξ�̤�
       movlpd (%rsi), %xmm0 # S1R �� xmm0 �β��̤ˤ�
       movhpd 8(%rsi), %xmm1 # S1R �� xmm1 �ξ�̤�
       movlpd 8(%rsi), %xmm1 # S1R �� xmm1 �β��̤ˤ�
       movhpd (%rdx), %xmm2 # S2R �� xmm2 �ξ�̤�
       movlpd (%rdx), %xmm2 # S2R �� xmm2 �β��̤ˤ�
       movhpd 8(%rdx), %xmm3 # S2R �� xmm3 �ξ�̤�
       movlpd 8(%rdx), %xmm3 # S2R �� xmm3 �β��̤ˤ�
       movhpd (%rcx), %xmm4 # R �� xmm4 �ξ�̤�
       movlpd (%rcx), %xmm4 # R �� xmm4 �β��̤ˤ�
       movq  8(%rsp), %r10  # : QB �Υ١������ɥ쥹       
       movq 16(%rsp), %r11  # : W1R �Υ١������ɥ쥹
       movq 24(%rsp), %rsi  # : W2R �Υ١������ɥ쥹                     
       
      # Y : r8, QA: r9, QB: r10, W1R: r11, W2R: rsi
      # W1I: rcx, W2I: rdi

       shlq $3,%rdi # JH*8 �� rdi ��
       
       movq %r8,%rdx
       addq %rdi,%rdx
       
       movq %r11,%rcx
       addq %rdi,%rcx
       
       addq %rsi,%rdi

.align 16
.L0:
       movaps (%r8), %xmm5 # Y
       movaps (%r9), %xmm6 # QA
       movaps (%r10), %xmm10 # QB �����
       movaps %xmm0, %xmm7 # 
       movaps (%r11), %xmm11 # W1R	
       movaps %xmm1, %xmm8 # 
       movaps (%rcx), %xmm12 # W1I
       movaps %xmm2, %xmm9 # 
       movaps (%rsi), %xmm13 # W2R
       movaps (%rdi), %xmm14 # W2I	
       
       mulpd %xmm4,%xmm5 # Y*R
       mulpd %xmm6,%xmm5 # R*Y*QA
       addpd %xmm5,%xmm10 # �������줿 QB �� xmm5 ��
       
       mulpd %xmm6,%xmm7 # S1R*QA
       addpd %xmm7,%xmm11 # �������줿 W1R �� xmm5 ��
       
       mulpd %xmm6,%xmm8 # S1I*QA
       addpd %xmm8,%xmm12 # �������줿 W1I �� xmm5 ��
       
       mulpd %xmm6,%xmm9 # S2R*QA
       addpd %xmm9,%xmm13 # �������줿 W2R �� xmm5 ��

       mulpd %xmm3,%xmm6 # S2I*QA
       addpd %xmm6,%xmm14 # �������줿 W1I �� xmm5 ��

       movaps %xmm10,(%r10) # �������줿 QB �򥹥ȥ�	
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
       
