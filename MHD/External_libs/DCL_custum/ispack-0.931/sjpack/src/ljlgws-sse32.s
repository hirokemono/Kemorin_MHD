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

       pushl	%ebx
       pushl	%esi       
       pushl	%edi       

       movl   16(%esp), %edi  # : JH �Υ١������ɥ쥹
       movl   24(%esp), %ecx  # : R �Υ١������ɥ쥹
       movhpd (%ecx), %xmm1 # R �� xmm1 �ξ�̤�
       movlpd (%ecx), %xmm1 # R �� xmm1 �β��̤ˤ�

       movl   28(%esp), %ecx  # : Y �Υ١������ɥ쥹
       movl   32(%esp), %eax  # : QA �Υ١������ɥ쥹       
       movl   36(%esp), %ebx  # : QB �Υ١������ɥ쥹              
       movl   40(%esp), %edx  # : W1 �Υ١������ɥ쥹
       
       pxor %xmm0,%xmm0 # xmm0 �� 0 �˥��å�       
       pxor %xmm6,%xmm6 # xmm6 �� 0 �˥��å�              
       
       movl (%edi),%edi # JH �� edi ��
       shll $3,%edi # JH*8 �� edi ��
       
       movl %ecx,%esi
       addl %edi,%esi
       
       addl %edx, %edi  # : W2 �Υ١������ɥ쥹       
       
.align 16
.L0:
       movaps (%ecx), %xmm4
       movaps (%eax), %xmm2
       movaps (%ebx), %xmm3
       movaps (%edx), %xmm5
       movaps (%edi), %xmm7       

       mulpd %xmm1,%xmm4 # Y*R
       mulpd %xmm2,%xmm4 # R*Y*QA
       addpd %xmm4,%xmm3 # �������줿 QB �� xmm3 ��

       mulpd %xmm2,%xmm5 # W1*QA
       addpd %xmm5,%xmm0 # S1=S1+W1*QA
       
       mulpd %xmm2,%xmm7 # W2*QA
       addpd %xmm7,%xmm6 # S2=S2+W2*QA

       movaps %xmm3,(%ebx)       

       addl $16,%ecx
       addl $16,%eax
       addl $16,%ebx       
       addl $16,%edx
       addl $16,%edi       
       cmpl %ecx,%esi
       jne .L0

       movl   20(%esp), %ecx  # : S1 �Υ١������ɥ쥹

       movaps %xmm0,%xmm1
       shufpd $0x1,%xmm1,%xmm1
       addpd %xmm1,%xmm0
       movlpd %xmm0,(%ecx)  # xmm0 �β��̤� S1 ��
       
       movaps %xmm6,%xmm1
       shufpd $0x1,%xmm1,%xmm1
       addpd %xmm1,%xmm6
       movlpd %xmm6,8(%ecx)  # xmm0 �β��̤� S2 ��

#------------------------------------
       popl	%edi       
       popl	%esi
       popl	%ebx       
	
       ret
       
