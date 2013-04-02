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

       pushl	%ebx
       pushl	%esi       
       pushl	%edi       
       pushl	%ebp

       movl   20(%esp), %edi  # : JH �Υ١������ɥ쥹
       movl   32(%esp), %ecx  # : R �Υ١������ɥ쥹
       movhpd (%ecx), %xmm2 # R �� xmm2 �ξ�̤�
       movlpd (%ecx), %xmm2 # R �� xmm2 �β��̤ˤ�

       movl   36(%esp), %ecx  # : Y �Υ١������ɥ쥹
       movl   40(%esp), %eax  # : QA �Υ١������ɥ쥹       
       movl   44(%esp), %ebx  # : QB �Υ١������ɥ쥹              
       movl   48(%esp), %edx  # : W1 �Υ١������ɥ쥹
       movl   52(%esp), %ebp  # : W2 �Υ١������ɥ쥹
       
       pxor %xmm0,%xmm0
       pxor %xmm1,%xmm1
       
       movl (%edi),%edi # JH �� edi ��
       shll $3,%edi # JH*8 �� edi ��
       
       movl %ecx,%esi
       addl %edi,%esi
       
.align 16
.L0:
       movaps (%ecx), %xmm3 # Y
       movaps (%eax), %xmm4 # QA
       movaps (%ebx), %xmm5 # QB
       movaps (%edx), %xmm6 # W1
       movaps (%ebp), %xmm7 # W2       

       mulpd %xmm2,%xmm3 # Y*R
       mulpd %xmm4,%xmm3 # R*Y*QA
       addpd %xmm3,%xmm5 # �������줿 QB �� xmm5 ��
       
       mulpd %xmm4,%xmm6 # W1*QA
       addpd %xmm6,%xmm0 # S1=S1+W1*QA
       
       mulpd %xmm4,%xmm7 # W2*QA
       addpd %xmm7,%xmm1 # S2=S2+W2*QA
       
       movaps %xmm5,(%ebx)
       
       addl $16,%ecx
       addl $16,%eax
       addl $16,%ebx       
       addl $16,%edx       
       addl $16,%ebp
       cmpl %ecx,%esi
       jne .L0

       movl   24(%esp), %ecx  # : S1 �Υ١������ɥ쥹       
       movaps %xmm0,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm0
       movlpd %xmm0,(%ecx)  # xmm0 �β��̤� S ��

       movl   28(%esp), %ecx  # : S2 �Υ١������ɥ쥹       
       movaps %xmm1,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm1
       movlpd %xmm1,(%ecx)  # xmm1 �β��̤� S2 ��

#------------------------------------
       popl	%ebp
       popl	%edi       
       popl	%esi
       popl	%ebx       
	
       ret
