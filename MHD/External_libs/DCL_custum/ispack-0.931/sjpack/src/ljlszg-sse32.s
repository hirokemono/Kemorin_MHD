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
.globl ljlszg_
ljlszg_:

       pushl	%ebx
       pushl	%esi       
       pushl	%edi       

       movl   16(%esp), %edi  # : JH �Υ١������ɥ쥹
       movl   20(%esp), %ecx  # : S �Υ١������ɥ쥹
       movhpd (%ecx), %xmm0 # S �� xmm0 �ξ�̤�
       movlpd (%ecx), %xmm0 # S �� xmm0 �β��̤ˤ�
       movl   24(%esp), %ecx  # : R �Υ١������ɥ쥹
       movhpd (%ecx), %xmm1 # R �� xmm1 �ξ�̤�
       movlpd (%ecx), %xmm1 # R �� xmm1 �β��̤ˤ�

       movl   28(%esp), %ecx  # : Y �Υ١������ɥ쥹
       movl   32(%esp), %eax  # : QA �Υ١������ɥ쥹       
       movl   36(%esp), %ebx  # : QB �Υ١������ɥ쥹              
       movl   40(%esp), %edx  # : W �Υ١������ɥ쥹
       
       movl (%edi),%edi # JH �� edi ��
       shll $3,%edi # JH*8 �� edi ��
       
       movl %ecx,%esi
       addl %edi,%esi
       
.align 16
.L0:
       movaps (%ecx), %xmm4
       movaps (%eax), %xmm2
       movaps (%ebx), %xmm3
       movaps (%edx), %xmm5

       mulpd %xmm1,%xmm4 # Y*R
       mulpd %xmm2,%xmm4 # R*Y*QA
       addpd %xmm4,%xmm3 # �������줿 QB �� xmm3 ��
       
       mulpd %xmm0,%xmm2 # S*QA
       addpd %xmm2,%xmm5 # �������줿 W �� xmm5 ��
       
       movaps %xmm3,(%ebx)       
       movaps %xmm5,(%edx)

       addl $16,%ecx
       addl $16,%eax
       addl $16,%ebx       
       addl $16,%edx       
       cmpl %ecx,%esi
       jne .L0
       
#------------------------------------
       popl	%edi       
       popl	%esi
       popl	%ebx       
	
       ret
