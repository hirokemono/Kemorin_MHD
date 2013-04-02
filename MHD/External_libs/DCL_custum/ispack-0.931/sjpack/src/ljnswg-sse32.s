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

       pushl	%ebx
       pushl	%esi       
       pushl	%edi       
       pushl	%ebp              

       movl   20(%esp), %edi  # : JH �Υ١������ɥ쥹
       movl   24(%esp), %ecx  # : S1 �Υ١������ɥ쥹
       movhpd (%ecx), %xmm0 # S1R �� xmm0 �ξ�̤�
       movlpd (%ecx), %xmm0 # S1R �� xmm0 �β��̤ˤ�
       movhpd 8(%ecx), %xmm1 # S1R �� xmm1 �ξ�̤�
       movlpd 8(%ecx), %xmm1 # S1R �� xmm1 �β��̤ˤ�
       movl   32(%esp), %ecx  # : R �Υ١������ɥ쥹
       movhpd (%ecx), %xmm2 # R �� xmm2 �ξ�̤�
       movlpd (%ecx), %xmm2 # R �� xmm2 �β��̤ˤ�

       movl   36(%esp), %ecx  # : Y �Υ١������ɥ쥹
       movl   40(%esp), %eax  # : QA �Υ١������ɥ쥹       
       movl   44(%esp), %ebx  # : QB �Υ١������ɥ쥹              
       movl   48(%esp), %edx  # : W1R �Υ١������ɥ쥹

       movl (%edi),%edi # JH �� edi ��
       shll $3,%edi # JH*8 �� edi ��
       
       movl %ecx,%esi
       addl %edi,%esi
       
       movl %edx,%ebp
       addl %edi,%ebp   # W1I �Υ١������ɥ쥹

.align 16
.L0:
       movaps (%ecx), %xmm3 # Y
       movaps (%eax), %xmm4 # QA
       movaps (%ebx), %xmm5 # QB �����
       movaps (%edx), %xmm6 # W1R
       
       movaps %xmm0, %xmm7 # S1R �� xmm7 �ˤ�       

       mulpd %xmm2,%xmm3 # Y*R
       mulpd %xmm4,%xmm3 # R*Y*QA
       addpd %xmm3,%xmm5 # �������줿 QB �� xmm5 ��
       movaps (%ebp), %xmm3 # W1I       
       movaps %xmm5,(%ebx) # �������줿 QB �򥹥ȥ�
       
       mulpd %xmm4,%xmm7 # S1R*QA
       addpd %xmm7,%xmm6 # �������줿 W1R �� xmm6 ��
       movaps %xmm6,(%edx)
       
       mulpd %xmm1,%xmm4 # S1I*QA
       addpd %xmm4,%xmm3 # �������줿 W1I �� xmm5 ��
       movaps %xmm3,(%ebp)

       addl $16,%ecx
       addl $16,%eax
       addl $16,%ebx       
       addl $16,%edx
       addl $16,%ebp
       cmpl %ecx,%esi
       jne .L0

#-----------------------------
       movl   28(%esp), %ecx  # : S2 �Υ١������ɥ쥹       
       movhpd (%ecx), %xmm0 # S2R �� xmm2 �ξ�̤�
       movlpd (%ecx), %xmm0 # S2R �� xmm2 �β��̤ˤ�
       movhpd 8(%ecx), %xmm1 # S2R �� xmm3 �ξ�̤�
       movlpd 8(%ecx), %xmm1 # S2R �� xmm3 �β��̤ˤ�
       movl   52(%esp), %edx  # : W2R �Υ١������ɥ쥹
       
       subl %edi, %eax # QA �Υ١������ɥ쥹
       
       movl %edx,%ebp
       addl %edi,%ebp   # W2I �Υ١������ɥ쥹

       movl %eax,%esi
       addl %edi,%esi

.L1:
       movaps %xmm0,%xmm5
       movaps (%eax), %xmm4 # QA
       movaps (%edx), %xmm6 # W1R       
       movaps (%ebp), %xmm3 # W1I
       
       mulpd %xmm4,%xmm5 # S2R*QA
       addpd %xmm5,%xmm6 # �������줿 W1R �� xmm5 ��
       movaps %xmm6,(%edx)
       
       mulpd %xmm1,%xmm4 # S2I*QA
       addpd %xmm4,%xmm3 # �������줿 W1I �� xmm5 ��
       movaps %xmm3,(%ebp)
       
       addl $16,%eax
       addl $16,%edx
       addl $16,%ebp
       cmpl %eax,%esi
       jne .L1
       
#------------------------------------
       popl	%ebp
       popl	%edi       
       popl	%esi
       popl	%ebx       
	
       ret
       
