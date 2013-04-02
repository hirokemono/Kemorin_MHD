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

       pushl	%ebx
       pushl	%esi       
       pushl	%edi       
       pushl	%ebp

       movl   20(%esp), %edi  # : JH のベースアドレス
       movl   24(%esp), %ecx  # : S のベースアドレス
       movhpd (%ecx), %xmm0 # S1 を xmm0 の上位に
       movlpd (%ecx), %xmm0 # S1 を xmm0 の下位にも
       movl   28(%esp), %ecx  # : S のベースアドレス
       movhpd (%ecx), %xmm1 # S2 を xmm1 の上位に
       movlpd (%ecx), %xmm1 # S2 を xmm1 の下位にも
       movl   32(%esp), %ecx  # : R のベースアドレス
       movhpd (%ecx), %xmm2 # R を xmm2 の上位に
       movlpd (%ecx), %xmm2 # R を xmm2 の下位にも

       movl   36(%esp), %ecx  # : Y のベースアドレス
       movl   40(%esp), %eax  # : QA のベースアドレス       
       movl   44(%esp), %ebx  # : QB のベースアドレス              
       movl   48(%esp), %edx  # : W1 のベースアドレス
       movl   52(%esp), %ebp  # : W2 のベースアドレス       
       
       movl (%edi),%edi # JH が edi に
       shll $3,%edi # JH*8 が edi に
       
       movl %ecx,%esi
       addl %edi,%esi
       
.align 16
.L0:
       movaps (%ecx), %xmm3 # Y
       movaps (%eax), %xmm4 # QA
       movaps (%ebx), %xmm5 # QB
       movaps %xmm0, %xmm7 # S1を xmm7 にも       
       movaps (%edx), %xmm6 # W1

       mulpd %xmm2,%xmm3 # Y*R
       mulpd %xmm4,%xmm3 # R*Y*QA
       addpd %xmm3,%xmm5 # 更新された QB が xmm5 に
       
       movaps (%ebp), %xmm3 # W2
       
       mulpd %xmm4,%xmm7 # S1*QA
       addpd %xmm7,%xmm6 # 更新された W1 が xmm6 に
       
       mulpd %xmm1,%xmm4 # S2*QA
       addpd %xmm4,%xmm3 # 更新された W2 が xmm3 に
       
       movaps %xmm5,(%ebx)       
       movaps %xmm6,(%edx)
       movaps %xmm3,(%ebp)       

       addl $16,%ecx
       addl $16,%eax
       addl $16,%ebx       
       addl $16,%edx       
       addl $16,%ebp
       cmpl %ecx,%esi
       jne .L0
       
#------------------------------------
       popl	%ebp
       popl	%edi       
       popl	%esi
       popl	%ebx       
	
       ret
