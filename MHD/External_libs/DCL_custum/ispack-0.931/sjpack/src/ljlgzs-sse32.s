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
.globl ljlgzs_
ljlgzs_:

       pushl	%ebx
       pushl	%esi       
       pushl	%edi       

       movl   16(%esp), %edi  # : JH のベースアドレス
       movl   24(%esp), %ecx  # : R のベースアドレス
       movhpd (%ecx), %xmm1 # R を xmm1 の上位に
       movlpd (%ecx), %xmm1 # R を xmm1 の下位にも
       movl   28(%esp), %ecx  # : Y のベースアドレス
       movl   32(%esp), %eax  # : QA のベースアドレス       
       movl   36(%esp), %ebx  # : QB のベースアドレス              
       movl   40(%esp), %edx  # : W のベースアドレス
       
       movl (%edi),%edi # JH が edi に
       shll $3,%edi # JH*8 が edi に
       
       movl %ecx,%esi
       addl %edi,%esi
       
       pxor %xmm0,%xmm0 # xmm0 を 0 にセット       
       
.align 16
.L0:
       movaps (%ecx), %xmm4
       movaps (%eax), %xmm2
       movaps (%ebx), %xmm3
       movaps (%edx), %xmm5

       mulpd %xmm1,%xmm4 # Y*R
       mulpd %xmm2,%xmm4 # R*Y*QA
       addpd %xmm4,%xmm3 # 更新された QB が xmm3 に
       
       mulpd %xmm5,%xmm2 # W*QA
       addpd %xmm2,%xmm0 # S=S+W*QA
       
       movaps %xmm3,(%ebx)       

       addl $16,%ecx
       addl $16,%eax
       addl $16,%ebx       
       addl $16,%edx       
       cmpl %ecx,%esi
       jne .L0

       movl   20(%esp), %ecx  # : S のベースアドレス       
       movaps %xmm0,%xmm1
       shufpd $0x1,%xmm1,%xmm1
       addpd %xmm1,%xmm0
       movlpd %xmm0,(%ecx)  # xmm0 の下位を S に

#------------------------------------
       popl	%edi       
       popl	%esi
       popl	%ebx       
	
       ret
       
