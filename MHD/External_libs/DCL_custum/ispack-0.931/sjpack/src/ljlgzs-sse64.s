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

       movl   (%rdi), %edi  # : JH が rdi に
       movhpd (%rdx), %xmm1 # R を xmm1 の上位に
       movlpd (%rdx), %xmm1 # R を xmm1 の下位にも
       movq  8(%rsp), %r10  # : W のベースアドレス
       
       pushq %rbx
       
       pxor %xmm0,%xmm0 # xmm0 を 0 にセット       
       
       shlq $3,%rdi # JH*8 が rdi に
       
       movq $0,%rdx
       subq %rdi,%rdx
       
       addq %rdi,%rcx
       addq %rdi,%r8       
       addq %rdi,%r9
       addq %rdi,%r10
       
       movq %r8,%rax
       movq %r9,%rdi
       movq %r10,%rbx
       
.align 16
.L0:
       movaps (%rcx,%rdx), %xmm4 # Y
       movaps (%rax,%rdx), %xmm2 # QA

       mulpd %xmm1,%xmm4 # Y*R
       mulpd %xmm2,%xmm4 # R*Y*QA
       addpd (%rdi,%rdx),%xmm4 # 更新された QB が xmm4 に
       movaps %xmm4,(%rdi,%rdx)  

       mulpd (%rbx,%rdx),%xmm2 # W*QA
       addpd %xmm2,%xmm0 # S=S+W*QA
       
       addq $16,%rdx
       jnz .L0
       
       movaps %xmm0,%xmm1
       shufpd $0x1,%xmm1,%xmm1
       addpd %xmm1,%xmm0
       movlpd %xmm0,(%rsi)  # xmm0 の下位を S に
       
#------------------------------------

       popq %rbx

       ret
       
