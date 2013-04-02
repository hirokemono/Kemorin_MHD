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

       movl   (%rdi), %edi  # : JH が rdi に
       movhpd (%rsi), %xmm0 # S を xmm0 の上位に
       movlpd (%rsi), %xmm0 # S を xmm0 の下位にも
       movhpd (%rdx), %xmm1 # R を xmm1 の上位に
       movlpd (%rdx), %xmm1 # R を xmm1 の下位にも
       movq  8(%rsp), %r10  # : W のベースアドレス
       
       shlq $3,%rdi # JH*8 が rdi に
       
       movq $0,%rsi
       subq %rdi,%rsi
       
       addq %rdi,%rcx
       addq %rdi,%r8
       addq %rdi,%r9       
       addq %rdi,%r10
       
       movq %r8,%rax
       movq %r9,%rdx
       movq %r10,%rdi

.align 16
.L0:
       movaps (%rcx,%rsi), %xmm4 # Y
       movaps (%rax,%rsi), %xmm2 # QA
       
       mulpd %xmm1,%xmm4 # Y*R
       mulpd %xmm2,%xmm4 # R*Y*QA
       addpd (%rdx,%rsi),%xmm4 # 更新された QB が xmm4 に
       
       mulpd %xmm0,%xmm2 # S*QA
       addpd (%rdi,%rsi),%xmm2 # 更新された W が xmm2 に
       movaps %xmm4,(%rdx,%rsi)                
       movaps %xmm2,(%rdi,%rsi)

       addq $16,%rsi
       jnz .L0
       
#------------------------------------

       ret
       
