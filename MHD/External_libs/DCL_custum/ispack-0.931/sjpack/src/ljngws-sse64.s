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
.globl ljngws_
ljngws_:

       pushq %r12

       movl   (%rdi), %edi  # : JH が rdi に
       movhpd (%rcx), %xmm4 # R を xmm4 の上位に
       movlpd (%rcx), %xmm4 # R を xmm4 の下位にも
       movq 16(%rsp), %r10  # : QB のベースアドレス       
       movq 24(%rsp), %r11  # : W1R のベースアドレス
       movq 32(%rsp), %r12  # : W2R のベースアドレス

       pxor %xmm0,%xmm0 # xmm0 を 0 にセット       
       pxor %xmm1,%xmm1 # xmm1 を 0 にセット              
       pxor %xmm2,%xmm2 # xmm2 を 0 にセット
       pxor %xmm3,%xmm3 # xmm3 を 0 にセット                     
       
       
      # Y : r8, QA: r9, QB: r10, W1R: r11, W2R: r12
      # W1I: rcx, W2I: rdi

       shlq $3,%rdi # JH*8 が rdi に
       
       movq %r8,%rax
       addq %rdi,%rax
       
       movq %r11,%rcx
       addq %rdi,%rcx
       
       addq %r12,%rdi

.align 16
.L0:
       movaps (%r8), %xmm5 # Y
       movaps (%r9), %xmm6 # QA
       movaps (%r10), %xmm10 # QB をロード
       movaps (%r11), %xmm11 # W1R	
       movaps (%rcx), %xmm12 # W1I
       movaps (%r12), %xmm13 # W2R
       movaps (%rdi), %xmm14 # W2I	
       
       mulpd %xmm4,%xmm5 # Y*R
       mulpd %xmm6,%xmm5 # R*Y*QA
       addpd %xmm5,%xmm10 # 更新された QB が xmm5 に

       mulpd %xmm6,%xmm11 # W1R*QA
       addpd %xmm11,%xmm0 # S1R=S1R+W1R*QA
       
       mulpd %xmm6,%xmm12 # W1I*QA
       addpd %xmm12,%xmm1 # S1I=S1I+W1I*QA

       mulpd %xmm6,%xmm13 # W2R*QA
       addpd %xmm13,%xmm2 # S2R=S2R+W2R*QA
       
       mulpd %xmm6,%xmm14 # W2I*QA
       addpd %xmm14,%xmm3 # S2I=S2I+W2I*QA

       movaps %xmm10,(%r10) # 更新された QB をストア	

       addq $16,%r8       
       addq $16,%r9
       addq $16,%r10       
       addq $16,%r11
       addq $16,%rcx	
       addq $16,%r12
       addq $16,%rdi
       cmpq %r8,%rax
       jne .L0
       
       movaps %xmm0,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm0
       movlpd %xmm0,(%rsi)  # xmm0 の下位を S1R に
       
       movaps %xmm1,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm1
       movlpd %xmm1,8(%rsi)  # xmm0 の下位を S1I に
       
       movaps %xmm2,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm2
       movlpd %xmm2,(%rdx)  # xmm0 の下位を S1R に
       
       movaps %xmm3,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       addpd %xmm7,%xmm3
       movlpd %xmm3,8(%rdx)  # xmm0 の下位を S1I に
       
       popq %r12       
       
       ret
       
