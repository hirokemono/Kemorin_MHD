########################################################################
# FTTJ:  An FFT library
# Copyright (C) 2008 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
# fft 256 out-of-place backward (16x16(split) type)
.text
.globl fjcob8_
fjcob8_:

       movl    4(%esp), %eax
       movl    8(%esp), %ecx  # : ZD のベースアドレス
       movl   16(%esp), %edx  # : ZT のベースアドレス
       pushl	%esi
       movl   16(%esp), %esi  # : ZDD のベースアドレス       
       
       pushl	%ebx
       
#------------------------------------
# fftj16 のループ(前処理の方)
#------------------------------------

       movl %eax,%ebx
       addl $256,%ebx
       
########################################       

       movaps 256(%eax), %xmm0
       movaps 2304(%eax), %xmm1       
       movaps 1280(%eax), %xmm2       
       movaps 3328(%eax), %xmm3

       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0
       addpd %xmm7,%xmm1

       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2
       addpd %xmm7,%xmm3

       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1
       addpd %xmm7,%xmm3

       movaps %xmm3,16(%esi)
       
#-------------

       movaps 768(%eax), %xmm5
       movaps 2816(%eax), %xmm3
       movaps 1792(%eax), %xmm6              
       movaps 3840(%eax), %xmm4

       movaps %xmm5,%xmm7
       subpd %xmm3,%xmm5
       addpd %xmm7,%xmm3

       movaps %xmm6,%xmm7
       subpd %xmm4,%xmm6
       addpd %xmm7,%xmm4

       movaps %xmm3,%xmm7
       subpd %xmm4,%xmm3
       addpd %xmm7,%xmm4

       movaps %xmm1,48(%esi)
       movaps %xmm3,176(%esi)       
       movaps %xmm4,144(%esi)              

#-------------

       movaps %xmm0,%xmm7
       subpd %xmm6,%xmm0
       addpd %xmm7,%xmm6

       movaps %xmm5,%xmm7
       subpd %xmm2,%xmm5
       addpd %xmm7,%xmm2

       movaps .C1,%xmm1
       movaps %xmm0,%xmm7
       mulpd %xmm1,%xmm0
       movaps .S1,%xmm3
       movaps %xmm5,%xmm4
       mulpd %xmm3,%xmm5
       addpd %xmm5,%xmm0
       movaps %xmm0,112(%esi)
       
       mulpd %xmm3,%xmm7
       mulpd %xmm1,%xmm4
       subpd %xmm4,%xmm7
       movaps %xmm7,80(%esi)
       
       movaps %xmm6,%xmm7
       movaps %xmm2,%xmm4
       mulpd %xmm3,%xmm6
       mulpd %xmm1,%xmm2
       addpd %xmm2,%xmm6
       movaps %xmm6,240(%esi)       
       

       mulpd %xmm1,%xmm7
       mulpd %xmm3,%xmm4
       subpd %xmm4,%xmm7
       movaps %xmm7,208(%esi)
       
#-----------------------------------

       movaps (%eax), %xmm0
       movaps 2048(%eax), %xmm3
       movaps %xmm0,%xmm7
       subpd %xmm3,%xmm0       
       addpd %xmm7,%xmm3

       movaps 1024(%eax), %xmm2
       movaps 3072(%eax), %xmm1
       movaps %xmm2,%xmm7
       subpd %xmm1,%xmm2
       addpd %xmm7,%xmm1
       
       movaps %xmm3,%xmm7
       subpd %xmm1,%xmm3
       addpd %xmm7,%xmm1
       
       movaps %xmm1,(%esi)
       
#-----------------------------------

       movaps 512(%eax), %xmm5
       movaps 2560(%eax), %xmm4
       movaps 1536(%eax), %xmm6              
       movaps 3584(%eax), %xmm1

       movaps %xmm5,%xmm7
       subpd %xmm4,%xmm5
       addpd %xmm7,%xmm4
       
       movaps %xmm6,%xmm7
       subpd %xmm1,%xmm6
       addpd %xmm7,%xmm1
       
       movaps %xmm4,%xmm7
       subpd %xmm1,%xmm4
       addpd %xmm7,%xmm1
       
       movaps %xmm5,%xmm7
       subpd %xmm6,%xmm5
       addpd %xmm7,%xmm6
       
       movaps %xmm1,128(%esi)
       
#-----------------------------------

       movaps .C2,%xmm1
       mulpd %xmm1,%xmm5
       mulpd %xmm1,%xmm6       
       movaps %xmm0,%xmm7
       subpd %xmm5,%xmm0
       addpd %xmm7,%xmm5

       movaps %xmm2,%xmm7
       subpd %xmm6,%xmm2
       addpd %xmm7,%xmm6
       
       movaps %xmm0,64(%esi)
       movaps %xmm2,192(%esi)
       
#-----------------------------------

       movaps 48(%esi), %xmm2
       movaps 176(%esi), %xmm7       
       
       mulpd %xmm1,%xmm2
       mulpd %xmm1,%xmm7
       
       movaps .CI,%xmm1

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm1,%xmm7

       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm1,%xmm4

       movaps %xmm3,%xmm0
       subpd %xmm2,%xmm3
       addpd %xmm0,%xmm2

       movaps %xmm7,%xmm0
       subpd %xmm4,%xmm7
       addpd %xmm0,%xmm4
       
       movaps %xmm2,%xmm0
       subpd %xmm4,%xmm2
       addpd %xmm0,%xmm4

       movaps %xmm3,%xmm0
       subpd %xmm7,%xmm3
       addpd %xmm0,%xmm7
       
       movaps %xmm4,32(%esi)              
       movaps %xmm7,96(%esi)
       movaps %xmm3,160(%esi)
       movaps %xmm2,224(%esi)       
       
#-----------------------------------       

       movaps 112(%esi), %xmm4
       movaps 240(%esi), %xmm3
       movaps 16(%esi), %xmm2
       movaps 144(%esi), %xmm7              
       
       movaps %xmm5,%xmm0
       subpd %xmm4,%xmm5
       addpd %xmm0,%xmm4
       
       movaps %xmm3,%xmm0
       subpd %xmm6,%xmm3
       addpd %xmm0,%xmm6
       shufpd $0x1,%xmm3,%xmm3
       xorpd %xmm1,%xmm3
       shufpd $0x1,%xmm6,%xmm6
       xorpd %xmm1,%xmm6

       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4
       addpd %xmm0,%xmm6
       
       movaps %xmm5,%xmm0
       subpd %xmm3,%xmm5
       addpd %xmm0,%xmm3
       
       movaps %xmm6,16(%esi)
       movaps %xmm3,112(%esi)
       movaps %xmm5,144(%esi)
       movaps %xmm4,240(%esi)       
       
#-----------------------------------

       movaps (%esi), %xmm3
       movaps 128(%esi), %xmm5       
       movaps 64(%esi), %xmm4
       movaps 192(%esi), %xmm6

       movaps %xmm3,%xmm0
       subpd %xmm5,%xmm3
       addpd %xmm0,%xmm5

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7

       shufpd $0x1,%xmm2,%xmm2
       xorpd %xmm1,%xmm2

       movaps %xmm5,%xmm0
       subpd %xmm7,%xmm5
       addpd %xmm0,%xmm7
       
       movaps %xmm3,%xmm0
       subpd %xmm2,%xmm3
       addpd %xmm0,%xmm2
       
       movaps %xmm7,(%esi)
       movaps %xmm2,64(%esi)
       movaps %xmm5,128(%esi)       
       movaps %xmm3,192(%esi)
       
#-----------------------------------       
       
       movaps 80(%esi), %xmm3
       movaps 208(%esi), %xmm5
       
       movaps %xmm4,%xmm0
       subpd %xmm3,%xmm4
       addpd %xmm0,%xmm3

       movaps %xmm5,%xmm0
       subpd %xmm6,%xmm5
       addpd %xmm0,%xmm6

       shufpd $0x1,%xmm5,%xmm5
       xorpd %xmm1,%xmm5
       shufpd $0x1,%xmm6,%xmm6
       xorpd %xmm1,%xmm6
       
       movaps %xmm3,%xmm0
       subpd %xmm5,%xmm3
       addpd %xmm0,%xmm5
       
       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4
       addpd %xmm0,%xmm6
       
       movaps %xmm5,48(%esi)
       movaps %xmm6,80(%esi)
       movaps %xmm4,176(%esi)
       movaps %xmm3,208(%esi)              
       
       addl $16,%eax
       addl $256,%esi
       
########################################
.align 16       
.L0:
       addl $512,%edx

       movaps 256(%eax), %xmm0
       movaps 2304(%eax), %xmm1       
       movaps 1280(%eax), %xmm2       
       movaps 3328(%eax), %xmm3

       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0
       addpd %xmm7,%xmm1

       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2
       addpd %xmm7,%xmm3

       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1
       addpd %xmm7,%xmm3

       movaps %xmm3,16(%esi)
       
#-------------

       movaps 768(%eax), %xmm5
       movaps 2816(%eax), %xmm3
       movaps 1792(%eax), %xmm6              
       movaps 3840(%eax), %xmm4

       movaps %xmm5,%xmm7
       subpd %xmm3,%xmm5
       addpd %xmm7,%xmm3

       movaps %xmm6,%xmm7
       subpd %xmm4,%xmm6
       addpd %xmm7,%xmm4

       movaps %xmm3,%xmm7
       subpd %xmm4,%xmm3
       addpd %xmm7,%xmm4

       movaps %xmm1,48(%esi)
       movaps %xmm3,176(%esi)       
       movaps %xmm4,144(%esi)              

#-------------

       movaps %xmm0,%xmm7
       subpd %xmm6,%xmm0
       addpd %xmm7,%xmm6

       movaps %xmm5,%xmm7
       subpd %xmm2,%xmm5
       addpd %xmm7,%xmm2

       movaps .C1,%xmm1
       movaps %xmm0,%xmm7
       mulpd %xmm1,%xmm0
       movaps .S1,%xmm3
       movaps %xmm5,%xmm4
       mulpd %xmm3,%xmm5
       addpd %xmm5,%xmm0
       movaps %xmm0,112(%esi)
       
       mulpd %xmm3,%xmm7
       mulpd %xmm1,%xmm4
       subpd %xmm4,%xmm7
       movaps %xmm7,80(%esi)
       
       movaps %xmm6,%xmm7
       movaps %xmm2,%xmm4
       mulpd %xmm3,%xmm6
       mulpd %xmm1,%xmm2
       addpd %xmm2,%xmm6
       movaps %xmm6,240(%esi)       
       

       mulpd %xmm1,%xmm7
       mulpd %xmm3,%xmm4
       subpd %xmm4,%xmm7
       movaps %xmm7,208(%esi)
       
#-----------------------------------

       movaps (%eax), %xmm0
       movaps 2048(%eax), %xmm3
       movaps %xmm0,%xmm7
       subpd %xmm3,%xmm0       
       addpd %xmm7,%xmm3

       movaps 1024(%eax), %xmm2
       movaps 3072(%eax), %xmm1
       movaps %xmm2,%xmm7
       subpd %xmm1,%xmm2
       addpd %xmm7,%xmm1
       
       movaps %xmm3,%xmm7
       subpd %xmm1,%xmm3
       addpd %xmm7,%xmm1
       
       movaps %xmm1,(%esi)
       
#-----------------------------------

       movaps 512(%eax), %xmm5
       movaps 2560(%eax), %xmm4
       movaps 1536(%eax), %xmm6              
       movaps 3584(%eax), %xmm1

       movaps %xmm5,%xmm7
       subpd %xmm4,%xmm5
       addpd %xmm7,%xmm4
       
       movaps %xmm6,%xmm7
       subpd %xmm1,%xmm6
       addpd %xmm7,%xmm1
       
       movaps %xmm4,%xmm7
       subpd %xmm1,%xmm4
       addpd %xmm7,%xmm1
       
       movaps %xmm5,%xmm7
       subpd %xmm6,%xmm5
       addpd %xmm7,%xmm6
       
       movaps %xmm1,128(%esi)
       
#-----------------------------------

       movaps .C2,%xmm1
       mulpd %xmm1,%xmm5
       mulpd %xmm1,%xmm6       
       movaps %xmm0,%xmm7
       subpd %xmm5,%xmm0
       addpd %xmm7,%xmm5

       movaps %xmm2,%xmm7
       subpd %xmm6,%xmm2
       addpd %xmm7,%xmm6
       
       movaps %xmm0,64(%esi)
       movaps %xmm2,192(%esi)
       
#-----------------------------------

       movaps 48(%esi), %xmm2
       movaps 176(%esi), %xmm7       
       
       mulpd %xmm1,%xmm2
       mulpd %xmm1,%xmm7
       
       movaps .CI,%xmm1

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7
       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm1,%xmm7

       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm1,%xmm4

       movaps %xmm3,%xmm0
       subpd %xmm2,%xmm3
       addpd %xmm0,%xmm2

       movaps %xmm7,%xmm0
       subpd %xmm4,%xmm7
       addpd %xmm0,%xmm4
       
       movaps %xmm2,%xmm0
       subpd %xmm4,%xmm2
       addpd %xmm0,%xmm4

       movaps %xmm3,%xmm0
       subpd %xmm7,%xmm3
       addpd %xmm0,%xmm7
       
       movaps %xmm4,%xmm0
       shufpd $0x1,%xmm4,%xmm4
       mulpd 64(%edx),%xmm0       
       mulpd 80(%edx),%xmm4
       addpd %xmm0,%xmm4       

       movaps %xmm7,%xmm0
       shufpd $0x1,%xmm7,%xmm7
       mulpd 192(%edx),%xmm0       
       mulpd 208(%edx),%xmm7
       addpd %xmm0,%xmm7

       movaps %xmm3,%xmm0
       shufpd $0x1,%xmm3,%xmm3
       mulpd 320(%edx),%xmm0       
       mulpd 336(%edx),%xmm3
       addpd %xmm0,%xmm3

       movaps %xmm2,%xmm0
       shufpd $0x1,%xmm2,%xmm2
       mulpd 448(%edx),%xmm0
       mulpd 464(%edx),%xmm2
       addpd %xmm0,%xmm2
       
       movaps %xmm4,32(%esi)              
       movaps %xmm7,96(%esi)
       movaps %xmm3,160(%esi)
       movaps %xmm2,224(%esi)       
       
#-----------------------------------       

       movaps 112(%esi), %xmm4
       movaps 240(%esi), %xmm3
       movaps 16(%esi), %xmm2
       movaps 144(%esi), %xmm7              
       
       movaps %xmm5,%xmm0
       subpd %xmm4,%xmm5
       addpd %xmm0,%xmm4
       
       movaps %xmm3,%xmm0
       subpd %xmm6,%xmm3
       addpd %xmm0,%xmm6
       shufpd $0x1,%xmm3,%xmm3
       xorpd %xmm1,%xmm3
       shufpd $0x1,%xmm6,%xmm6
       xorpd %xmm1,%xmm6

       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4
       addpd %xmm0,%xmm6
       
       movaps %xmm5,%xmm0
       subpd %xmm3,%xmm5
       addpd %xmm0,%xmm3
       
       movaps %xmm6,%xmm0
       shufpd $0x1,%xmm6,%xmm6
       mulpd 32(%edx),%xmm0       
       mulpd 48(%edx),%xmm6
       addpd %xmm0,%xmm6       
       
       movaps %xmm3,%xmm0
       shufpd $0x1,%xmm3,%xmm3
       mulpd 224(%edx),%xmm0       
       mulpd 240(%edx),%xmm3
       addpd %xmm0,%xmm3    
       
       movaps %xmm5,%xmm0
       shufpd $0x1,%xmm5,%xmm5
       mulpd 288(%edx),%xmm0       
       mulpd 304(%edx),%xmm5
       addpd %xmm0,%xmm5
       
       movaps %xmm4,%xmm0
       shufpd $0x1,%xmm4,%xmm4
       mulpd 480(%edx),%xmm0       
       mulpd 496(%edx),%xmm4
       addpd %xmm0,%xmm4

       movaps %xmm6,16(%esi)
       movaps %xmm3,112(%esi)
       movaps %xmm5,144(%esi)
       movaps %xmm4,240(%esi)       
       
#-----------------------------------

       movaps (%esi), %xmm3
       movaps 128(%esi), %xmm5       
       movaps 64(%esi), %xmm4
       movaps 192(%esi), %xmm6

       movaps %xmm3,%xmm0
       subpd %xmm5,%xmm3
       addpd %xmm0,%xmm5

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7

       shufpd $0x1,%xmm2,%xmm2
       xorpd %xmm1,%xmm2

       movaps %xmm5,%xmm0
       subpd %xmm7,%xmm5
       addpd %xmm0,%xmm7
       
       movaps %xmm3,%xmm0
       subpd %xmm2,%xmm3
       addpd %xmm0,%xmm2
       
       movaps %xmm2,%xmm0
       shufpd $0x1,%xmm2,%xmm2
       mulpd 128(%edx),%xmm0       
       mulpd 144(%edx),%xmm2
       addpd %xmm0,%xmm2

       movaps %xmm5,%xmm0
       shufpd $0x1,%xmm5,%xmm5
       mulpd 256(%edx),%xmm0       
       mulpd 272(%edx),%xmm5
       addpd %xmm0,%xmm5
       
       movaps %xmm3,%xmm0
       shufpd $0x1,%xmm3,%xmm3
       mulpd 384(%edx),%xmm0       
       mulpd 400(%edx),%xmm3
       addpd %xmm0,%xmm3

       movaps %xmm7,(%esi)
       movaps %xmm2,64(%esi)
       movaps %xmm5,128(%esi)       
       movaps %xmm3,192(%esi)
       
#-----------------------------------       
       
       movaps 80(%esi), %xmm3
       movaps 208(%esi), %xmm5
       
       movaps %xmm4,%xmm0
       subpd %xmm3,%xmm4
       addpd %xmm0,%xmm3

       movaps %xmm5,%xmm0
       subpd %xmm6,%xmm5
       addpd %xmm0,%xmm6

       shufpd $0x1,%xmm5,%xmm5
       xorpd %xmm1,%xmm5
       shufpd $0x1,%xmm6,%xmm6
       xorpd %xmm1,%xmm6
       
       movaps %xmm3,%xmm0
       subpd %xmm5,%xmm3
       addpd %xmm0,%xmm5
       
       movaps %xmm4,%xmm0
       subpd %xmm6,%xmm4
       addpd %xmm0,%xmm6
       
       movaps %xmm5,%xmm0
       shufpd $0x1,%xmm5,%xmm5
       mulpd 96(%edx),%xmm0       
       mulpd 112(%edx),%xmm5
       addpd %xmm0,%xmm5
       
       movaps %xmm6,%xmm0
       shufpd $0x1,%xmm6,%xmm6
       mulpd 160(%edx),%xmm0       
       mulpd 176(%edx),%xmm6
       addpd %xmm0,%xmm6
       
       movaps %xmm4,%xmm0
       shufpd $0x1,%xmm4,%xmm4
       mulpd 352(%edx),%xmm0       
       mulpd 368(%edx),%xmm4
       addpd %xmm0,%xmm4
       
       movaps %xmm3,%xmm0
       shufpd $0x1,%xmm3,%xmm3
       mulpd 416(%edx),%xmm0       
       mulpd 432(%edx),%xmm3
       addpd %xmm0,%xmm3

       movaps %xmm5,48(%esi)
       movaps %xmm6,80(%esi)
       movaps %xmm4,176(%esi)
       movaps %xmm3,208(%esi)              
       
       addl $16,%eax
       addl $256,%esi
       cmpl %eax,%ebx
       jne .L0
########################       

       subl $256,%eax
       subl $4096,%esi

#------------------------------------
# fftj16 のループ(後処理)
#------------------------------------

       movaps .CI, %xmm7
       movaps .C2, %xmm6
       movaps .C1, %xmm5
       
       movl %esi,%ebx
       addl $256,%ebx

.align 16       
.L1:
       movaps (%esi), %xmm0
       movaps 2048(%esi), %xmm2              
       movaps 1024(%esi), %xmm1
       movaps 3072(%esi), %xmm3

       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       
       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3

       movaps %xmm0,3072(%esi)  # X0-X8
       movaps %xmm1,1024(%esi) # X4-X12
       movaps %xmm2,2048(%esi)   # (X0+X8)-(X4+X12)
       movaps %xmm3,(%esi)   # (X0+X8)+(X4+X12)
       
       addl $16,%esi
       cmpl %esi,%ebx
       jne .L1

       subl $256,%esi
       
#-----------
.align 16       
 .L2:

       movaps 256(%esi), %xmm0
       movaps 2304(%esi), %xmm2              
       movaps 1792(%esi), %xmm1
       movaps 3840(%esi), %xmm3

       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       
       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3

       movaps %xmm0,3840(%esi) # (X1-X9)-(X7-X15)
       movaps %xmm1,1792(%esi) # (X1-X9)+(X7-X15)
       movaps %xmm2,2304(%esi) # (X1+X9)-(X7+X15)
       movaps %xmm3,256(%esi)  # (X1+X9)+(X7+X15)
       
       addl $16,%esi
       cmpl %esi,%ebx
       jne .L2

       subl $256,%esi       

#-----------
.align 16       
.L3:
       movaps 512(%esi), %xmm0
       movaps 2560(%esi), %xmm2              
       movaps 1536(%esi), %xmm1
       movaps 3584(%esi), %xmm3

       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       
       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       mulpd %xmm6,%xmm0
       mulpd %xmm6,%xmm1       
       
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3

       movaps %xmm0,3584(%esi) # ((X2-X10)-(X6-X14))*C2
       movaps %xmm1,1536(%esi) # ((X2-X10)+(X6-X14))*C2
       movaps %xmm2,2560(%esi) # (X2+X10)-(X6+X14)
       movaps %xmm3,512(%esi)  # (X2+X10)+(X6+X14)

       addl $16,%esi
       cmpl %esi,%ebx
       jne .L3

       subl $256,%esi       
       
#---------------
.align 16       
.L4: 
       movaps 768(%esi), %xmm0
       movaps 2816(%esi), %xmm2              
       movaps 1280(%esi), %xmm1
       movaps 3328(%esi), %xmm3

       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       
       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3

       movaps %xmm0,3328(%esi) # (X3-X11)-(X5-X13)
       movaps %xmm1,1280(%esi) # (X3-X11)+(X5-X13)
       movaps %xmm2,2816(%esi) # (X3+X11)-(X5+X13)
       movaps %xmm3,768(%esi)  # (X3+X11)+(X5+X13)       

       addl $16,%esi
       cmpl %esi,%ebx
       jne .L4

       subl $256,%esi       

#-----------------------------------------------
.align 16       
.L5:
       movaps (%esi), %xmm0 # (X0+X8)+(X4+X12)
       movaps 512(%esi), %xmm2 # (X2+X10)+(X6+X14)
       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2

       movaps 768(%esi), %xmm3 # (X3+X11)+(X5+X13)
       addpd 256(%esi),%xmm3 # ((X3+X11)+(X5+X13))+((X1+X9)+(X7+X15))       
       movaps 2304(%esi), %xmm1 # (X1+X9)-(X7+X15)       
       subpd 2816(%esi),%xmm1 # ((X1+X9)-(X7+X15))-((X3+X11)-(X5+X13))
       
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1
       
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3
       
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1

       movaps %xmm2,2048(%ecx)  # X8
       movaps %xmm3,(%ecx)   # X0
       movaps %xmm0,3072(%ecx)  # X12
       movaps %xmm1,1024(%ecx) # X4

       addl $16,%esi
       addl $16,%ecx       
       cmpl %esi,%ebx
       jne .L5
       subl $256,%esi       
       subl $256,%ecx

#-----------------
.align 16       
.L6:
       movaps 2816(%esi), %xmm3 # (X3+X11)-(X5+X13)
       addpd 2304(%esi),%xmm3 # ((X1+X9)-(X7+X15))+((X3+X11)-(X5+X13))
       mulpd %xmm6,%xmm3       
       movaps 256(%esi), %xmm1 # (X1+X9)+(X7+X15)              
       subpd 768(%esi), %xmm1 # ((X1+X9)+(X7+X15))-((X3+X11)+(X5+X13))
       mulpd %xmm6,%xmm1
       
       movaps 2048(%esi), %xmm0 # (X0+X8)-(X4+X12)
       movaps 2560(%esi), %xmm2 # (X2+X10)-(X6+X14)
       
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       
       movaps %xmm3,%xmm4
       subpd %xmm2,%xmm3
       addpd %xmm4,%xmm2

       shufpd $0x1,%xmm3,%xmm3
       xorpd %xmm7,%xmm3
       shufpd $0x1,%xmm2,%xmm2
       xorpd %xmm7,%xmm2

       movaps %xmm0,%xmm4
       subpd %xmm3,%xmm0
       addpd %xmm4,%xmm3

       movaps %xmm1,%xmm4
       subpd %xmm2,%xmm1
       addpd %xmm4,%xmm2

       movaps %xmm0,2560(%ecx)  # X10
       movaps %xmm3,1536(%ecx) # X6       
       movaps %xmm1,3584(%ecx)  # X14
       movaps %xmm2,512(%ecx)   # X2       

       addl $16,%esi
       addl $16,%ecx       
       cmpl %esi,%ebx
       jne .L6
       subl $256,%esi       
       subl $256,%ecx
#-----------------------
       movaps .S1, %xmm6
.align 16       
.L7:
       movaps 1024(%esi), %xmm0 # X4-X12
       addpd 1536(%esi),%xmm0 # (X4-X12)+((X2-X10)+(X6-X14))*C2       
       movaps 3584(%esi), %xmm2 # ((X2-X10)-(X6-X14))*C2       
       addpd 3072(%esi),%xmm2  # (X0-X8)+((X2-X10)+(X6-X14))*C2

       movaps 1792(%esi), %xmm1 # (X1-X9)+(X7-X15)
       mulpd %xmm6,%xmm1
       movaps 1280(%esi), %xmm3 # # (X3-X11)+(X5-X13)       
       mulpd %xmm5,%xmm3
       addpd %xmm3,%xmm1 # ((X1-X9)+(X7-X15))*S1+((X3-X11)+(X5-X13))*C1

       movaps 3840(%esi),%xmm3 # (X1-X9)-(X7-X15)
       mulpd %xmm5,%xmm3
       movaps 3328(%esi),%xmm4 # (X3-X11)-(X5-X13)       
       mulpd %xmm6,%xmm4
       addpd %xmm4,%xmm3 # ((X1-X9)-(X7-X15))*C1+((X3-X11)-(X5-X13))*S1
       
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3

       movaps %xmm1,%xmm4
       subpd %xmm0,%xmm1
       addpd %xmm4,%xmm0

       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1
       shufpd $0x1,%xmm0,%xmm0
       xorpd %xmm7,%xmm0
       
       movaps %xmm3,%xmm4
       subpd %xmm0,%xmm3
       addpd %xmm4,%xmm0

       movaps %xmm2,%xmm4
       subpd %xmm1,%xmm2
       addpd %xmm4,%xmm1

       movaps %xmm3,3840(%ecx)  # X15
       movaps %xmm0,256(%ecx)   # X1
       movaps %xmm2,2304(%ecx)  # X9       
       movaps %xmm1,1792(%ecx)  # X7

       addl $16,%esi
       addl $16,%ecx       
       cmpl %esi,%ebx
       jne .L7
       subl $256,%esi       
       subl $256,%ecx
       
#------------------------------------       
.align 16       
.L8:
       movaps 1024(%esi), %xmm0 # X4-X12
       subpd 1536(%esi),%xmm0 # (X4-X12)-((X2-X10)+(X6-X14))*C2
       movaps 3072(%esi), %xmm2 # (X0-X8)       
       subpd 3584(%esi),%xmm2  # (X0-X8)-((X2-X10)-(X6-X14))*C2
       
       movaps 1792(%esi), %xmm1 # (X1-X9)+(X5-X13)
       mulpd %xmm5,%xmm1
       movaps 1280(%esi), %xmm3 # # (X3-X11)+(X7-X15)       
       mulpd %xmm6,%xmm3
       subpd %xmm3,%xmm1 # ((X1-X9)+(X5-X13))*C1+((X3-X11)+(X7-X15))*S1

       movaps 3840(%esi),%xmm3 # (X1-X9)-(X5-X13)
       mulpd %xmm6,%xmm3
       movaps 3328(%esi),%xmm4 # (X3-X11)-(X7-X15)       
       mulpd %xmm5,%xmm4
       subpd %xmm4,%xmm3 # ((X1-X9)-(X5-X13))*S1-((X3-X11)-(X7-X15))*C1
       
       movaps %xmm1,%xmm4
       subpd %xmm0,%xmm1
       addpd %xmm4,%xmm0

       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3

       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1
       shufpd $0x1,%xmm0,%xmm0
       xorpd %xmm7,%xmm0
       
       movaps %xmm3,%xmm4
       subpd %xmm1,%xmm3
       addpd %xmm4,%xmm1

       movaps %xmm2,%xmm4
       subpd %xmm0,%xmm2
       addpd %xmm4,%xmm0

       movaps %xmm3,3328(%ecx)  # X13
       movaps %xmm1,768(%ecx)   # X3
       movaps %xmm2,2816(%ecx)  # X11       
       movaps %xmm0,1280(%ecx)  # X5

       addl $16,%esi
       addl $16,%ecx       
       cmpl %esi,%ebx
       jne .L8
       
#------------------------------------
       popl  %ebx
       popl  %esi       
	
       ret

.align 16
.CI:
      .long 0x0, 0x80000000, 0x0, 0x0
.align 16
.C2:
      .long 0x667f3bcd,0x3fe6a09e,0x667f3bcd,0x3fe6a09e
.align 16
.C1:
      .long 0xcf328d46,0x3fed906b,0xcf328d46,0x3fed906b
.align 16
.S1:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2

