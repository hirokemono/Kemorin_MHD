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
# rfft 2048 in-place backward
.text
.globl fjribb_
fjribb_:
       movl    4(%esp), %eax
       movl    8(%esp), %ecx  # : ZD のベースアドレス
       movl   16(%esp), %edx  # : ZT のベースアドレス
       pushl	%esi
       movl   16(%esp), %esi  # : ZDD のベースアドレス       
       
       pushl	%ebx
       
       pushl	%edi
       pushl	%ebp       
       
       movl  %edx,%edi  # : ZT のベースアドレスを記憶
       movl  %eax,%ebp  # : ZT のベースアドレスを記憶
       
#--------------------------------       
       
#  rfft のための前処理

      addl $32768,%edi
      movl $16384,%ebx  # %ebx に (N/2)*16 が入る
 
      addl %ebp,%ebx
      movaps .CID,%xmm7
      
      movaps (%ebp),%xmm0
      movaps %xmm0,%xmm1            
      xorpd %xmm7,%xmm0
      shufpd $1,%xmm1,%xmm1      
      addpd %xmm1,%xmm0
      movapd %xmm0,(%ebp)
      
      movaps -16(%ebx),%xmm1
      movaps 16(%ebp),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd 32(%edi),%xmm2
      mulpd 48(%edi),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,16(%ebp)      
      movaps %xmm1,-16(%ebx)
      
      addl $32,%ebp
      subl $32,%ebx
      addl $64,%edi      

.align 16
.L1R:
      movaps (%ebx),%xmm1
      movaps (%ebp),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd (%edi),%xmm2
      mulpd 16(%edi),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,(%ebp)      
      movaps %xmm1,(%ebx)

      movaps -16(%ebx),%xmm1
      movaps 16(%ebp),%xmm0

      xorpd %xmm7,%xmm1
      movaps %xmm0,%xmm2
      subpd %xmm1,%xmm0
      addpd %xmm2,%xmm1
      
      movaps %xmm0,%xmm2
      shufpd $1,%xmm0,%xmm0
      mulpd 32(%edi),%xmm2
      mulpd 48(%edi),%xmm0
      addpd %xmm2,%xmm0
      
      movaps %xmm0,%xmm2
      addpd %xmm1,%xmm0
      subpd %xmm2,%xmm1
      xorpd %xmm7,%xmm1      

      movaps %xmm0,16(%ebp)
      movaps %xmm1,-16(%ebx)
      
      addl $32,%ebp
      subl $32,%ebx
      addl $64,%edi      

      cmpl %ebp,%ebx
      jne .L1R

      movaps (%ebp),%xmm0
      addpd %xmm0,%xmm0
      xorpd %xmm7,%xmm0
      movaps %xmm0,(%ebp)

#--------------------------------
# fft 1024 in-place backward (16x16x4 type)
#.text
#.globl fjciba_
#fjciba_:
#       movl    4(%esp), %eax
#       movl    8(%esp), %ecx  # : ZD のベースアドレス
#       movl   16(%esp), %edx  # : ZT のベースアドレス
#       pushl	%esi
#       movl   16(%esp), %esi  # : ZDD のベースアドレス       
#       
#       pushl	%ebx
#       pushl	%edi	
#------------------------------------

       movl %eax,%edi
       addl $1024,%edi

########################################


       addl $2048,%edx
       subl $32,%edx
       
       movl %eax,%ebx
       addl $1024,%ebx

.LAA:

       movl %eax,%edi
       addl $64,%edi

.LA:
#------------------------------------
# fftj16 のループ
#------------------------------------

#------------------------------------
#------------------------------------
       movaps 1024(%eax), %xmm0 #!
       movaps 5120(%eax), %xmm2 #!
       movaps 9216(%eax), %xmm1 #!
       movaps 13312(%eax), %xmm3 #!

       movaps %xmm0,%xmm7
       subpd %xmm1,%xmm0
       addpd %xmm7,%xmm1

       movaps %xmm2,%xmm7
       subpd %xmm3,%xmm2
       addpd %xmm7,%xmm3

       movaps %xmm1,%xmm7
       subpd %xmm3,%xmm1
       addpd %xmm7,%xmm3

       movaps %xmm3,16(%ecx)
       
#-------------

       movaps 3072(%eax), %xmm5 #!
       movaps 7168(%eax), %xmm6 #!
       movaps 11264(%eax), %xmm3 #!
       movaps 15360(%eax), %xmm4 #!

       movaps %xmm5,%xmm7
       subpd %xmm3,%xmm5
       addpd %xmm7,%xmm3

       movaps %xmm6,%xmm7
       subpd %xmm4,%xmm6
       addpd %xmm7,%xmm4

       movaps %xmm3,%xmm7
       subpd %xmm4,%xmm3
       addpd %xmm7,%xmm4

       movaps %xmm1,48(%ecx)
       movaps %xmm3,176(%ecx)       
       movaps %xmm4,144(%ecx)              

#-------------

       movaps .C1,%xmm1
       movaps .C3,%xmm3       

       movaps %xmm0,%xmm7
       subpd %xmm6,%xmm0
       addpd %xmm7,%xmm6

       movaps %xmm5,%xmm7
       subpd %xmm2,%xmm5
       addpd %xmm7,%xmm2

       movaps %xmm1,%xmm7
       movaps %xmm3,%xmm4
       
       mulpd %xmm5,%xmm7
       mulpd %xmm0,%xmm4       
       subpd %xmm7,%xmm4
       
       mulpd %xmm1,%xmm0
       mulpd %xmm3,%xmm5
       addpd %xmm5,%xmm0
       
       movaps %xmm1,%xmm7
       
       mulpd %xmm6,%xmm7
       mulpd %xmm3,%xmm6
       
       mulpd %xmm2,%xmm3
       subpd %xmm3,%xmm7

       mulpd %xmm1,%xmm2
       addpd %xmm2,%xmm6

       movaps %xmm0,112(%ecx)
       movaps %xmm4,80(%ecx)
       
       movaps %xmm6,240(%ecx)              
       movaps %xmm7,208(%ecx)
       
#-----------------------------------

       movaps (%eax), %xmm0 #!
       movaps 8192(%eax), %xmm3 #!
       movaps 4096(%eax), %xmm2 #!
       movaps 12288(%eax), %xmm1 #!
       
       movaps %xmm0,%xmm7
       subpd %xmm3,%xmm0       
       addpd %xmm7,%xmm3

       movaps %xmm2,%xmm7
       subpd %xmm1,%xmm2
       addpd %xmm7,%xmm1
       
       movaps %xmm3,%xmm7
       subpd %xmm1,%xmm3
       addpd %xmm7,%xmm1
       
       movaps %xmm1,(%ecx)
       
#-----------------------------------

       movaps 2048(%eax), %xmm5 #!
       movaps 6144(%eax), %xmm6 #!
       movaps 10240(%eax), %xmm4 #!
       movaps 14336(%eax), %xmm1 #!

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
       
       movaps %xmm1,128(%ecx)
       
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
       
       movaps %xmm0,64(%ecx)
       movaps %xmm2,192(%ecx)
       
#-----------------------------------

       movaps 48(%ecx), %xmm2
       movaps 176(%ecx), %xmm7       
       
       mulpd %xmm1,%xmm2
       mulpd %xmm1,%xmm7
       
       movaps .C0,%xmm1

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7

       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm1,%xmm4
       
       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm1,%xmm7

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
       
###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 64(%edx),%xmm0
       mulpd 80(%edx),%xmm4
       addpd %xmm0,%xmm4
###       
###
       movaps %xmm7,%xmm0
       shufpd $1,%xmm7,%xmm7
       mulpd 192(%edx),%xmm0
       mulpd 208(%edx),%xmm7
       addpd %xmm0,%xmm7
###
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 320(%edx),%xmm0
       mulpd 336(%edx),%xmm3
       addpd %xmm0,%xmm3
###       
###
       movaps %xmm2,%xmm0
       shufpd $1,%xmm2,%xmm2
       mulpd 448(%edx),%xmm0
       mulpd 464(%edx),%xmm2
       addpd %xmm0,%xmm2
###       
       
       movaps %xmm4,32(%ecx)              
       movaps %xmm7,96(%ecx)
       movaps %xmm3,160(%ecx)
       movaps %xmm2,224(%ecx)       
       
#-----------------------------------       
       
       movaps 16(%ecx), %xmm2
       movaps 112(%ecx), %xmm4
       movaps 144(%ecx), %xmm7       
       movaps 240(%ecx), %xmm3
       
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
       
###
       movaps %xmm6,%xmm0
       shufpd $1,%xmm6,%xmm6
       mulpd 32(%edx),%xmm0
       mulpd 48(%edx),%xmm6
       addpd %xmm0,%xmm6
###
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 224(%edx),%xmm0
       mulpd 240(%edx),%xmm3
       addpd %xmm0,%xmm3
###       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 288(%edx),%xmm0
       mulpd 304(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 480(%edx),%xmm0
       mulpd 496(%edx),%xmm4
       addpd %xmm0,%xmm4
###       

       movaps %xmm6,16(%ecx)              
       movaps %xmm3,112(%ecx)
       movaps %xmm5,144(%ecx)
       movaps %xmm4,240(%ecx)       
       
#-----------------------------------

       movaps (%ecx), %xmm3
       movaps 64(%ecx), %xmm4
       movaps 128(%ecx), %xmm5
       movaps 192(%ecx), %xmm6

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
       
###
       movaps %xmm2,%xmm0
       shufpd $1,%xmm2,%xmm2
       mulpd 128(%edx),%xmm0
       mulpd 144(%edx),%xmm2
       addpd %xmm0,%xmm2
###       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 256(%edx),%xmm0
       mulpd 272(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 384(%edx),%xmm0
       mulpd 400(%edx),%xmm3
       addpd %xmm0,%xmm3
###       

       movaps %xmm7,(%ecx)
       movaps %xmm2,64(%ecx)
       movaps %xmm5,128(%ecx)       
       movaps %xmm3,192(%ecx)
       
#-----------------------------------       
       
       movaps 80(%ecx), %xmm3
       movaps 208(%ecx), %xmm5
       
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
       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 96(%edx),%xmm0
       mulpd 112(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm6,%xmm0
       shufpd $1,%xmm6,%xmm6
       mulpd 160(%edx),%xmm0
       mulpd 176(%edx),%xmm6
       addpd %xmm0,%xmm6
###       
###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 352(%edx),%xmm0
       mulpd 368(%edx),%xmm4
       addpd %xmm0,%xmm4
###       
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 416(%edx),%xmm0
       mulpd 432(%edx),%xmm3
       addpd %xmm0,%xmm3
###       

       movaps %xmm5,48(%ecx)
       movaps %xmm6,80(%ecx)
       movaps %xmm4,176(%ecx)
       movaps %xmm3,208(%ecx)
       
#---------------------------------------

       addl $16,%eax
       addl $4096,%ecx
       addl $480,%edx       
       cmpl %eax,%edi
       jne .LA
       
       subl $64,%eax         
       subl $16384,%ecx
       addl $64,%eax                
       addl $256,%ecx
       cmpl %eax,%ebx
       jne .LAA


       subl $1024,%eax  
       subl $4096,%ecx
       
       subl $32736,%edx

#---------------------------------------       

       movl %ecx,%ebx
#       addl $1024,%ebx
       addl $16384,%ebx

#---------------------------------------

       movl %ecx,%edi
       addl $256,%edi

.LBD:

       movaps  256(%ecx), %xmm0 #! !
       movaps 1280(%ecx), %xmm2 #! !
       movaps 2304(%ecx), %xmm1 #! !
       movaps 3328(%ecx), %xmm3 #! !

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

       movaps  768(%ecx), %xmm5 #! !
       movaps 1792(%ecx), %xmm6 #! !
       movaps 2816(%ecx), %xmm3 #! !
       movaps 3840(%ecx), %xmm4 #! !

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

       movaps .C1,%xmm1
       movaps .C3,%xmm3       

       movaps %xmm0,%xmm7
       subpd %xmm6,%xmm0
       addpd %xmm7,%xmm6

       movaps %xmm5,%xmm7
       subpd %xmm2,%xmm5
       addpd %xmm7,%xmm2

       movaps %xmm1,%xmm7
       movaps %xmm3,%xmm4
       
       mulpd %xmm5,%xmm7
       mulpd %xmm0,%xmm4       
       subpd %xmm7,%xmm4
       
       mulpd %xmm1,%xmm0
       mulpd %xmm3,%xmm5
       addpd %xmm5,%xmm0
       
       movaps %xmm1,%xmm7
       
       mulpd %xmm6,%xmm7
       mulpd %xmm3,%xmm6
       
       mulpd %xmm2,%xmm3
       subpd %xmm3,%xmm7

       mulpd %xmm1,%xmm2
       addpd %xmm2,%xmm6

       movaps %xmm0,112(%esi)
       movaps %xmm4,80(%esi)
       
       movaps %xmm6,240(%esi)              
       movaps %xmm7,208(%esi)
       
#-----------------------------------

       movaps (%ecx), %xmm0 #! !
       movaps 2048(%ecx), %xmm3 #! !
       movaps 1024(%ecx), %xmm2 #! !
       movaps 3072(%ecx), %xmm1 #! !
       
       movaps %xmm0,%xmm7
       subpd %xmm3,%xmm0       
       addpd %xmm7,%xmm3

       movaps %xmm2,%xmm7
       subpd %xmm1,%xmm2
       addpd %xmm7,%xmm1
       
       movaps %xmm3,%xmm7
       subpd %xmm1,%xmm3
       addpd %xmm7,%xmm1
       
       movaps %xmm1,(%esi)
       
#-----------------------------------

       movaps  512(%ecx), %xmm5 #! !
       movaps 1536(%ecx), %xmm6 #! !
       movaps 2560(%ecx), %xmm4 #! !
       movaps 3584(%ecx), %xmm1 #! !

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
       
       movaps .C0,%xmm1

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7

       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm1,%xmm4
       
       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm1,%xmm7

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
       
       movaps 16(%esi), %xmm2
       movaps 112(%esi), %xmm4
       movaps 144(%esi), %xmm7       
       movaps 240(%esi), %xmm3
       
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
       movaps 64(%esi), %xmm4
       movaps 128(%esi), %xmm5
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
   
#----------------------------------
       addl $16,%ecx
#       addl $256,%esi
       addl $1024,%esi ###
       cmpl %ecx,%edi
       jne .LBD
       
       subl $256,%ecx  
#       subl $4096,%esi       
       subl $16384,%esi ###

       addl $4096,%ecx
#       addl $4096,%esi
       addl $256,%esi ###
       addl $512,%edx       
       

.LBB:

       movl %ecx,%edi
       addl $256,%edi

.LB:

       movaps  256(%ecx), %xmm0 #! !
       movaps 1280(%ecx), %xmm2 #! !
       movaps 2304(%ecx), %xmm1 #! !
       movaps 3328(%ecx), %xmm3 #! !

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
       
       movaps  768(%ecx), %xmm5 #! !
       movaps 1792(%ecx), %xmm6 #! !
       movaps 2816(%ecx), %xmm3 #! !
       movaps 3840(%ecx), %xmm4 #! !
       

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

       movaps .C1,%xmm1
       movaps .C3,%xmm3       

       movaps %xmm0,%xmm7
       subpd %xmm6,%xmm0
       addpd %xmm7,%xmm6

       movaps %xmm5,%xmm7
       subpd %xmm2,%xmm5
       addpd %xmm7,%xmm2

       movaps %xmm1,%xmm7
       movaps %xmm3,%xmm4
       
       mulpd %xmm5,%xmm7
       mulpd %xmm0,%xmm4       
       subpd %xmm7,%xmm4
       
       mulpd %xmm1,%xmm0
       mulpd %xmm3,%xmm5
       addpd %xmm5,%xmm0
       
       movaps %xmm1,%xmm7
       
       mulpd %xmm6,%xmm7
       mulpd %xmm3,%xmm6
       
       mulpd %xmm2,%xmm3
       subpd %xmm3,%xmm7

       mulpd %xmm1,%xmm2
       addpd %xmm2,%xmm6

       movaps %xmm0,112(%esi)
       movaps %xmm4,80(%esi)
       
       movaps %xmm6,240(%esi)              
       movaps %xmm7,208(%esi)
       
#-----------------------------------

       movaps (%ecx), %xmm0 #! !
       movaps 2048(%ecx), %xmm3 #! !
       movaps 1024(%ecx), %xmm2 #! !
       movaps 3072(%ecx), %xmm1 #! !
       
       movaps %xmm0,%xmm7
       subpd %xmm3,%xmm0       
       addpd %xmm7,%xmm3

       movaps %xmm2,%xmm7
       subpd %xmm1,%xmm2
       addpd %xmm7,%xmm1
       
       movaps %xmm3,%xmm7
       subpd %xmm1,%xmm3
       addpd %xmm7,%xmm1
       
       movaps %xmm1,(%esi)
       
#-----------------------------------

       movaps  512(%ecx), %xmm5 #! !
       movaps 1536(%ecx), %xmm6 #! !
       movaps 2560(%ecx), %xmm4 #! !
       movaps 3584(%ecx), %xmm1 #! !

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
       
       movaps .C0,%xmm1

       movaps %xmm2,%xmm0
       subpd %xmm7,%xmm2
       addpd %xmm0,%xmm7

       shufpd $0x1,%xmm4,%xmm4
       xorpd %xmm1,%xmm4
       
       shufpd $0x1,%xmm7,%xmm7
       xorpd %xmm1,%xmm7

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
       
###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 64(%edx),%xmm0
       mulpd 80(%edx),%xmm4
       addpd %xmm0,%xmm4
###       
###
       movaps %xmm7,%xmm0
       shufpd $1,%xmm7,%xmm7
       mulpd 192(%edx),%xmm0
       mulpd 208(%edx),%xmm7
       addpd %xmm0,%xmm7
###
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 320(%edx),%xmm0
       mulpd 336(%edx),%xmm3
       addpd %xmm0,%xmm3
###       
###
       movaps %xmm2,%xmm0
       shufpd $1,%xmm2,%xmm2
       mulpd 448(%edx),%xmm0
       mulpd 464(%edx),%xmm2
       addpd %xmm0,%xmm2
###       
       
       movaps %xmm4,32(%esi)              
       movaps %xmm7,96(%esi)
       movaps %xmm3,160(%esi)
       movaps %xmm2,224(%esi)       
       
#-----------------------------------       
       
       movaps 16(%esi), %xmm2
       movaps 112(%esi), %xmm4
       movaps 144(%esi), %xmm7       
       movaps 240(%esi), %xmm3
       
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

###
       movaps %xmm6,%xmm0
       shufpd $1,%xmm6,%xmm6
       mulpd 32(%edx),%xmm0
       mulpd 48(%edx),%xmm6
       addpd %xmm0,%xmm6
###
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 224(%edx),%xmm0
       mulpd 240(%edx),%xmm3
       addpd %xmm0,%xmm3
###       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 288(%edx),%xmm0
       mulpd 304(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 480(%edx),%xmm0
       mulpd 496(%edx),%xmm4
       addpd %xmm0,%xmm4
###       

       movaps %xmm6,16(%esi)              
       movaps %xmm3,112(%esi)
       movaps %xmm5,144(%esi)
       movaps %xmm4,240(%esi)       
       
#-----------------------------------

       movaps (%esi), %xmm3
       movaps 64(%esi), %xmm4
       movaps 128(%esi), %xmm5
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
       
###
       movaps %xmm2,%xmm0
       shufpd $1,%xmm2,%xmm2
       mulpd 128(%edx),%xmm0
       mulpd 144(%edx),%xmm2
       addpd %xmm0,%xmm2
###       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 256(%edx),%xmm0
       mulpd 272(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 384(%edx),%xmm0
       mulpd 400(%edx),%xmm3
       addpd %xmm0,%xmm3
###       

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
       
###
       movaps %xmm5,%xmm0
       shufpd $1,%xmm5,%xmm5
       mulpd 96(%edx),%xmm0
       mulpd 112(%edx),%xmm5
       addpd %xmm0,%xmm5
###       
###
       movaps %xmm6,%xmm0
       shufpd $1,%xmm6,%xmm6
       mulpd 160(%edx),%xmm0
       mulpd 176(%edx),%xmm6
       addpd %xmm0,%xmm6
###       
###
       movaps %xmm4,%xmm0
       shufpd $1,%xmm4,%xmm4
       mulpd 352(%edx),%xmm0
       mulpd 368(%edx),%xmm4
       addpd %xmm0,%xmm4
###       
###
       movaps %xmm3,%xmm0
       shufpd $1,%xmm3,%xmm3
       mulpd 416(%edx),%xmm0
       mulpd 432(%edx),%xmm3
       addpd %xmm0,%xmm3
###       

       movaps %xmm5,48(%esi)
       movaps %xmm6,80(%esi)
       movaps %xmm4,176(%esi)
       movaps %xmm3,208(%esi)
   
#----------------------------------
       addl $16,%ecx
       addl $1024,%esi ###
       cmpl %ecx,%edi
       jne .LB
       
       subl $256,%ecx  
       subl $16384,%esi ###

       addl $4096,%ecx
       addl $256,%esi ###
       addl $512,%edx       
       
       cmpl %ecx,%ebx
       jne .LBB
       
       subl $16384,%ecx
       subl $1024,%esi ###
      
#-------------------------------------------------------------------------       
#------------------------------------
# fftj4 のループ
#------------------------------------

       movl %eax,%edi
       addl $256,%edi

       movaps .C0,%xmm7
.LD:
       movl %eax,%ebx
       addl $4096,%ebx
.LC:
#------0
       movaps (%esi), %xmm0   ###
       movaps 256(%esi), %xmm1 ###
       movaps 512(%esi), %xmm2 ###   
       movaps 768(%esi), %xmm3 ###
       
       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       movaps %xmm1,4096(%eax)
       movaps %xmm0,12288(%eax)
       movaps %xmm3,(%eax)     
       movaps %xmm2,8192(%eax)
       addl $16,%eax
       addl $1024,%esi ###
#------1
       movaps (%esi), %xmm0   ###
       movaps 256(%esi), %xmm1 ###
       movaps 512(%esi), %xmm2 ###   
       movaps 768(%esi), %xmm3 ###
       
       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       movaps %xmm1,4096(%eax)
       movaps %xmm0,12288(%eax)
       movaps %xmm3,(%eax)     
       movaps %xmm2,8192(%eax)
       addl $16,%eax
       addl $1024,%esi ###       
#------2
       movaps (%esi), %xmm0   ###
       movaps 256(%esi), %xmm1 ###
       movaps 512(%esi), %xmm2 ###   
       movaps 768(%esi), %xmm3 ###

       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       movaps %xmm1,4096(%eax)
       movaps %xmm0,12288(%eax)
       movaps %xmm3,(%eax)     
       movaps %xmm2,8192(%eax)
       addl $16,%eax
       addl $1024,%esi ###
#------3       
       movaps (%esi), %xmm0   ###
       movaps 256(%esi), %xmm1 ###
       movaps 512(%esi), %xmm2 ###   
       movaps 768(%esi), %xmm3 ###

       movaps %xmm1,%xmm4
       subpd %xmm3,%xmm1
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm2,%xmm0
       addpd %xmm4,%xmm2
       shufpd $0x1,%xmm1,%xmm1
       xorpd %xmm7,%xmm1
       movaps %xmm2,%xmm4
       subpd %xmm3,%xmm2
       addpd %xmm4,%xmm3
       movaps %xmm0,%xmm4
       subpd %xmm1,%xmm0
       addpd %xmm4,%xmm1
       movaps %xmm1,4096(%eax)
       movaps %xmm0,12288(%eax)
       movaps %xmm3,(%eax)     
       movaps %xmm2,8192(%eax)
       addl $16,%eax
       addl $1024,%esi ###
#---------------       
       subl $64,%eax
       subl $4096,%esi ###
       
       addl $256,%eax
       addl $16,%esi
       
       cmpl %eax,%ebx
       jne .LC
#---------------       
       subl $4096,%eax
       subl $256,%esi

       addl $64,%eax
       addl $4096,%esi ###
       
       cmpl %eax,%edi
       jne .LD
#-------------------------------------------------------------
.LE:

       popl %ebp       
       popl %edi
       
       popl %ebx
       popl %esi       	
	
       ret

#--------------------------------------------------
.section .rodata
.align 16
.CID:
      .long 0x0, 0x0, 0x0, 0x80000000
.align 16
.C0:
      .long 0x0, 0x80000000, 0x0, 0x0
.align 16
.C1:
      .long 0xcf328d46,0x3fed906b,0xcf328d46,0x3fed906b
.align 16
.C2:
      .long 0x667f3bcd, 0x3fe6a09e,0x667f3bcd, 0x3fe6a09e
.align 16
.C3:
      .long 0xa6aea963,0x3fd87de2,0xa6aea963,0x3fd87de2
