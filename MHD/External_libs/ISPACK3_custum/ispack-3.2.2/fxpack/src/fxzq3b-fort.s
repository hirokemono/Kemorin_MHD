########################################################################
# ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
# Copyright (C) 1998--2024 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
.section .note.GNU-stack,"",@progbits
# mark_description "Intel(R) Fortran Intel(R) 64 Compiler for applications running on Intel(R) 64, Version 17.0.0.098 Build 2016";
# mark_description "0721";
# mark_description "-xHost -S";
	.file "fxzq3b-fort.f90"
	.text
..TXTST0:
# -- Begin  fxzq3b_
	.text
# mark_begin;
       .align    16,0x90
	.globl fxzq3b_
# --- FXZQ3B
fxzq3b_:
# parameter 1: %rdi
# parameter 2: %rsi
..B1.1:                         # Preds ..B1.0
                                # Execution count [1.00e+00]
	.cfi_startproc
..___tag_value_fxzq3b_.1:
..L2:
                                                          #4.12
        movq      $0x5555555555555556, %rax                     #16.9
        movq      (%rdi), %rcx                                  #4.12
        xorl      %edi, %edi                                    #14.3
        imulq     %rcx                                          #16.9
        sarq      $63, %rcx                                     #16.9
        movq      %rdx, %r8                                     #16.9
        subq      %rcx, %r8                                     #16.9
        xorl      %ecx, %ecx                                    #15.6
        movslq    %r8d, %rax                                    #16.9
        movq      %rax, %rdx                                    #16.13
        shlq      $6, %rdx                                      #16.13
        testq     %r8, %r8                                      #14.3
        jle       ..B1.5        # Prob 2%                       #14.3
                                # LOE rax rdx rcx rbx rbp rsi rdi r8 r12 r13 r14 r15
..B1.2:                         # Preds ..B1.1
                                # Execution count [9.79e-01]
        shlq      $7, %rax                                      #16.25
        addq      %rsi, %rdx                                    #16.13
        vmovupd   .L_2il0floatpacket.0(%rip), %ymm7             #20.27
        addq      %rsi, %rax                                    #16.25
        vmovupd   .L_2il0floatpacket.1(%rip), %ymm6             #24.27
                                # LOE rax rdx rcx rbx rbp rsi rdi r8 r12 r13 r14 r15 ymm6 ymm7
..B1.3:                         # Preds ..B1.3 ..B1.2
                                # Execution count [5.44e+00]
        vmovupd   (%rcx,%rdx), %xmm8                            #16.13
        vmovupd   32(%rcx,%rdx), %xmm10                         #17.13
        vmovupd   (%rcx,%rax), %xmm9                            #16.25
        vmovupd   32(%rcx,%rax), %xmm11                         #17.25
        vmovupd   (%rcx,%rsi), %xmm0                            #20.13
        incq      %rdi                                          #14.3
        vinsertf128 $1, 16(%rcx,%rdx), %ymm8, %ymm12            #16.13
        vinsertf128 $1, 48(%rcx,%rdx), %ymm10, %ymm14           #17.13
        vinsertf128 $1, 16(%rcx,%rax), %ymm9, %ymm13            #16.25
        vinsertf128 $1, 48(%rcx,%rax), %ymm11, %ymm15           #17.25
        vaddpd    %ymm13, %ymm12, %ymm3                         #18.9
        vaddpd    %ymm15, %ymm14, %ymm2                         #19.9
        vsubpd    %ymm13, %ymm12, %ymm5                         #16.9
        vsubpd    %ymm15, %ymm14, %ymm4                         #17.9
        vmovupd   32(%rcx,%rsi), %xmm8                          #21.13
        vmulpd    %ymm3, %ymm7, %ymm1                           #20.27
        vmulpd    %ymm2, %ymm7, %ymm9                           #21.27
        vmulpd    %ymm4, %ymm6, %ymm4                           #24.27
        vmulpd    %ymm5, %ymm6, %ymm5                           #25.27
        vinsertf128 $1, 16(%rcx,%rsi), %ymm0, %ymm10            #20.13
        vinsertf128 $1, 48(%rcx,%rsi), %ymm8, %ymm11            #21.13
        vaddpd    %ymm1, %ymm10, %ymm1                          #20.9
        vaddpd    %ymm9, %ymm11, %ymm0                          #21.9
        vaddpd    %ymm10, %ymm3, %ymm3                          #22.9
        vaddpd    %ymm4, %ymm1, %ymm12                          #24.9
        vsubpd    %ymm5, %ymm0, %ymm13                          #25.9
        vaddpd    %ymm1, %ymm1, %ymm14                          #26.24
        vaddpd    %ymm0, %ymm0, %ymm0                           #27.24
        vaddpd    %ymm11, %ymm2, %ymm2                          #23.9
        vsubpd    %ymm12, %ymm14, %ymm15                        #26.9
        vsubpd    %ymm13, %ymm0, %ymm1                          #27.9
        vmovupd   %xmm3, (%rcx,%rsi)                            #22.9
        vmovupd   %xmm2, 32(%rcx,%rsi)                          #23.9
        vextractf128 $1, %ymm3, 16(%rcx,%rsi)                   #22.9
        vextractf128 $1, %ymm2, 48(%rcx,%rsi)                   #23.9
        vmovupd   %xmm12, (%rcx,%rax)                           #24.9
        vmovupd   %xmm13, 32(%rcx,%rax)                         #25.9
        vextractf128 $1, %ymm12, 16(%rcx,%rax)                  #24.9
        vextractf128 $1, %ymm13, 48(%rcx,%rax)                  #25.9
        vmovupd   %xmm15, (%rcx,%rdx)                           #26.9
        vmovupd   %xmm1, 32(%rcx,%rdx)                          #27.9
        vextractf128 $1, %ymm15, 16(%rcx,%rdx)                  #26.9
        vextractf128 $1, %ymm1, 48(%rcx,%rdx)                   #27.9
        addq      $64, %rcx                                     #14.3
        cmpq      %r8, %rdi                                     #14.3
        jb        ..B1.3        # Prob 82%                      #14.3
                                # LOE rax rdx rcx rbx rbp rsi rdi r8 r12 r13 r14 r15 ymm6 ymm7
..B1.5:                         # Preds ..B1.3 ..B1.1
                                # Execution count [1.00e+00]
        vzeroupper                                              #31.1
        ret                                                     #31.1
        .align    16,0x90
	.cfi_endproc
                                # LOE
# mark_end;
	.type	fxzq3b_,@function
	.size	fxzq3b_,.-fxzq3b_
	.data
# -- End  fxzq3b_
	.section .rodata, "a"
	.align 32
	.align 32
.L_2il0floatpacket.0:
	.long	0x00000000,0xbfe00000,0x00000000,0xbfe00000,0x00000000,0xbfe00000,0x00000000,0xbfe00000
	.type	.L_2il0floatpacket.0,@object
	.size	.L_2il0floatpacket.0,32
	.align 32
.L_2il0floatpacket.1:
	.long	0xe8584caa,0x3febb67a,0xe8584caa,0x3febb67a,0xe8584caa,0x3febb67a,0xe8584caa,0x3febb67a
	.type	.L_2il0floatpacket.1,@object
	.size	.L_2il0floatpacket.1,32
	.data
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
# End
