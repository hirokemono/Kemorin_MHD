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
	.file "fxzq5b-fort.f90"
	.text
..TXTST0:
# -- Begin  fxzq5b_
	.text
# mark_begin;
       .align    16,0x90
	.globl fxzq5b_
# --- FXZQ5B
fxzq5b_:
# parameter 1: %rdi
# parameter 2: %rsi
..B1.1:                         # Preds ..B1.0
                                # Execution count [1.00e+00]
	.cfi_startproc
..___tag_value_fxzq5b_.1:
..L2:
                                                          #4.12
        movq      $0x6666666666666667, %rax                     #18.9
        movq      (%rdi), %rcx                                  #4.12
        xorl      %r10d, %r10d                                  #16.3
        imulq     %rcx                                          #18.9
        sarq      $1, %rdx                                      #18.9
        xorl      %r9d, %r9d                                    #17.6
        sarq      $63, %rcx                                     #18.9
        subq      %rcx, %rdx                                    #18.9
        movslq    %edx, %rcx                                    #18.9
        movq      %rcx, %rax                                    #18.13
        shlq      $6, %rax                                      #18.13
        testq     %rdx, %rdx                                    #16.3
        jle       ..B1.5        # Prob 2%                       #16.3
                                # LOE rax rdx rcx rbx rbp rsi r9 r10 r12 r13 r14 r15
..B1.2:                         # Preds ..B1.1
                                # Execution count [9.79e-01]
        movq      %rcx, %rdi                                    #18.25
        lea       (%rsi,%rax), %r8                              #18.13
        shlq      $8, %rdi                                      #18.25
        lea       (%rax,%rax,2), %rax                           #22.25
        shlq      $7, %rcx                                      #22.13
        addq      %rsi, %rdi                                    #18.25
        vmovupd   .L_2il0floatpacket.0(%rip), %ymm12            #26.19
        addq      %rsi, %rcx                                    #22.13
        vmovupd   .L_2il0floatpacket.3(%rip), %ymm0             #42.27
        addq      %rsi, %rax                                    #22.25
                                # LOE rax rdx rcx rbx rbp rsi rdi r8 r9 r10 r12 r13 r14 r15 ymm0 ymm12
..B1.3:                         # Preds ..B1.3 ..B1.2
                                # Execution count [5.44e+00]
        vmovupd   (%r9,%rdi), %xmm10                            #18.25
        vmovupd   32(%r9,%r8), %xmm2                            #19.13
        vmovupd   (%r9,%r8), %xmm8                              #18.13
        vmovupd   (%r9,%rax), %xmm5                             #22.25
        vmovupd   32(%r9,%rdi), %xmm1                           #19.25
        vmovupd   (%r9,%rcx), %xmm6                             #22.13
        incq      %r10                                          #16.3
        vinsertf128 $1, 16(%r9,%rdi), %ymm10, %ymm7             #18.25
        vinsertf128 $1, 48(%r9,%r8), %ymm2, %ymm9               #19.13
        vmovupd   32(%r9,%rcx), %xmm10                          #23.13
        vmovupd   32(%r9,%rax), %xmm2                           #23.25
        vinsertf128 $1, 16(%r9,%r8), %ymm8, %ymm11              #18.13
        vsubpd    %ymm7, %ymm11, %ymm4                          #18.9
        vaddpd    %ymm7, %ymm11, %ymm15                         #20.9
        vinsertf128 $1, 48(%r9,%rcx), %ymm10, %ymm11            #23.13
        vinsertf128 $1, 48(%r9,%rax), %ymm2, %ymm7              #23.25
        vinsertf128 $1, 16(%r9,%rax), %ymm5, %ymm14             #22.25
        vsubpd    %ymm7, %ymm11, %ymm5                          #23.9
        vinsertf128 $1, 48(%r9,%rdi), %ymm1, %ymm8              #19.25
        vsubpd    %ymm8, %ymm9, %ymm3                           #19.9
        vaddpd    %ymm8, %ymm9, %ymm13                          #21.9
        vaddpd    %ymm7, %ymm11, %ymm1                          #25.9
        vmovupd   (%r9,%rsi), %xmm11                            #34.13
        vmulpd    %ymm5, %ymm12, %ymm8                          #27.19
        vaddpd    %ymm8, %ymm3, %ymm8                           #27.9
        vmulpd    %ymm12, %ymm3, %ymm3                          #29.15
        vmulpd    %ymm8, %ymm0, %ymm8                           #46.27
        vsubpd    %ymm5, %ymm3, %ymm5                           #29.9
        vaddpd    %ymm1, %ymm13, %ymm3                          #31.9
        vsubpd    %ymm1, %ymm13, %ymm13                         #33.9
        vmulpd    %ymm5, %ymm0, %ymm5                           #42.27
        vinsertf128 $1, 16(%r9,%rcx), %ymm6, %ymm9              #22.13
        vsubpd    %ymm14, %ymm9, %ymm6                          #22.9
        vaddpd    %ymm14, %ymm9, %ymm14                         #24.9
        vmulpd    %ymm6, %ymm12, %ymm9                          #26.19
        vaddpd    %ymm14, %ymm15, %ymm10                        #30.9
        vsubpd    %ymm14, %ymm15, %ymm15                        #32.9
        vaddpd    %ymm9, %ymm4, %ymm9                           #26.9
        vmulpd    %ymm12, %ymm4, %ymm4                          #28.15
        vmovupd   .L_2il0floatpacket.2(%rip), %ymm14            #36.19
        vmulpd    %ymm9, %ymm0, %ymm9                           #47.27
        vsubpd    %ymm6, %ymm4, %ymm6                           #28.9
        vmovupd   32(%r9,%rsi), %xmm4                           #35.13
        vmulpd    %ymm13, %ymm14, %ymm13                        #37.19
        vmulpd    %ymm15, %ymm14, %ymm15                        #36.19
        vmulpd    %ymm6, %ymm0, %ymm6                           #43.27
        vinsertf128 $1, 16(%r9,%rsi), %ymm11, %ymm2             #34.13
        vmovupd   .L_2il0floatpacket.1(%rip), %ymm11            #34.27
        vmulpd    %ymm10, %ymm11, %ymm7                         #34.27
        vmulpd    %ymm3, %ymm11, %ymm11                         #35.27
        vsubpd    %ymm7, %ymm2, %ymm7                           #34.9
        vaddpd    %ymm2, %ymm10, %ymm2                          #40.9
        vsubpd    %ymm15, %ymm7, %ymm15                         #36.9
        vaddpd    %ymm7, %ymm7, %ymm1                           #38.16
        vsubpd    %ymm5, %ymm15, %ymm10                         #42.9
        vsubpd    %ymm15, %ymm1, %ymm7                          #38.9
        vinsertf128 $1, 48(%r9,%rsi), %ymm4, %ymm4              #35.13
        vsubpd    %ymm11, %ymm4, %ymm11                         #35.9
        vaddpd    %ymm4, %ymm3, %ymm3                           #41.9
        vaddpd    %ymm15, %ymm15, %ymm4                         #44.24
        vsubpd    %ymm13, %ymm11, %ymm13                        #37.9
        vaddpd    %ymm11, %ymm11, %ymm14                        #39.16
        vaddpd    %ymm6, %ymm13, %ymm6                          #43.9
        vaddpd    %ymm13, %ymm13, %ymm15                        #45.24
        vsubpd    %ymm13, %ymm14, %ymm11                        #39.9
        vaddpd    %ymm9, %ymm11, %ymm9                          #47.9
        vaddpd    %ymm11, %ymm11, %ymm11                        #49.24
        vsubpd    %ymm6, %ymm15, %ymm1                          #45.9
        vmovupd   %xmm2, (%r9,%rsi)                             #40.9
        vmovupd   %xmm3, 32(%r9,%rsi)                           #41.9
        vextractf128 $1, %ymm2, 16(%r9,%rsi)                    #40.9
        vextractf128 $1, %ymm3, 48(%r9,%rsi)                    #41.9
        vmovupd   %xmm6, 32(%r9,%rax)                           #43.9
        vextractf128 $1, %ymm6, 48(%r9,%rax)                    #43.9
        vmovupd   %xmm10, (%r9,%rax)                            #42.9
        vextractf128 $1, %ymm10, 16(%r9,%rax)                   #42.9
        vsubpd    %ymm10, %ymm4, %ymm4                          #44.9
        vmovupd   %xmm1, 32(%r9,%rcx)                           #45.9
        vextractf128 $1, %ymm1, 48(%r9,%rcx)                    #45.9
        vsubpd    %ymm8, %ymm7, %ymm1                           #46.9
        vaddpd    %ymm7, %ymm7, %ymm7                           #48.24
        vmovupd   %xmm4, (%r9,%rcx)                             #44.9
        vextractf128 $1, %ymm4, 16(%r9,%rcx)                    #44.9
        vmovupd   %xmm1, (%r9,%rdi)                             #46.9
        vmovupd   %xmm9, 32(%r9,%rdi)                           #47.9
        vextractf128 $1, %ymm1, 16(%r9,%rdi)                    #46.9
        vextractf128 $1, %ymm9, 48(%r9,%rdi)                    #47.9
        vsubpd    %ymm1, %ymm7, %ymm7                           #48.9
        vsubpd    %ymm9, %ymm11, %ymm1                          #49.9
        vmovupd   %xmm7, (%r9,%r8)                              #48.9
        vmovupd   %xmm1, 32(%r9,%r8)                            #49.9
        vextractf128 $1, %ymm7, 16(%r9,%r8)                     #48.9
        vextractf128 $1, %ymm1, 48(%r9,%r8)                     #49.9
        addq      $64, %r9                                      #16.3
        cmpq      %rdx, %r10                                    #16.3
        jb        ..B1.3        # Prob 82%                      #16.3
                                # LOE rax rdx rcx rbx rbp rsi rdi r8 r9 r10 r12 r13 r14 r15 ymm0 ymm12
..B1.5:                         # Preds ..B1.3 ..B1.1
                                # Execution count [1.00e+00]
        vzeroupper                                              #53.1
        ret                                                     #53.1
        .align    16,0x90
	.cfi_endproc
                                # LOE
# mark_end;
	.type	fxzq5b_,@function
	.size	fxzq5b_,.-fxzq5b_
	.data
# -- End  fxzq5b_
	.section .rodata, "a"
	.align 32
	.align 32
.L_2il0floatpacket.0:
	.long	0x372fe950,0x3fe3c6ef,0x372fe950,0x3fe3c6ef,0x372fe950,0x3fe3c6ef,0x372fe950,0x3fe3c6ef
	.type	.L_2il0floatpacket.0,@object
	.size	.L_2il0floatpacket.0,32
	.align 32
.L_2il0floatpacket.1:
	.long	0x00000000,0x3fd00000,0x00000000,0x3fd00000,0x00000000,0x3fd00000,0x00000000,0x3fd00000
	.type	.L_2il0floatpacket.1,@object
	.size	.L_2il0floatpacket.1,32
	.align 32
.L_2il0floatpacket.2:
	.long	0x9b97f4a8,0x3fe1e377,0x9b97f4a8,0x3fe1e377,0x9b97f4a8,0x3fe1e377,0x9b97f4a8,0x3fe1e377
	.type	.L_2il0floatpacket.2,@object
	.size	.L_2il0floatpacket.2,32
	.align 32
.L_2il0floatpacket.3:
	.long	0x134454ff,0xbfee6f0e,0x134454ff,0xbfee6f0e,0x134454ff,0xbfee6f0e,0x134454ff,0xbfee6f0e
	.type	.L_2il0floatpacket.3,@object
	.size	.L_2il0floatpacket.3,32
	.data
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
# End
