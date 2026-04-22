!***********************************************************************
! ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
! Copyright (C) 1998--2024 Keiichi Ishioka <ishioka@gfd-dennou.org>
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
! 02110-1301 USA.
!***********************************************************************
SUBROUTINE LXGPRM(JM,JV,JR)

  INTEGER(8) :: JM,JV,JR,ICPU

  CALL MXGCPU(ICPU)
  
  IF(ICPU.EQ.0.OR.ICPU.EQ.10.OR.ICPU.EQ.20) THEN ! fort, avx, fma¤Î¾ì¹ç
     IF(MOD(JM,8).EQ.0) THEN
        JV=4
        JR=JM/2/JV
        DO WHILE(MOD(JR,2).EQ.0.AND.JR.GT.128) 
           JR=JR/2
        END DO
     ELSE
        JV=JM/2
        JR=1
     END IF
  ELSE IF(ICPU.EQ.30.OR.ICPU.EQ.1000) THEN ! avx512, fx
     IF(MOD(JM,16).EQ.0) THEN
        JV=8
        JR=JM/2/JV
        DO WHILE(MOD(JR,2).EQ.0.AND.JR.GT.64) 
           JR=JR/2
        END DO
     ELSE
        JV=JM/2
        JR=1
     END IF
  ELSE IF(ICPU.EQ.100) THEN ! SX-Aurora
     IF(MOD(JM,512).EQ.0) THEN
        JV=256
        JR=JM/2/JV
        DO WHILE(MOD(JR,2).EQ.0.AND.JR.GT.64)
           JR=JR/2
        END DO
     ELSE
        JV=JM/2
        JR=1
     END IF
  ELSE
     JV=JM/2
     JR=1
  END IF

END SUBROUTINE LXGPRM
