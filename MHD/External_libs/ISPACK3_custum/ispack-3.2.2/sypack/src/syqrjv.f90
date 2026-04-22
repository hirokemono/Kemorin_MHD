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
SUBROUTINE SYQRJV(JM,JV)

  INTEGER(8) :: JM,JV,ICPU

  CALL MXGCPU(ICPU)
  
  IF(ICPU.EQ.0.OR.ICPU.EQ.10.OR.ICPU.EQ.20) THEN ! 一般的なスカラー計算機
     IF(MOD(JM,8).EQ.0) THEN
        JV=4
     ELSE
        JV=JM/2
     END IF
  ELSE IF(ICPU.EQ.30.OR.ICPU.EQ.1000) THEN ! avx512 or fx
     IF(MOD(JM,16).EQ.0) THEN
        JV=8
     ELSE
        JV=JM/2
     END IF
  ELSE IF(ICPU.EQ.100) THEN ! SX-Aurora
     IF(MOD(JM,512).EQ.0) THEN
        JV=256
     ELSE
        JV=JM/2
     END IF
  ELSE
     JV=JM/2
  END IF

END SUBROUTINE SYQRJV
