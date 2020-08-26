!***********************************************************************
! ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
! Copyright (C) 1998--2019 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
SUBROUTINE SXGPRM(JM,JV,JR)

  INTEGER(8) :: JM,JV,JR,ICPU

  CALL MXGCPU(ICPU)
  
  IF(ICPU.EQ.0.OR.ICPU.EQ.10.OR.ICPU.EQ.20) THEN ! ����Ū�ʥ����顼�׻���
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
  ELSE IF(ICPU.EQ.30) THEN ! avx512
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
  ELSE
     IF(MOD(JM,8).EQ.0) THEN
        JR=4
        JV=JM/8
     ELSE IF(MOD(JM,4).EQ.0) THEN
        JR=2
        JV=JM/4
     ELSE
        JR=1
        JV=JM/2
     END IF
  END IF

END SUBROUTINE SXGPRM
