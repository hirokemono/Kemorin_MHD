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
!***********************************************************************
!  多層用に MPIのコミュニケーターの分割を行う
!-----------------------------------------------------------------------
SUBROUTINE SYKINI(KM,NDV,K1,K2,ICOML,ICOMS)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: ICOML,ICOMS,NPH,NDV,NPH1,NPH2,NA,IKEY,ICOLOR,KM,K1,K2,NK1,NK2
  INTEGER :: NPL4,IPL4,IERR4,ICOML4,ICOMS4,IKEY4,ICLR4

  ICOML4=ICOML
  CALL MPI_COMM_SIZE(ICOML4,NPL4,IERR4)
  CALL MPI_COMM_RANK(ICOML4,IPL4,IERR4)

  IF(NDV.GT.NPL4.OR.NDV.GT.KM) THEN
     PRINT*, '*** error in SYKINI: NDV must <= # of processes and <= KM.'     
     STOP
  END IF

  NPH1=(NPL4-1)/NDV
  NPH2=NPH1+1
  NA=NPL4-NPH1*NDV ! NPH2となる個数
  IF(IPL4.LT.NA*NPH2) THEN
     ICOLOR=IPL4/NPH2
     IKEY=MOD(IPL4,NPH2)
     NPH=NPH2
  ELSE
     ICOLOR=NDV-1-(NPL4-IPL4-1)/NPH1
     IKEY=NPH1-1-MOD(NPL4-IPL4-1,NPH1)
     NPH=NPH1
  END IF
  ICLR4=ICOLOR
  IKEY4=IKEY
  CALL MPI_COMM_SPLIT(ICOML4,ICLR4,IKEY4,ICOMS4,IERR4)
  ICOMS=ICOMS4

  NK1=(KM-1)/NDV
  NK2=NK1+1
  NA=KM-NK1*NDV ! NK2となる個数
  IF(ICOLOR.LT.NA) THEN
     K1=1+NK2*ICOLOR
     K2=K1+NK2-1
  ELSE
     K2=KM-NK1*(NDV-ICOLOR-1)
     K1=K2-NK1+1
  END IF

END SUBROUTINE SYKINI
