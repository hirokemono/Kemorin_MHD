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
!  多層用の data gather
!-----------------------------------------------------------------------
SUBROUTINE SYKGXX(NDIM,KM,NDV,X,XALL,ICOML)

  IMPLICIT NONE    
  INCLUDE 'mpif.h'
  INTEGER(8) :: NDIM,KM,NDV,ICOML
  INTEGER(8) :: NPH1,NPH2,NA,IKEY,ICOLOR,K1,K2,NK1,NK2,K
  REAL(8) ::  X(NDIM,*),XALL(NDIM,KM)
  REAL(8),ALLOCATABLE :: XBUF(:,:,:)
  INTEGER :: IPL4,NPL4,IERR4,NB4,ICOML4,ICOMS4,ICLR4,IKEY4

  ICOML4=ICOML
  CALL MPI_COMM_SIZE(ICOML4,NPL4,IERR4)
  CALL MPI_COMM_RANK(ICOML4,IPL4,IERR4)

  IF(NDV.GT.NPL4.OR.NDV.GT.KM) THEN
     PRINT*, '*** error in SYKGXX: NDV must <= # of processes and <= KM.'     
     STOP
  END IF

  NPH1=(NPL4-1)/NDV
  NPH2=NPH1+1
  NA=NPL4-NPH1*NDV ! NPH2となる個数
  IF(IPL4.LT.NA*NPH2) THEN
     ICOLOR=IPL4/NPH2
     IKEY=MOD(IPL4,NPH2)
  ELSE
     ICOLOR=NDV-1-(NPL4-IPL4-1)/NPH1
     IKEY=NPH1-1-MOD(NPL4-IPL4-1,NPH1)
  END IF
  ICLR4=ICOLOR
  IKEY4=IKEY
  CALL MPI_COMM_SPLIT(ICOML4,IKEY4,ICLR4,ICOMS4,IERR4)
  
  NK1=(KM-1)/NDV
  NK2=NK1+1
  
  NB4=NDIM*NK2

  IF(IPL4.EQ.0) THEN
     ALLOCATE(XBUF(NDIM,NK2,0:NDV-1))
  ELSE
     ALLOCATE(XBUF(1,1,1))
  END IF

  IF(IKEY.EQ.0) THEN
     CALL MPI_GATHER(X,NB4,MPI_REAL8,XBUF,NB4,MPI_REAL8,0,ICOMS4,IERR4)
  END IF

  IF(IPL4.EQ.0) THEN
     DO ICOLOR=0,NDV-1
        NA=KM-NK1*NDV ! NK2となる個数
        IF(ICOLOR.LT.NA) THEN
           K1=1+NK2*ICOLOR
           K2=K1+NK2-1
        ELSE
           K2=KM-NK1*(NDV-ICOLOR-1)
           K1=K2-NK1+1
        END IF
        DO K=K1,K2
           XALL(:,K)=XBUF(:,K-K1+1,ICOLOR)
        END DO
     END DO
  END IF

  CALL MPI_COMM_FREE(ICOMS4,IERR4)  

  DEALLOCATE(XBUF)    

END SUBROUTINE SYKGXX
