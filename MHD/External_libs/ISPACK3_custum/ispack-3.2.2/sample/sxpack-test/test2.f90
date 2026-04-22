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
  USE ISO_C_BINDING
  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JM=2**10
  INTEGER(8),PARAMETER :: IM=2*JM,MM=JM-1
  INTEGER(8),PARAMETER :: NM=MM+1,NT=MM
  INTEGER(8) :: L,N,M,LR,LI,LAS,IG
  REAL(8) :: RAN,SL,SLMAX,SLAMAX,EPS=1D-14*IM
  REAL(8),DIMENSION(:),ALLOCATABLE :: S,SD,SX,SXD,SXR,SY,SYD,R,T,C,D
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: IT,JC
  REAL(8),DIMENSION(:),POINTER:: W,G
  REAL(8),DIMENSION(:,:),POINTER:: P
  TYPE(C_PTR) :: PW,PG,PP

  ALLOCATE(S(NT+1+MM*(2*NT-MM+1)))
  ALLOCATE(SD(NT+1+MM*(2*NT-MM+1)))
  ALLOCATE(SX(NT+1+MM*(2*NT-MM+1)))
  ALLOCATE(SXD(NT+1+MM*(2*NT-MM+1)))
  ALLOCATE(SXR(NT+2+MM*(2*NT-MM+3)))
  ALLOCATE(SY(NT+2+MM*(2*NT-MM+3)))
  ALLOCATE(SYD(NT+1+MM*(2*NT-MM+1)))
  ALLOCATE(C(NT+1+MM*(2*NT-MM+1)))
  ALLOCATE(D((NT+1+MM*(2*NT-MM+1))*2))

  ALLOCATE(T(IM*3/2))
  ALLOCATE(IT(IM/2))
  ALLOCATE(R(((MM+1)*(2*NM-MM-1)+1)/4*3+(2*NM-MM)*(MM+1)/2+MM+1))
  ALLOCATE(JC(MM*(2*NM-MM-1)/16+MM))
  CALL MXALLC(PP,JM/2*(2*MM+5))      
  CALL MXALLC(PG,JM*IM)
  CALL MXALLC(PW,JM*IM)
  CALL C_F_POINTER(PG, G, [JM*IM])
  CALL C_F_POINTER(PW, W, [JM*IM])
  CALL C_F_POINTER(PP,P,[JM/2,2*MM+5])

  IG=1
  CALL SXINI1(MM,NM,IM,IT,T,R)
  CALL SXINI2(MM,NM,JM,IG,P,R,JC)
  CALL SXINIC(MM,NT,C)
  CALL SXINID(MM,NT,D)

  DO L=1,NT+1+MM*(2*NT-MM+1)
     call random_number(RAN)
     S(L)=2*RAN-1
  END DO

  S(1)=0
  CALL SXCLAP(MM,NT,S,SD,D,2_8)
  CALL SXCLAP(MM,NT,SD,S,D,1_8)

  CALL SXCS2X(MM,NT,SD,SX)
  CALL SXCRPK(MM,NT,NT+1,SX,SXR)
  CALL SXCS2Y(MM,NT,SD,SY,C)

  CALL SXTS2G(MM,NM,NT+1,IM,JM,SXR,G,IT,T,P,R,JC,W,1_8)         
  CALL SXTG2S(MM,NM,NT+1,IM,JM,SXR,G,IT,T,P,R,JC,W,1_8)
  CALL SXTS2G(MM,NM,NT+1,IM,JM,SY,G,IT,T,P,R,JC,W,1_8)         
  CALL SXTG2S(MM,NM,NT+1,IM,JM,SY,G,IT,T,P,R,JC,W,1_8)

  CALL SXCY2S(MM,NT,SY,SYD,C)
  CALL SXCRPK(MM,NT+1,NT,SXR,SX)
  CALL SXCS2X(MM,NT,SX,SXD)

  SD=SXD+SYD

  SLMAX=0
  SLAMAX=0
  M=0
  DO N=0,NT
     CALL SXNM2L(NT,N,M,L)
     SL=ABS(SD(L)-S(L))
     IF(SL.GT.SLMAX) THEN
        SLMAX=SL
        LAS=L
     END IF
     SLAMAX=SLAMAX+SL**2
  END DO

  DO M=1,MM
     DO N=M,NT
        CALL SXNM2L(NT,N,M,LR)
        CALL SXNM2L(NT,N,-M,LI)
        SL=(SD(LR)-S(LR))**2+(SD(LI)-S(LI))**2
        SL=SQRT(SL)
        IF(SL.GT.SLMAX) THEN
           SLMAX=SL
           LAS=LR
        END IF
        SLAMAX=SLAMAX+SL**2
     END DO
  END DO

  CALL SXL2NM(NT,LAS,N,M)
  PRINT '(A,ES9.2,A,I5,A,I5,A)','maxerror =',SLMAX,' (n=',N,', m=',M,')'
  PRINT '(A,ES9.2)','rmserror =',SQRT(SLAMAX/((MM+1)*(2*NT-MM+2)/2))
  print *,'gradient and divergence check:'
  IF(SLMAX.LE.EPS) THEN
     print *,'** OK'
  ELSE
     print *,'** Fail'
  END IF

  DEALLOCATE(S)
  DEALLOCATE(SD)
  DEALLOCATE(SX)
  DEALLOCATE(SXD)
  DEALLOCATE(SXR)
  DEALLOCATE(SY)
  DEALLOCATE(SYD)
  DEALLOCATE(C)
  DEALLOCATE(D)
  DEALLOCATE(T)
  DEALLOCATE(IT)      
  DEALLOCATE(R)
  DEALLOCATE(JC)
  CALL MXFREE(PP)      
  CALL MXFREE(PG)
  CALL MXFREE(PW)

END program
