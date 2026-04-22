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
  INTEGER(8),PARAMETER :: NTR=10
  INTEGER(8),PARAMETER :: JM=2**10
  INTEGER(8),PARAMETER :: IM=2*JM,MM=JM-1
  INTEGER(8),PARAMETER :: NM=MM,NN=MM
  INTEGER(8),PARAMETER :: IPOW=0
  INTEGER(8) :: L,N,M,LR,LI,LAS,ITR,MAXTD,ICPU,IG
  REAL(8) :: RC,RAN,SL,SLMAX,SLAMAX,GFLOPS,TIM0,TIM1
  REAL(8),DIMENSION(:),ALLOCATABLE :: S,SD,R,T
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: IT,JC
  REAL(8),DIMENSION(:),POINTER:: W,G
  REAL(8),DIMENSION(:,:),POINTER:: P
  TYPE(C_PTR) :: PW,PG,PP

  ALLOCATE(S((MM+1)*(MM+1)))
  ALLOCATE(SD((MM+1)*(MM+1)))
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

  RC=1D0*5*IM*LOG(1D0*IM)/LOG(2D0)*0.5D0*JM+1D0*(MM+1)*(MM+1)*JM

  IG=1
  CALL SXINI1(MM,NM,IM,IT,T,R)
  CALL SXINI2(MM,NM,JM,IG,P,R,JC)

  DO L=1,(MM+1)*(MM+1)
     call random_number(RAN)
     S(L)=2*RAN-1
  END DO

  PRINT '(A,I5,A,I5,A,I5,A,I4)','MM=',MM,', IM=',IM,' JM=',JM,', NTR=',NTR

  CALL MXGCPU(ICPU)
  IF(ICPU.EQ.0) THEN
     PRINT '(A)','SSE=fort'
  ELSE IF(ICPU.EQ.10) THEN
     PRINT '(A)','SSE=avx'
  ELSE IF(ICPU.EQ.20) THEN
     PRINT '(A)','SSE=fma'
  ELSE IF(ICPU.EQ.30) THEN
     PRINT '(A)','SSE=avx512'
  ELSE IF(ICPU.EQ.100) THEN
     PRINT '(A)','SSE=sx'
  ELSE IF(ICPU.EQ.1000) THEN
     PRINT '(A)','SSE=fx'
  END IF

  CALL MXGOMP(MAXTD)      
  PRINT '(A,I3)','number of threads =',MAXTD

  CALL MXTIME(TIM0)
  DO ITR=1,NTR
     CALL SXTS2G(MM,NM,NN,IM,JM,S,G,IT,T,P,R,JC,W,IPOW)         
  END DO
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  PRINT '(A,ES9.2,A,F6.1,A)','S2G: ',(TIM1-TIM0)/NTR,' sec  (', &
       & GFLOPS,' GFlops)'

  CALL MXTIME(TIM0)
  DO ITR=1,NTR
     CALL SXTG2S(MM,NM,NN,IM,JM,SD,G,IT,T,P,R,JC,W,IPOW)                  
  END DO
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  PRINT '(A,ES9.2,A,F6.1,A)','G2S: ',(TIM1-TIM0)/NTR,' sec  (', &
       & GFLOPS,' GFlops)'

  SLMAX=0
  SLAMAX=0
  M=0
  DO N=0,MM
     CALL SXNM2L(MM,N,M,L)
     SL=ABS(SD(L)-S(L))
     IF(SL.GT.SLMAX) THEN
        SLMAX=SL
        LAS=L
     END IF
     SLAMAX=SLAMAX+SL**2
  END DO

  DO M=1,MM
     DO N=M,MM
        CALL SXNM2L(MM,N,M,LR)
        CALL SXNM2L(MM,N,-M,LI)
        SL=(SD(LR)-S(LR))**2+(SD(LI)-S(LI))**2
        SL=SQRT(SL)
        IF(SL.GT.SLMAX) THEN
           SLMAX=SL
           LAS=LR
        END IF
        SLAMAX=SLAMAX+SL**2
     END DO
  END DO

  CALL SXL2NM(MM,LAS,N,M)

  PRINT '(A,ES9.2,A,I5,A,I5,A)','maxerror =',SLMAX,' (n=',N,', m=',M,')'
  PRINT '(A,ES9.2)','rmserror =',SQRT(SLAMAX/((MM+1)*(MM+2)/2))

  DEALLOCATE(S)
  DEALLOCATE(SD)
  DEALLOCATE(T)
  DEALLOCATE(IT)      
  DEALLOCATE(R)
  DEALLOCATE(JC)
  CALL MXFREE(PP)      
  CALL MXFREE(PG)
  CALL MXFREE(PW)

END program
