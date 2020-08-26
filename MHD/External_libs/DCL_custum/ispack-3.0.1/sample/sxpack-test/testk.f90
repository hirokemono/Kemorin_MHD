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
  USE ISO_C_BINDING
  IMPLICIT NONE
  INTEGER(8),PARAMETER :: NTR=32
  INTEGER(8),PARAMETER :: KM=16
!  INTEGER(8),PARAMETER :: JM=2**10
  INTEGER(8),PARAMETER :: JM=2**8
  INTEGER(8),PARAMETER :: IM=2*JM,MM=JM-1
  INTEGER(8),PARAMETER :: NM=MM,NN=MM
  INTEGER(8),PARAMETER :: IPOW=0
  INTEGER(8) :: K,L,N,M,LR,LI,LAS,ITR,MAXTD,ICPU,IG,KS,KSA
  REAL(8) :: RC,RAN,SL,SLMAX,SLAMAX,GFLOPS,TIM0,TIM1,SLAMAXK
  REAL(8),DIMENSION(:),ALLOCATABLE :: R,T
  REAL(8),DIMENSION(:,:),ALLOCATABLE :: S,SD
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: IT,JC
  REAL(8),DIMENSION(:,:),POINTER:: W,G
  REAL(8),DIMENSION(:,:),POINTER:: P
  TYPE(C_PTR) :: PW,PG,PP
  INTEGER :: omp_get_thread_num

  ALLOCATE(S((MM+1)*(MM+1),KM))
  ALLOCATE(SD((MM+1)*(MM+1),KM))
  ALLOCATE(T(IM*3/2))
  ALLOCATE(IT(IM/2))
  ALLOCATE(R(((MM+1)*(2*NM-MM-1)+1)/4*3+(2*NM-MM)*(MM+1)/2+MM+1))
  ALLOCATE(JC(MM*(2*NM-MM-1)/16+MM))
  CALL MXALLC(PP,JM/2*(2*MM+5))      
  CALL MXALLC(PG,JM*IM*KM)
  CALL MXALLC(PW,JM*IM*KM)
  CALL C_F_POINTER(PG, G, [JM*IM,KM])
  CALL C_F_POINTER(PW, W, [JM*IM,KM])
  CALL C_F_POINTER(PP,P,[JM/2,2*MM+5])

  RC=1D0*5*IM*LOG(1D0*IM)/LOG(2D0)*0.5D0*JM+1D0*(MM+1)*(MM+1)*JM*KM

  IG=1
  CALL SXINI1(MM,NM,IM,IT,T,R)
  CALL SXINI2(MM,NM,JM,IG,P,R,JC)

  DO K=1,KM
     DO L=1,(MM+1)*(MM+1)
        call random_number(RAN)
        S(L,K)=2*RAN-1
     END DO
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
  END IF

  CALL MXSOMP(1_8)  
  CALL MXGOMP(MAXTD)      
  PRINT '(A,I3)','number of threads =',MAXTD

  CALL MXTIME(TIM0)
  !$omp parallel private(ITR)
  !$omp do schedule(dynamic)
  DO K=1,KM
!     !$  print*, omp_get_thread_num()
          DO ITR=1,NTR  
        CALL SXTS2G(MM,NM,NN,IM,JM,S(1,K),G(1,K),IT,T,P,R,JC,W(1,K),IPOW)
     END DO
  END DO     
  !$omp end do
  !$omp end parallel
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  PRINT '(A,ES9.2,A,F6.1,A)','S2G: ',(TIM1-TIM0)/NTR,' sec  (', &
       & GFLOPS,' GFlops)'

  CALL MXTIME(TIM0)
  !$omp parallel private(ITR)
  !$omp do schedule(dynamic)
  DO K=1,KM  
     DO ITR=1,NTR
        CALL SXTG2S(MM,NM,NN,IM,JM,SD(1,K),G(1,K),IT,T,P,R,JC,W(1,K),IPOW)
     END DO
  END DO
  !$omp end do
  !$omp end parallel
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  PRINT '(A,ES9.2,A,F6.1,A)','G2S: ',(TIM1-TIM0)/NTR,' sec  (', &
       & GFLOPS,' GFlops)'

  KSA=1
  KS=1
  SLAMAXK=0
  DO K=1,KM
     SLMAX=0
     SLAMAX=0
     M=0
     DO N=0,MM
        CALL SXNM2L(MM,N,M,L)
        SL=ABS(SD(L,K)-S(L,K))
        IF(SL.GT.SLMAX) THEN
           SLMAX=SL
           LAS=L
           KS=K
        END IF
        SLAMAX=SLAMAX+SL**2
     END DO

     DO M=1,MM
        DO N=M,MM
           CALL SXNM2L(MM,N,M,LR)
           CALL SXNM2L(MM,N,-M,LI)
           SL=(SD(LR,K)-S(LR,K))**2+(SD(LI,K)-S(LI,K))**2
           SL=SQRT(SL)
           IF(SL.GT.SLMAX) THEN
              SLMAX=SL
              LAS=LR
              KS=K              
           END IF
           SLAMAX=SLAMAX+SL**2
        END DO
     END DO
     IF(SLAMAX.GT.SLAMAXK) THEN
        SLAMAXK=SLAMAX
        KSA=K
     END IF
  END DO
  CALL SXL2NM(MM,LAS,N,M)  

  PRINT '(A,ES9.2,A,I5,A,I5,A,I5,A)',&
       'maxerror =',SLMAX,' (n=',N,', m=',M,', k=',KS,')'
  PRINT '(A,ES9.2,A,I5,A)','rmserror =',SQRT(SLAMAXK/((MM+1)*(MM+2)/2)),' (k=',KSA,')'

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
