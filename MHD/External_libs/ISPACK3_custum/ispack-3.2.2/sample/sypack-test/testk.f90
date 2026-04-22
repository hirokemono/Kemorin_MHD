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
  INCLUDE 'mpif.h'
  INTEGER(8),PARAMETER :: JM=2**10,NTR=1
  INTEGER(8),PARAMETER :: MM=JM-1,IM=JM*2
  INTEGER(8),PARAMETER :: NM=MM,NN=NM
  INTEGER(8),PARAMETER :: KM=8 ! number of layers
  INTEGER(8),PARAMETER :: NDV=2 ! number of division in the layer direction
  INTEGER(8) :: JV, N,M,L,LR,LI,LAS,ISEED,ICPU,IPOW,ITR,MAXTD
  INTEGER(8) :: I,J,ICOML,NP,NA
  INTEGER(8) :: K1,K2,ICOMS,NPH
  INTEGER(8) :: K,KS,KSA
  INTEGER :: IERR4,NPL4,IPL4,ICOMS4,IPS4,NPS4,ICOML4
  REAL(8) :: SLMAX,RAN,SLAMAX,SLAMAXK,SL,GFLOPS,RC,TIM0,TIM1  
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: IT
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: JC
  REAL(8),DIMENSION(:),ALLOCATABLE :: T
  REAL(8),DIMENSION(:),ALLOCATABLE :: R
  REAL(8),DIMENSION(:,:),ALLOCATABLE :: S
  REAL(8),DIMENSION(:,:),ALLOCATABLE :: SKALL
  REAL(8),DIMENSION(:,:),ALLOCATABLE :: SKALLD  
  REAL(8),DIMENSION(:,:),ALLOCATABLE :: SALL
  REAL(8),DIMENSION(:),POINTER:: W,P
  REAL(8),DIMENSION(:,:),POINTER:: G
  TYPE(C_PTR) :: PW,PG,PP  
  !$    INTEGER :: omp_get_max_threads

  IF(NDV.GT.KM) THEN
     PRINT*, 'Please set NDV not to be larger than KM. --> stop'
     STOP
  END IF

  RC=1D0*5*IM*LOG(1D0*IM)/LOG(2D0)*0.5D0*JM+1D0*(MM+1)*(MM+1)*JM*KM

  CALL MPI_INIT(IERR4)

  ICOML4=MPI_COMM_WORLD
  ICOML=ICOML4

  CALL MPI_COMM_SIZE(ICOML4,NPL4,IERR4)

  IF(NDV.GT.NPL4) THEN
     PRINT*, 'Please set NDV not to be larger than the number of processes. --> stop'
     STOP
  END IF

  CALL SYQRJV(JM,JV)  
  
  CALL MPI_COMM_RANK(ICOML4,IPL4,IERR4)
  IF(IPL4.EQ.0) THEN
     ALLOCATE(SKALL((MM+1)*(MM+1),KM))
     ALLOCATE(SKALLD((MM+1)*(MM+1),KM))     
     DO K=1,KM
        DO L=1,(MM+1)*(MM+1)
           call random_number(RAN)            
           SKALL(L,K)=2*RAN-1
        END DO
     END DO
  ELSE
     ALLOCATE(SKALL(1,1))
     ALLOCATE(SKALLD(1,1))     
  END IF
  
  CALL SYKINI(KM,NDV,K1,K2,ICOML,ICOMS)
  ICOMS4=ICOMS

  ! 分割して生成された新しいコミュニケーターのIDは,
  ! それぞれ異なっているわけではないことに注意.
  ! (プロセスが違うので. 実際, IP=0以外は IDが同じになる実装のよう).

  CALL MPI_COMM_RANK(ICOMS4,IPS4,IERR4)  
  IF(IPS4.EQ.0) THEN
     ALLOCATE(SALL((MM+1)*(MM+1),(KM-1)/NDV+1))
  ELSE
     ALLOCATE(SALL(1,1))
  END IF

  CALL SYKSXX((MM+1)*(MM+1),KM,NDV,SKALL,SALL,ICOML)

  CALL MPI_COMM_SIZE(ICOMS4,NPS4,IERR4)  

  NP=NPS4

  ALLOCATE(IT(IM/2))
  ALLOCATE(JC((MM/NP+1)*(2*NM-MM/NP*NP)/16+MM/NP+1))
  ALLOCATE(T(IM*3/2))
  ALLOCATE(R(5*(MM/NP+1)*(2*NM-MM/NP*NP)/4+MM/NP+1))
  ALLOCATE(S((MM/NP+1)*(2*(NN+1)-MM/NP*NP),(KM-1)/NDV+1))
  CALL MXALLC(PG,IM*((JM/JV-1)/NP+1)*JV*((KM-1)/NDV+1))
  CALL MXALLC(PW,2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2)
  CALL MXALLC(PP,JM/2*(5+2*(MM/NP+1)))  
  CALL C_F_POINTER(PG, G, [IM*((JM/JV-1)/NP+1)*JV,(KM-1)/NDV+1])
  CALL C_F_POINTER(PW, W, [2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2])
  CALL C_F_POINTER(PP,P,[JM/2*(5+2*(MM/NP+1))])

  CALL SYINI1(MM,NM,IM,IT,T,R,ICOMS)
  CALL SYINI2(MM,NM,JM,1_8,P,R,JC,ICOMS)

  CALL MPI_COMM_RANK(ICOMS4,IPS4,IERR4)

  IF(IPL4.EQ.0) THEN
     PRINT '(A,I5,A,I5,A,I5,A,I2,A,I5,A,I5,A,I4)','MM=',MM,', IM=',IM, &
     ', JM=',JM,', JV=',JV,', KM=',KM,', NDV=',NDV,', NTR=',NTR
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
     MAXTD=1
     !$       MAXTD=omp_get_max_threads()      
     PRINT '(A,I3)','number of threads =',MAXTD
     PRINT '(A,I3)','number of processes =',NPL4
  END IF

  DO K=1,K2-K1+1
     CALL SYSS2S(MM,NN,SALL(1,K),S(1,K),ICOMS)
  END DO
  
  IPOW=0
  CALL MPI_BARRIER(ICOML4,IERR4)
  CALL MXTIME(TIM0)
  DO ITR=1,NTR  
     DO K=1,K2-K1+1
        CALL SYTS2G(MM,NM,NN,IM,JM,JV,S(1,K),G(1,K),IT,T,P,R,JC,W,IPOW,ICOMS)
     END DO
  END DO
  CALL MPI_BARRIER(ICOML4,IERR4)
  CALL MXTIME(TIM1)     
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  IF(IPL4.EQ.0) THEN
     PRINT '(A,ES9.2,A,F6.1,A)',&
          'S2G: ',(TIM1-TIM0)/NTR,' sec  (',GFLOPS,' GFlops)'
  END IF

  IPOW=0
  CALL MPI_BARRIER(ICOML4,IERR4)  
  CALL MXTIME(TIM0)
  DO ITR=1,NTR
     DO K=1,K2-K1+1
        CALL SYTG2S(MM,NM,NN,IM,JM,JV,S(1,K),G(1,K),IT,T,P,R,JC,W,IPOW,ICOMS)
     END DO
  END DO
  CALL MPI_BARRIER(ICOML4,IERR4)    
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  IF(IPL4.EQ.0) THEN
     PRINT '(A,ES9.2,A,F6.1,A)','G2S: ',&
          (TIM1-TIM0)/NTR,' sec  (',GFLOPS,' GFlops)'
  END IF

  DO K=1,K2-K1+1
     CALL SYGS2S(MM,NN,S(1,K),SALL(1,K),ICOMS)
  END DO

  CALL SYKGXX((MM+1)*(MM+1),KM,NDV,SALL,SKALLD,ICOML)
  
  IF(IPL4.EQ.0) THEN
     KS=1
     SLAMAXK=0
     DO K=1,KM
        SLMAX=0
        SLAMAX=0
        M=0
        DO N=0,MM
           CALL SXNM2L(MM,N,M,L)
           SL=ABS(SKALLD(L,K)-SKALL(L,K))
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
              SL=(SKALLD(LR,K)-SKALL(LR,K))**2+(SKALLD(LI,K)-SKALL(LI,K))**2
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
  END IF

  DEALLOCATE(SALL)
  DEALLOCATE(SKALL)
  DEALLOCATE(SKALLD)  
  DEALLOCATE(IT)
  DEALLOCATE(JC)
  DEALLOCATE(T)
  DEALLOCATE(R)
  DEALLOCATE(S)
  CALL MXFREE(PP)      
  CALL MXFREE(PG)
  CALL MXFREE(PW)

  CALL MPI_FINALIZE(IERR4)

END program
