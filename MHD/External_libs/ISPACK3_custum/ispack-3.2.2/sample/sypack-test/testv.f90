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
  INTEGER(8),PARAMETER :: JM=2**10,NTR=10
  INTEGER(8),PARAMETER :: MM=JM-1,IM=JM*2
  INTEGER(8),PARAMETER :: NM=MM,NN=NM
  INTEGER(8) :: JV,N,M,L,LR,LI,LAS,ISEED,ICPU,IPOW,ITR,MAXTD
  INTEGER(8) :: I,J,ICOM
  INTEGER :: IERR,NP,IP
  REAL(8) :: SLMAX,RAN,SLAMAX,SL,GFLOPS,RC,TIM0,TIM1  
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: IT
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: JC
  REAL(8),DIMENSION(:),ALLOCATABLE :: T
  REAL(8),DIMENSION(:),ALLOCATABLE :: R
  REAL(8),DIMENSION(:),ALLOCATABLE :: S1
  REAL(8),DIMENSION(:),ALLOCATABLE :: S2 
  REAL(8),DIMENSION(:),ALLOCATABLE :: S1ALL,S2ALL
  REAL(8),DIMENSION(:),ALLOCATABLE :: S1DALL,S2DALL
  REAL(8),DIMENSION(:),POINTER:: W,G1,G2,P
  TYPE(C_PTR) :: PW,PG1,PG2,PP  
  !$    INTEGER :: omp_get_max_threads

  RC=1D0*5*IM*LOG(1D0*IM)/LOG(2D0)*0.5D0*JM+1D0*(MM+1)*(MM+1)*JM
  ! 逆変換/正変換の1回あたりの演算数概算

  RC=RC*2 ! because of paired transform  
  
  CALL MPI_INIT(IERR)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)

  ICOM=MPI_COMM_WORLD

  CALL SYQRJV(JM,JV)  

  ALLOCATE(IT(IM/2))
  ALLOCATE(JC((MM/NP+1)*(2*NM-MM/NP*NP)/16+MM/NP+1))
  ALLOCATE(T(IM*3/2))
  ALLOCATE(R(5*(MM/NP+1)*(2*NM-MM/NP*NP)/4+MM/NP+1))
  ALLOCATE(S1((MM/NP+1)*(2*(NN+1)-MM/NP*NP)))
  ALLOCATE(S2((MM/NP+1)*(2*(NN+1)-MM/NP*NP)))    
  CALL MXALLC(PG1,IM*((JM/JV-1)/NP+1)*JV)
  CALL MXALLC(PG2,IM*((JM/JV-1)/NP+1)*JV)  
  CALL MXALLC(PW,2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2*2)  
  CALL MXALLC(PP,JM/2*(5+2*(MM/NP+1)))  
  CALL C_F_POINTER(PG1, G1, [IM*((JM/JV-1)/NP+1)*JV])
  CALL C_F_POINTER(PG2, G2, [IM*((JM/JV-1)/NP+1)*JV])  
  CALL C_F_POINTER(PW, W, [2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2*2])
  CALL C_F_POINTER(PP,P,[JM/2*(5+2*(MM/NP+1))])

  CALL SYINI1(MM,NM,IM,IT,T,R,ICOM)
  CALL SYINI2(MM,NM,JM,1_8,P,R,JC,ICOM)

  IF(IP.EQ.0) THEN
     ALLOCATE(S1ALL((MM+1)*(MM+1)))
     ALLOCATE(S2ALL((MM+1)*(MM+1)))
     ALLOCATE(S1DALL((MM+1)*(MM+1)))          
     ALLOCATE(S2DALL((MM+1)*(MM+1)))     
     DO L=1,(MM+1)*(MM+1)
        call random_number(RAN)            
        S1ALL(L)=2*RAN-1
        S2ALL(L)=-S1ALL(L)
        S1DALL(L)=S1ALL(L)
        S2DALL(L)=S2ALL(L)
     END DO
     PRINT '(A,I5,A,I5,A,I5,A,I5,A,I4)','MM=',MM,', IM=',IM,' JM=',JM,' JV=',JV,', NTR=',NTR
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
     PRINT '(A,I3)','number of processes =',NP
  ELSE
     ALLOCATE(S1ALL(1))
     ALLOCATE(S2ALL(1))     
     ALLOCATE(S1DALL(1))
     ALLOCATE(S2DALL(1))
  END IF

  CALL SYSS2S(MM,NN,S1ALL,S1,ICOM)
  CALL SYSS2S(MM,NN,S2ALL,S2,ICOM)  


  IPOW=0
  CALL MXTIME(TIM0)
  DO ITR=1,NTR
     CALL SYTS2V(MM,NM,NN,IM,JM,JV,S1,S2,G1,G2,IT,T,P,R,JC,W,IPOW,ICOM)
  END DO
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  IF(IP.EQ.0) THEN
     PRINT '(A,ES9.2,A,F6.1,A)','S2V: ',(TIM1-TIM0)/NTR,' sec  (',  &
          &      GFLOPS,' GFlops)'
  END IF

  IPOW=0
  CALL MXTIME(TIM0)
  DO ITR=1,NTR
     CALL SYTV2S(MM,NM,NN,IM,JM,JV,S1,S2,G1,G2,IT,T,P,R,JC,W,IPOW,ICOM)
  END DO
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  IF(IP.EQ.0) THEN
     PRINT '(A,ES9.2,A,F6.1,A)','V2S: ',(TIM1-TIM0)/NTR,' sec  (',  &
          &      GFLOPS,' GFlops)'
  END IF

  CALL SYGS2S(MM,NN,S1,S1DALL,ICOM)
  CALL SYGS2S(MM,NN,S2,S2DALL,ICOM)  

  IF(IP.EQ.0) THEN
     SLMAX=0
     SLAMAX=0
     M=0
     DO N=0,MM
        CALL SXNM2L(MM,N,M,L)
        SL=ABS(S1DALL(L)-S1ALL(L))
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
           SL=(S1DALL(LR)-S1ALL(LR))**2+(S1DALL(LI)-S1ALL(LI))**2
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

     SLMAX=0
     SLAMAX=0
     M=0
     DO N=0,MM
        CALL SXNM2L(MM,N,M,L)
        SL=ABS(S2DALL(L)-S2ALL(L))
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
           SL=(S2DALL(LR)-S2ALL(LR))**2+(S2DALL(LI)-S2ALL(LI))**2
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
  END IF

  DEALLOCATE(S1ALL)
  DEALLOCATE(S2ALL)  
  DEALLOCATE(S1DALL)
  DEALLOCATE(S2DALL)    
  DEALLOCATE(IT)
  DEALLOCATE(JC)
  DEALLOCATE(T)
  DEALLOCATE(R)
  DEALLOCATE(S1)
  DEALLOCATE(S2)  
  CALL MXFREE(PP)      
  CALL MXFREE(PG1)
  CALL MXFREE(PG2)  
  CALL MXFREE(PW)

  CALL MPI_FINALIZE(IERR)

END program
