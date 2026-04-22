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
  INTEGER(8) :: JV,N,M,L,LR,LI,LAS,ICPU,IPOW,ITR,MAXTD
  INTEGER(8) :: ICOM
  INTEGER :: IERR,NP,IP
  REAL(8) :: SLMAX,RAN,SLAMAX,SL,GFLOPS,RC,TIM0,TIM1  
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: IT
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: JC
  REAL(8),DIMENSION(:),ALLOCATABLE :: T
  REAL(8),DIMENSION(:),ALLOCATABLE :: R
  REAL(8),DIMENSION(:),ALLOCATABLE :: S
  REAL(8),DIMENSION(:),ALLOCATABLE :: SALL
  REAL(8),DIMENSION(:),ALLOCATABLE :: SDALL
  REAL(8),DIMENSION(:),POINTER:: W,G,P
  TYPE(C_PTR) :: PW,PG,PP  
  !$    INTEGER :: omp_get_max_threads

  RC=1D0*5*IM*LOG(1D0*IM)/LOG(2D0)*0.5D0*JM+1D0*(MM+1)*(MM+1)*JM
  ! 逆変換/正変換の1回あたりの演算数概算

  CALL MPI_INIT(IERR)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)

  ICOM=MPI_COMM_WORLD

  CALL SYQRJV(JM,JV)

  ALLOCATE(IT(IM/2))
  ALLOCATE(JC((MM/NP+1)*(2*NM-MM/NP*NP)/16+MM/NP+1))
  ALLOCATE(T(IM*3/2))
  ALLOCATE(R(5*(MM/NP+1)*(2*NM-MM/NP*NP)/4+MM/NP+1))
  ALLOCATE(S((MM/NP+1)*(2*(NN+1)-MM/NP*NP)))  
  CALL MXALLC(PG,IM*((JM/JV-1)/NP+1)*JV)
  CALL MXALLC(PW,2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2)
  CALL MXALLC(PP,JM/2*(5+2*(MM/NP+1)))  
  CALL C_F_POINTER(PG, G, [IM*((JM/JV-1)/NP+1)*JV])
  CALL C_F_POINTER(PW, W, [2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2])
  CALL C_F_POINTER(PP,P,[JM/2*(5+2*(MM/NP+1))])

  CALL SYINI1(MM,NM,IM,IT,T,R,ICOM)
  CALL SYINI2(MM,NM,JM,1_8,P,R,JC,ICOM)

  IF(IP.EQ.0) THEN
     ALLOCATE(SALL((MM+1)*(MM+1)))
     ALLOCATE(SDALL((MM+1)*(MM+1)))     
     DO L=1,(MM+1)*(MM+1)
        call random_number(RAN)            
        SALL(L)=2*RAN-1
        SDALL(L)=SALL(L)
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
     ALLOCATE(SALL(1))
     ALLOCATE(SDALL(1))              
  END IF

  CALL SYSS2S(MM,NN,SALL,S,ICOM)


  IPOW=0
  CALL MXTIME(TIM0)
  DO ITR=1,NTR
     CALL SYTS2G(MM,NM,NN,IM,JM,JV,S,G,IT,T,P,R,JC,W,IPOW,ICOM)
  END DO
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  IF(IP.EQ.0) THEN
     PRINT '(A,ES9.2,A,F6.1,A)','S2G: ',(TIM1-TIM0)/NTR,' sec  (',  &
          &      GFLOPS,' GFlops)'
  END IF

  IPOW=0
  CALL MXTIME(TIM0)
  DO ITR=1,NTR
     CALL SYTG2S(MM,NM,NN,IM,JM,JV,S,G,IT,T,P,R,JC,W,IPOW,ICOM)
  END DO
  CALL MXTIME(TIM1)
  GFLOPS=RC*NTR/(TIM1-TIM0)/1D9
  IF(IP.EQ.0) THEN
     PRINT '(A,ES9.2,A,F6.1,A)','G2S: ',(TIM1-TIM0)/NTR,' sec  (',  &
          &      GFLOPS,' GFlops)'
  END IF

  CALL SYGS2S(MM,NN,S,SDALL,ICOM)

  IF(IP.EQ.0) THEN
     SLMAX=0
     SLAMAX=0
     M=0
     DO N=0,MM
        CALL SXNM2L(MM,N,M,L)
        SL=ABS(SDALL(L)-SALL(L))
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
           SL=(SDALL(LR)-SALL(LR))**2+(SDALL(LI)-SALL(LI))**2
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

  DEALLOCATE(SALL)
  DEALLOCATE(SDALL)  
  DEALLOCATE(IT)
  DEALLOCATE(JC)
  DEALLOCATE(T)
  DEALLOCATE(R)
  DEALLOCATE(S)
  CALL MXFREE(PP)      
  CALL MXFREE(PG)
  CALL MXFREE(PW)

  CALL MPI_FINALIZE(IERR)

END program
