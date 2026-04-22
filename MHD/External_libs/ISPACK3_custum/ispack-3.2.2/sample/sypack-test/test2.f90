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
  INTEGER(8),PARAMETER :: JM=2**10
  INTEGER(8),PARAMETER :: MM=JM-1,IM=JM*2
  INTEGER(8),PARAMETER :: NM=MM+1,NN=MM,NT=MM
  INTEGER(8) :: JV, N,M,L,LR,LI,LAS,ISEED,ICPU,IPOW,ITR,MAXTD
  INTEGER(8) :: I,J,ICOM
  INTEGER :: IERR,NP,IP
  REAL(8) ::RAN,SL,SLMAX,SLAMAX,EPS=1D-14*IM
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: IT
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: JC
  REAL(8),DIMENSION(:),ALLOCATABLE :: T
  REAL(8),DIMENSION(:),ALLOCATABLE :: R
  REAL(8),DIMENSION(:),ALLOCATABLE :: S
  REAL(8),DIMENSION(:),ALLOCATABLE :: C,D,SD,SX,SXD,SXR,SY,SYD
  REAL(8),DIMENSION(:),ALLOCATABLE :: SALL,SDALL
  REAL(8),DIMENSION(:,:),ALLOCATABLE :: GALL
  REAL(8),DIMENSION(:),POINTER:: W,G,P
  TYPE(C_PTR) :: PW,PG,PP  

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
  ALLOCATE(C((MM/NP+1)*(2*(NT+1)-MM/NP*NP)))
  ALLOCATE(D((MM/NP+1)*(2*(NT+1)-MM/NP*NP)*2))

  ALLOCATE(SD((MM/NP+1)*(2*(NT+1)-MM/NP*NP)))
  ALLOCATE(SX((MM/NP+1)*(2*(NT+1)-MM/NP*NP)))
  ALLOCATE(SXD((MM/NP+1)*(2*(NT+1)-MM/NP*NP)))
  ALLOCATE(SXR((MM/NP+1)*(2*(NT+1+1)-MM/NP*NP)))
  ALLOCATE(SY((MM/NP+1)*(2*(NT+1+1)-MM/NP*NP)))
  ALLOCATE(SYD((MM/NP+1)*(2*(NT+1)-MM/NP*NP)))

  CALL MXALLC(PG,IM*((JM/JV-1)/NP+1)*JV)
  CALL MXALLC(PW,2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2)
  CALL MXALLC(PP,JM/2*(5+2*(MM/NP+1)))  
  CALL C_F_POINTER(PG, G, [IM*((JM/JV-1)/NP+1)*JV])
  CALL C_F_POINTER(PW, W, [2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2])
  CALL C_F_POINTER(PP,P,[JM/2*(5+2*(MM/NP+1))])

  CALL SYINI1(MM,NM,IM,IT,T,R,ICOM)
  CALL SYINI2(MM,NM,JM,1_8,P,R,JC,ICOM)

  CALL SYINIC(MM,NT,C,ICOM)
  CALL SYINID(MM,NT,D,ICOM)

  IF(IP.EQ.0) THEN
     ALLOCATE(SALL((MM+1)*(MM+1)))
     ALLOCATE(SDALL((MM+1)*(MM+1)))     
     DO L=1,(MM+1)*(MM+1)
        call random_number(RAN)            
        SALL(L)=2*RAN-1
     END DO
  ELSE
     ALLOCATE(SALL(1))
     ALLOCATE(SDALL(1))              
  END IF

  CALL SYSS2S(MM,NN,SALL,S,ICOM)

  CALL SYCLAP(MM,NT,S,SD,D,2_8,ICOM)
  CALL SYCLAP(MM,NT,SD,S,D,1_8,ICOM)

  CALL SYCS2X(MM,NT,SD,SX,ICOM)
  CALL SYCRPK(MM,NT,NT+1,SX,SXR,ICOM)
  CALL SYCS2Y(MM,NT,SD,SY,C,ICOM)

  IPOW=1
  CALL SYTS2G(MM,NM,NT+1,IM,JM,JV,SXR,G,IT,T,P,R,JC,W,IPOW,ICOM)
  CALL SYTG2S(MM,NM,NT+1,IM,JM,JV,SXR,G,IT,T,P,R,JC,W,IPOW,ICOM)
  CALL SYTS2G(MM,NM,NT+1,IM,JM,JV,SY,G,IT,T,P,R,JC,W,IPOW,ICOM)
  CALL SYTG2S(MM,NM,NT+1,IM,JM,JV,SY,G,IT,T,P,R,JC,W,IPOW,ICOM)

  CALL SYCY2S(MM,NT,SY,SYD,C,ICOM)
  CALL SYCRPK(MM,NT+1,NT,SXR,SX,ICOM)
  CALL SYCS2X(MM,NT,SX,SXD,ICOM)

  SD=SXD+SYD

  CALL SYGS2S(MM,NT,S,SALL,ICOM)
  CALL SYGS2S(MM,NT,SD,SDALL,ICOM)

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
     print *,'gradient and divergence check:'
     IF(SLMAX.LE.EPS) THEN
        print *,'** OK'
     ELSE
        print *,'** Fail'
     END IF
  END IF

  DEALLOCATE(IT)
  DEALLOCATE(JC)
  DEALLOCATE(T)
  DEALLOCATE(R)
  DEALLOCATE(S)
  DEALLOCATE(SD)
  DEALLOCATE(SX)
  DEALLOCATE(SXD)
  DEALLOCATE(SXR)
  DEALLOCATE(SY)
  DEALLOCATE(SYD)
  DEALLOCATE(SALL)
  DEALLOCATE(SDALL)
  
  CALL MXFREE(PP)      
  CALL MXFREE(PG)
  CALL MXFREE(PW)

  CALL MPI_FINALIZE(IERR)

END program
