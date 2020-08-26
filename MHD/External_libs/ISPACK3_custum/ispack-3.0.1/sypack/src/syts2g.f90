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
SUBROUTINE SYTS2G(MM,NM,NN,IM,JM,JV,S,G,IT,T,P,R,JC,W,IPOW,ICOM)

  USE ISO_C_BINDING  
  IMPLICIT NONE  
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,NM,NN,IM,JM,JV,JR,M,IE,IJ,JD,NS,IPOW,ICOM
  INTEGER(8) :: JI,JP1,JP2,JS,JE,K,I,IV,J,NBF
  INTEGER(8) :: IPDEST,IPSRC
  INTEGER(8) :: MN,M1,N1
  INTEGER(8) :: ITH,NTH,NTHMAX
  REAL(8) :: S(*) ! S((MM/NP+1)*(2*(NN+1)-MM/NP*NP))
  REAL(8) :: G(*) ! G(0:IM-1,((JM/JV-1)/NP+1)*JV)
  INTEGER(8) :: IT(IM/2)  
  REAL(8) :: T(IM*3/2)
  REAL(8) :: P(JM/2,*) !  P(JM/2,5+2*(MM/NP+1))
  REAL(8) :: R(*) ! R(5*(MM/NP+1)*(2*NM-MM/NP*NP)/4+MM/NP+1)
  INTEGER(8) :: JC(*) ! JC((MM/NP+1)*(2*NM-MM/NP*NP)/16+MM/NP+1)
  REAL(8) :: W(*) ! W(2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2)
  TYPE(C_PTR),ALLOCATABLE :: WSP(:)
  TYPE(C_PTR),ALLOCATABLE :: WKP(:)
  TYPE(C_PTR),ALLOCATABLE :: WGP(:)
  LOGICAL,ALLOCATABLE :: LP(:)  
  REAL(8),POINTER:: WS(:)
  REAL(8),POINTER:: WK(:)
  REAL(8),POINTER:: WG(:)
  INTEGER :: IP,NP,IERR,NB,ICOM4

  !$ INTEGER :: omp_get_thread_num

  CALL SYGPRM(JM,JV,JR)

  CALL MXGOMP(NTHMAX)    

  ICOM4=ICOM
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  JI=((JM/JV-1)/NP+1)*JV

  !$omp parallel do num_threads(NTHMAX)
  DO IJ=1,IM*JI
     G(IJ)=0
  END DO
  !$omp end parallel do  

  NB=JI*2*(MM/NP+1)
  NBF=NB*NP  

  IF(JI*IP.LT.JM) THEN
     JP1=1+JI*IP
     JP2=MIN(JM,JI*(IP+1))
  ELSE ! ���Υץ����Ǥ��������ٱߤ�ô�����ʤ����
     JP1=0
     JP2=-1
  END IF

  N1=(MM+1)/NP
  M1=N1*NP
  MN=N1
  IF(MOD(N1,2).EQ.0) THEN
     IF(IP.LE.MM-M1) THEN
        MN=MN+1
     END IF
  ELSE
     IF(IP.GE.NP-MM+M1-1) THEN
        MN=MN+1
     END IF
  END IF

  IF(MN.GE.1) THEN

     NTH=MIN(MN,NTHMAX)
     ITH=0
     ALLOCATE(WKP(0:NTH-1))
     ALLOCATE(WSP(0:NTH-1))
     ALLOCATE(WGP(0:NTH-1))    
     ALLOCATE(LP(0:NTH-1))
     LP=.FALSE.

     !$omp parallel private(K,NS,M,IJ,IE,WK,WS,WG,JS,JE,J,IPDEST,ITH) num_threads(NTH)
     !$omp do schedule(dynamic)
     DO K=0,MN-1
        !$ ITH=omp_get_thread_num()
        IF(.NOT.LP(ITH)) THEN
           CALL MXALLC(WKP(ITH),JV*7*JR)
           CALL C_F_POINTER(WKP(ITH),WK,[JV*7*JR])
           CALL MXALLC(WSP(ITH),2*(NM+1))
           CALL C_F_POINTER(WSP(ITH),WS,[2*(NM+1)])
           CALL MXALLC(WGP(ITH),JM*2)
           CALL C_F_POINTER(WGP(ITH),WG,[JM*2])
           LP(ITH)=.TRUE.
        END IF

        IE=5*K*(2*NM-NP*(K-1))/4+K+1
        IJ=K*(2*NM-NP*(K-1))/16+K+1     
        M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
        NS=K*(2*(NN+1)-(K-1)*NP)+1
        IF(M.EQ.0) THEN
           CALL LXSSZG(NM,NN,JM,JV,JR,S,WG,P,R,WK,WS,IPOW,0_8)        
        ELSE
           CALL LXSSWG(NM,NN,JM,JV,JR,M,S(NS),WG,P,P(1,6+K*2), &
                &        R(IE),JC(IJ),WK,WS,IPOW,0_8)
        END IF
        JS=1
        IPDEST=-1
        DO WHILE(JS.LE.2*JM) 
           IPDEST=IPDEST+1
           JE=MIN(2*JM,2*JI*(IPDEST+1))
           DO J=JS,JE
              W(NBF+(IPDEST*(MM/NP+1)+K)*JI*2+J-JS+1)=WG(J)
           END DO
           JS=JE+1
        END DO
     END DO
     !$omp end do
     !$omp end parallel

     DO ITH=0,NTH-1
        IF(LP(ITH)) THEN
           CALL MXFREE(WKP(ITH))
           CALL MXFREE(WSP(ITH))
           CALL MXFREE(WGP(ITH))                
        END IF
     END DO

     DEALLOCATE(WSP)  
     DEALLOCATE(WKP)
     DEALLOCATE(WGP)  
     DEALLOCATE(LP)

  END IF

  CALL MPI_ALLTOALL(W(NBF+1),NB,MPI_REAL8,W,NB,MPI_REAL8,ICOM4,IERR)
  
  IF(JP1.NE.0) THEN  

     NTH=MIN((JP2-JP1+1)/JV,NTHMAX)
     ALLOCATE(WKP(0:NTH-1))
     ALLOCATE(LP(0:NTH-1))
     LP=.FALSE.

     ITH=0

     !$omp parallel private(JD,M,K,I,IPSRC,IV,ITH,WK) num_threads(NTH)
     !$omp do schedule(dynamic)
     DO JD=1,(JP2-JP1+1)/JV
        !$ ITH=omp_get_thread_num()
        IF(.NOT.LP(ITH)) THEN
           CALL MXALLC(WKP(ITH),JV*IM)
           CALL C_F_POINTER(WKP(ITH),WK,[JV*IM])
           LP(ITH)=.TRUE.
        END IF
        DO M=0,MM
           K=M/NP
           IF(MOD(K,2).EQ.0) THEN
              IPSRC=M-K*NP
           ELSE
              IPSRC=(K+1)*NP-M-1
           END IF
           DO IV=1,JV*2
              WK(IV+(JV*2)*M) &
                   &  =W(IV+2*JV*(JD-1)+JI*2*(K+(MM/NP+1)*IPSRC))
           END DO
        END DO
        DO M=MM+1,IM/2-1
           DO IV=1,JV*2
              WK(IV+(JV*2)*M)=0
           END DO
        END DO
        CALL FXRTBA(JV,IM,WK,IT,T)        
        DO IV=1,JV
           DO I=1,IM
              G(I+IM*(IV-1+JV*(JD-1)))=WK(IV+JV*(I-1))
           END DO
        END DO
     END DO
     !$omp end do
     !$omp end parallel

     DO ITH=0,NTH-1
        IF(LP(ITH)) THEN
           CALL MXFREE(WKP(ITH))
        END IF
     END DO
     DEALLOCATE(WKP)
     DEALLOCATE(LP)

  END IF

END SUBROUTINE SYTS2G
