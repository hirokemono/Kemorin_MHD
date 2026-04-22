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
SUBROUTINE SYTV2S(MM,NM,NN,IM,JM,JV,S1,S2,G1,G2,IT,T,P,R,JC,W,IPOW,ICOM)

  USE ISO_C_BINDING  
  IMPLICIT NONE  
  INCLUDE 'mpif.h'
  INTEGER(8) :: MM,NM,NN,IM,JM,JV,JR,M,IE,IJ,JD,NS,IPOW,L,ICOM
  INTEGER(8) :: JI,JP1,JP2,JS,JE,K,I,IV,J,NBF
  INTEGER(8) :: IPDEST,IPSRC
  INTEGER(8) :: MN,M1,N1
  INTEGER(8) :: NTH
  REAL(8) :: S1(*) ! S1((MM/NP+1)*(2*(NN+1)-MM/NP*NP))
  REAL(8) :: S2(*) ! S2((MM/NP+1)*(2*(NN+1)-MM/NP*NP))  
  REAL(8) :: G1(*) ! G1(0:IM-1,((JM/JV-1)/NP+1)*JV)
  REAL(8) :: G2(*) ! G2(0:IM-1,((JM/JV-1)/NP+1)*JV)  
  INTEGER(8) :: IT(IM/2)  
  REAL(8) :: T(IM*3/2)
  REAL(8) :: P(JM/2,*) !  P(JM/2,5+2*(MM/NP+1))
  REAL(8) :: R(*) ! R(5*(MM/NP+1)*(2*NM-MM/NP*NP)/4+MM/NP+1)
  INTEGER(8) :: JC(*) ! JC((MM/NP+1)*(2*NM-MM/NP*NP)/16+MM/NP+1)
  REAL(8) :: W(*) ! W(2*JV*((JM/JV-1)/NP+1)*(MM/NP+1)*NP*2*2)
  TYPE(C_PTR) :: WSP
  TYPE(C_PTR) :: WKP
  TYPE(C_PTR) :: WGP
  REAL(8),POINTER:: WS(:)
  REAL(8),POINTER:: WK(:)
  REAL(8),POINTER:: WG(:,:)
  INTEGER :: IP,NP,IERR,NB,ICOM4

  CALL SYGPRM(JM,JV,JR)
  CALL MXGOMP(NTH)   

  ICOM4=ICOM
  CALL MPI_COMM_SIZE(ICOM4,NP,IERR)
  CALL MPI_COMM_RANK(ICOM4,IP,IERR)

  JI=((JM/JV-1)/NP+1)*JV

  NB=JI*2*(MM/NP+1)
  NBF=NB*NP

  !$omp parallel do num_threads(NTH)
  DO L=1,(MM/NP+1)*(2*(NN+1)-MM/NP*NP)
     S1(L)=0
     S2(L)=0     
  END DO
  !$omp end parallel do

  IF(JI*IP.LT.JM) THEN
     JP1=1+JI*IP
     JP2=MIN(JM,JI*(IP+1))
  ELSE ! そのプロセスでは全く緯度円を担当しない場合
     JP1=0
     JP2=-1
  END IF
  
  IF(JP1.NE.0) THEN  
     !$omp parallel private(JD,M,K,I,IPSRC,IV,WKP,WK) num_threads(NTH)
     CALL MXALLC(WKP,JV*IM*2)
     CALL C_F_POINTER(WKP,WK,[JV*IM*2])
     !$omp do schedule(dynamic)
     DO JD=1,(JP2-JP1+1)/JV
        DO IV=1,JV
           DO I=1,IM
              WK(IV+JV*(I-1))=G1(I+IM*(IV-1+JV*(JD-1)))
              WK(JV*IM+IV+JV*(I-1))=G2(I+IM*(IV-1+JV*(JD-1)))              
           END DO
        END DO
        CALL FXRTFA(JV,IM,WK(1),IT,T) ! お節介なgfortranへの対応
        CALL FXRTFA(JV,IM,WK(JV*IM+1),IT,T)        
        DO M=0,MM
           K=M/NP
           IF(MOD(K,2).EQ.0) THEN
              IPSRC=M-K*NP
           ELSE
              IPSRC=(K+1)*NP-M-1
           END IF
           DO IV=1,JV*2
              W(IV+2*JV*(JD-1)+JI*2*(K+(MM/NP+1)*IPSRC))=WK(IV+(JV*2)*M)
              W(NBF+IV+2*JV*(JD-1)+JI*2*(K+(MM/NP+1)*IPSRC))=WK(JV*IM+IV+(JV*2)*M)
           END DO
        END DO
     END DO
     !$omp end do
     CALL MXFREE(WKP)     
     !$omp end parallel

  END IF

  CALL MPI_ALLTOALL(W,NB,MPI_REAL8,W(NBF*2+1),NB,MPI_REAL8,ICOM4,IERR)
  CALL MPI_ALLTOALL(W(NBF+1),NB,MPI_REAL8,W(NBF*3+1),NB,MPI_REAL8,ICOM4,IERR)

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
     !$omp parallel private(K,NS,M,IJ,IE,WK,WS,WG,JS,JE,J,IPDEST,WKP,WSP,WGP) num_threads(NTH)
     CALL MXALLC(WKP,JV*11*JR)
     CALL C_F_POINTER(WKP,WK,[JV*11*JR])
     CALL MXALLC(WSP,4*(NM+1))
     CALL C_F_POINTER(WSP,WS,[4*(NM+1)])
     CALL MXALLC(WGP,JM*2*2)
     CALL C_F_POINTER(WGP,WG,[JM*2,2_8])
     !$omp do schedule(dynamic)
     DO K=0,MN-1
        JS=1
        IPDEST=-1
        DO WHILE(JS.LE.2*JM) 
           IPDEST=IPDEST+1
           JE=MIN(2*JM,2*JI*(IPDEST+1))
           DO J=JS,JE
              WG(J,1)=W(NBF*2+(IPDEST*(MM/NP+1)+K)*JI*2+J-JS+1)
              WG(J,2)=W(NBF*3+(IPDEST*(MM/NP+1)+K)*JI*2+J-JS+1)              
           END DO
           JS=JE+1
        END DO
        IE=5*K*(2*NM-NP*(K-1))/4+K+1
        IJ=K*(2*NM-NP*(K-1))/16+K+1     
        M=K*NP+IP+MOD(K,2)*(NP-2*IP-1)
        NS=K*(2*(NN+1)-(K-1)*NP)+1
        IF(M.EQ.0) THEN
           CALL LXSVZS(NM,NN,JM,JV,JR,S1,S2,WG(1,1),WG(1,2),P,R,WK,WS,IPOW,0_8)
        ELSE
           CALL LXSVWS(NM,NN,JM,JV,JR,M,S1(NS),S2(NS),WG(1,1),WG(1,2), &
                & P,P(1,6+K*2),R(IE),JC(IJ),WK,WS,IPOW,0_8)
        END IF
     END DO
     !$omp end do
     CALL MXFREE(WKP)
     CALL MXFREE(WSP)
     CALL MXFREE(WGP)                
     !$omp end parallel

  END IF

END SUBROUTINE SYTV2S
