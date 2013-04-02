************************************************************************
* ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
* Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
************************************************************************
*************************************************************************
*     TRANSFORM SPECTRA TO GRID                               2002/05/09
************************************************************************
      SUBROUTINE P3SMGA(NM,MM,LM,KM,JM,IM,S,G,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION S(0:*),G(0:*),W(0:*)      
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      PARAMETER(MP=1024)
      DIMENSION ISC(0:MP-1),ISP(0:MP-1),IRC(0:MP-1),IRP(0:MP-1)

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.GT.MP) THEN
        CALL BSDMSG('E','P3SMGA','Please increase MP!')
      END IF

      LP=LM/NP+1
      LS=LP*IP
      LE=MIN(LP*(IP+1)-1,LM)
      IF(LE.GE.LS) THEN
        LC=LE-LS+1
      ELSE
        LC=0
        LS=0
        LE=0
      END IF

      JP=(JM-1)/NP+1
      JS=JP*IP
      JE=MIN(JP*(IP+1)-1,JM-1)
      IF(JE.GE.JS) THEN
        JC=JE-JS+1
      ELSE
        JC=0
        JS=0
        JE=0
      END IF

      DO IPD=0,NP-1
        JSD=JP*IPD
        JED=MIN(JP*(IPD+1)-1,JM-1)
        IF(JED.GE.JSD) THEN
          JCD=JED-JSD+1
          ISC(IPD)=KM*LC*JCD
          ISP(IPD)=KM*LC*JSD
        ELSE
          JCD=0
          ISC(IPD)=0
          ISP(IPD)=0
        END IF
      END DO
      
      DO IPD=0,NP-1
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        IF(LED.GE.LSD) THEN
          LCD=LED-LSD+1
          IRC(IPD)=KM*JC*LCD
          IRP(IPD)=KM*JC*LSD
        ELSE
          LCD=0
          IRC(IPD)=0
          IRP(IPD)=0
        END IF
      END DO

      IF(LC.GT.0) THEN
        CALL P3SMG4(NM,MM,KM,LM,LS,LC,S,W,ITK,TK)
        CALL P3SMG2(MM,KM,JM,LC,W,G,ITJ,TJ)
      END IF

      IF(ISC(IP).NE.0) THEN
        CALL BSCOPY(ISC(IP),G(ISP(IP)),W(IRP(IP)))
        CALL BSCOPY(ISC(IP),G(KM*JM*LC+ISP(IP)),W(KM*JC*(LM+1)+IRP(IP)))
      END IF

      ISC(IP)=0
      IRC(IP)=0
      CALL MPI_ALLTOALLV(G,ISC,ISP,MPI_REAL8,
     &  W,IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLTOALLV(G(KM*JM*LC),ISC,ISP,MPI_REAL8,
     &  W(KM*JC*(LM+1)),IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)

      IF(JC.GT.0) THEN
        CALL P3SMG3(LM,KM,IM,JC,NP,W,G,ITI,TI)
      END IF

      DO IPD=0,NP-1
        JSD=JP*IPD
        JED=MIN(JP*(IPD+1)-1,JM-1)
        IF(JED.GE.JSD) THEN
          JCD=JED-JSD+1
          IRC(IPD)=KM*IM*JCD
          IRP(IPD)=KM*IM*JSD
        ELSE
          JCD=0
          IRC(IPD)=0
          IRP(IPD)=0
        END IF
      END DO

      CALL MPI_ALLGATHERV(G,JC*KM*IM,MPI_REAL8,
     &  W,IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)

      CALL P3SMG5(KM,JM,IM,W,G)
      
      END
************************************************************************
      SUBROUTINE P3SMG4(NM,MM,KM,LM,LS,LC,S,G,ITK,TK)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-NM:NM,-MM:MM,-LM:LM)
      DIMENSION G(-MM:MM,0:KM-1,2,0:LC-1)
      DIMENSION ITK(5),TK(KM*2)

      LC2=2*LC-1
      IF(LS.EQ.0) THEN
        L1=1
      ELSE
        L1=0
      END IF
      
      DO L=0,LC-1
        DO N=NM+1,KM-NM-1
          DO M=-MM,MM                  
            G(M,N,1,L)=0
            G(M,N,2,L)=0
          END DO
        END DO
      END DO
      
      DO L=L1,LC-1
        DO N=1,NM
          DO M=-MM,MM                  
            G(M,   N,1,L)=S( N, M, (L+LS))
            G(M,   N,2,L)=S(-N,-M,-(L+LS))
            G(M,KM-N,1,L)=S(-N, M, (L+LS))
            G(M,KM-N,2,L)=S( N,-M,-(L+LS))
          END DO
        END DO
      END DO
* N=0
      DO L=L1,LC-1
        DO M=-MM,MM
          G(M,0,1,L)=S(0, M, (L+LS))
          G(M,0,2,L)=S(0,-M,-(L+LS))
        END DO
      END DO

      IF(LS.EQ.0) THEN
* L=0
        DO N=1,NM
          DO M=1,MM        
            G( M,   N,1,0)= S( N, M, 0)
            G( M,   N,2,0)= S(-N,-M, 0)
            G( M,KM-N,1,0)= S(-N, M, 0)
            G( M,KM-N,2,0)= S( N,-M, 0)
            G(-M,   N,1,0)= S(-N, M, 0)
            G(-M,   N,2,0)=-S( N,-M, 0)
            G(-M,KM-N,1,0)= S( N, M, 0)
            G(-M,KM-N,2,0)=-S(-N,-M, 0)
          END DO
        END DO
* L=M=0
        DO N=1,NM
          G(0,   N,1,0)= S( N, 0, 0)
          G(0,   N,2,0)= S(-N, 0, 0)
        END DO
        DO N=1,NM
          G(0,KM-N,1,0)= S( N, 0, 0)
          G(0,KM-N,2,0)=-S(-N, 0, 0)
        END DO
* L=N=0
        DO M=1,MM        
          G( M,0,1,0)= S(0, M, 0)
          G( M,0,2,0)= S(0,-M, 0)
          G(-M,0,1,0)= S(0, M, 0)
          G(-M,0,2,0)=-S(0,-M, 0)
        END DO
* L=M=N=0
        G(0,0,1,0)=S(0,0,0)
        G(0,0,2,0)=0
      END IF

      DO L=0,LC-1
        CALL FTTZUB(2*MM+1,KM,G(-MM,0,1,L),S,ITK,TK)
      END DO

      END
************************************************************************
      SUBROUTINE P3SMG5(KM,JM,IM,W,G)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(0:KM-1,0:IM-1,0:JM-1)      
      DIMENSION G(0:KM-1,0:JM-1,0:IM-1)

      DO I=0,IM-1
        DO J=0,JM-1
          DO K=0,KM-1
            G(K,J,I)=W(K,I,J)
          END DO
        END DO
      END DO
      
      END
************************************************************************
*     TRANSFORM SPECTRA TO GRID (分散配置用)                  2002/05/06
************************************************************************
*     SGは入力および出力である.
*
*     入力では本来の S(*,*, LS),...,S(*,*, LE),     
*                    S(*,*,-LE),...,S(*,*,-LS)
*     が順に格納されているものとして扱う. 但し, LS=0 の場合は      
*     最後の S(*,*,-LS) に対応する部分は参照しない.
*      
*     出力には最終的に, G(0:KM-1,0:IM-1,JS:JE) が格納される.
*      
*     SG および W に必要な領域は
*       KM * MAX( IM*((JM-1)/NP+1), JM*2*(LM/NP+1) )
*     以上である.      
*           
************************************************************************
      SUBROUTINE P3SMGB(NM,MM,LM,KM,JM,IM,SG,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION SG(0:*),W(0:*)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      PARAMETER(MP=1024)
      DIMENSION ISC(0:MP-1),ISP(0:MP-1),IRC(0:MP-1),IRP(0:MP-1)

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.GT.MP) THEN
        CALL BSDMSG('E','P3SMGB','Please increase MP!')
      END IF

      LP=LM/NP+1
      LS=LP*IP
      LE=MIN(LP*(IP+1)-1,LM)
      IF(LE.GE.LS) THEN
        LC=LE-LS+1
      ELSE
        LC=0
        LS=0
        LE=0
      END IF

      JP=(JM-1)/NP+1
      JS=JP*IP
      JE=MIN(JP*(IP+1)-1,JM-1)
      IF(JE.GE.JS) THEN
        JC=JE-JS+1
      ELSE
        JC=0
        JS=0
        JE=0
      END IF

      DO IPD=0,NP-1
        JSD=JP*IPD
        JED=MIN(JP*(IPD+1)-1,JM-1)
        IF(JED.GE.JSD) THEN
          JCD=JED-JSD+1
          ISC(IPD)=KM*LC*JCD
          ISP(IPD)=KM*LC*JSD
        ELSE
          JCD=0
          ISC(IPD)=0
          ISP(IPD)=0
        END IF
      END DO
      
      DO IPD=0,NP-1
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        IF(LED.GE.LSD) THEN
          LCD=LED-LSD+1
          IRC(IPD)=KM*JC*LCD
          IRP(IPD)=KM*JC*LSD
        ELSE
          LCD=0
          IRC(IPD)=0
          IRP(IPD)=0
        END IF
      END DO

      IF(LC.GT.0) THEN
        CALL P3SMG1(NM,MM,KM,LS,LC,SG,W,ITK,TK)
        CALL P3SMG2(MM,KM,JM,LC,W,SG,ITJ,TJ)
      END IF

      IF(ISC(IP).NE.0) THEN
        CALL BSCOPY(ISC(IP),SG(ISP(IP)),W(IRP(IP)))
        CALL BSCOPY(ISC(IP),SG(KM*JM*LC+ISP(IP)),
     &    W(KM*JC*(LM+1)+IRP(IP)))
      END IF

      ISC(IP)=0
      IRC(IP)=0
      CALL MPI_ALLTOALLV(SG,ISC,ISP,MPI_REAL8,
     &  W,IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLTOALLV(SG(KM*JM*LC),ISC,ISP,MPI_REAL8,
     &  W(KM*JC*(LM+1)),IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)

      IF(JC.GT.0) THEN
        CALL P3SMG3(LM,KM,IM,JC,NP,W,SG,ITI,TI)
      END IF

      END
************************************************************************
      SUBROUTINE P3SMG1(NM,MM,KM,LS,LC,S,G,ITK,TK)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-NM:NM,-MM:MM,0:*)
      DIMENSION G(-MM:MM,0:KM-1,2,0:LC-1)
      DIMENSION ITK(5),TK(KM*2)

      LC2=2*LC-1
      IF(LS.EQ.0) THEN
        L1=1
      ELSE
        L1=0
      END IF
      
      CALL BSSET0((2*MM+1)*KM*2*LC,G)
      DO L=L1,LC-1
        DO N=1,NM
          DO M=-MM,MM                  
            G(M,   N,1,L)=S( N, M,L)
            G(M,   N,2,L)=S(-N,-M,LC2-L)
            G(M,KM-N,1,L)=S(-N, M,L)
            G(M,KM-N,2,L)=S( N,-M,LC2-L)
          END DO
        END DO
      END DO
* N=0
      DO L=L1,LC-1
        DO M=-MM,MM
          G(M,0,1,L)=S(0, M,L)
          G(M,0,2,L)=S(0,-M,LC2-L)
        END DO
      END DO

      IF(LS.EQ.0) THEN
* L=0
        DO N=1,NM
          DO M=1,MM        
            G( M,   N,1,0)= S( N, M, 0)
            G( M,   N,2,0)= S(-N,-M, 0)
            G( M,KM-N,1,0)= S(-N, M, 0)
            G( M,KM-N,2,0)= S( N,-M, 0)
            G(-M,   N,1,0)= S(-N, M, 0)
            G(-M,   N,2,0)=-S( N,-M, 0)
            G(-M,KM-N,1,0)= S( N, M, 0)
            G(-M,KM-N,2,0)=-S(-N,-M, 0)
          END DO
        END DO
* L=M=0
        DO N=1,NM
          G(0,   N,1,0)= S( N, 0, 0)
          G(0,   N,2,0)= S(-N, 0, 0)
        END DO
        DO N=1,NM
          G(0,KM-N,1,0)= S( N, 0, 0)
          G(0,KM-N,2,0)=-S(-N, 0, 0)
        END DO
* L=N=0
        DO M=1,MM        
          G( M,0,1,0)= S(0, M, 0)
          G( M,0,2,0)= S(0,-M, 0)
          G(-M,0,1,0)= S(0, M, 0)
          G(-M,0,2,0)=-S(0,-M, 0)
        END DO
* L=M=N=0
        G(0,0,1,0)=S(0,0,0)
      END IF

      DO L=0,LC-1
        CALL FTTZUB(2*MM+1,KM,G(-MM,0,1,L),S,ITK,TK)
      END DO

      END
************************************************************************
      SUBROUTINE P3SMG2(MM,KM,JM,LC,G,S,ITJ,TJ)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(-MM:MM,0:KM-1,2,0:LC-1)
      DIMENSION S(0:KM-1,0:LC-1,0:JM-1,2)
      DIMENSION ITJ(5),TJ(JM*2)

      DO J=MM+1,JM-MM-1
        CALL BSSET0(KM*LC,S(0,0,J,1))
        CALL BSSET0(KM*LC,S(0,0,J,2))
      END DO
      
      DO L=0,LC-1
        DO M=1,MM
          DO K=0,KM-1
            S(K,L,   M,1)=G( M,K,1,L)
            S(K,L,JM-M,1)=G(-M,K,1,L)              
            S(K,L,   M,2)=G( M,K,2,L)
            S(K,L,JM-M,2)=G(-M,K,2,L)
          END DO
        END DO
        DO K=0,KM-1
          S(K,L,0,1)=G(0,K,1,L)
          S(K,L,0,2)=G(0,K,2,L)              
        END DO
      END DO
      CALL FTTZUB(KM*LC,JM,S,G,ITJ,TJ)

      END
************************************************************************
      SUBROUTINE P3SMG3(LM,KM,IM,JC,NP,G,S,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:KM-1,0:JC*(LM+1)-1,2)
      DIMENSION S(0:KM-1,2,0:IM/2-1,0:JC-1)
      DIMENSION ITI(5),TI(IM*2)

      LP=LM/NP+1

      DO J=0,JC-1
        DO L=LM+1,IM/2-1
          DO K=0,KM-1
            S(K,1,L,J)=0
            S(K,2,L,J)=0
          END DO
        END DO
      END DO

      DO IPD=0,NP-1
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        LCD=LED-LSD+1
        DO L=MAX(LSD,1),LED
          DO J=0,JC-1            
            DO K=0,KM-1
              S(K,1,L,J)=G(K,LSD*JC+LCD*J+L-LSD,1)
              S(K,2,L,J)=G(K,LSD*JC+LCD*J+L-LSD,2)
            END DO
          END DO
        END DO
      END DO
* L=0
      DO J=0,JC-1        
        DO K=0,KM-1
          S(K,1,0,J)=G(K,LP*J,1)
          S(K,2,0,J)=0
        END DO
      END DO

      DO J=0,JC-1
        CALL FTTRUB(KM,IM,S(0,1,0,J),G,ITI,TI)
      END DO

      END
