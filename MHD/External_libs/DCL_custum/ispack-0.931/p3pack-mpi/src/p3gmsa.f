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
*     TRANSFORM GRID TO SPECTRA                               2002/05/08
************************************************************************
      SUBROUTINE P3GMSA(NM,MM,LM,KM,JM,IM,G,S,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'
      DIMENSION G(0:*),S(0:*),W(0:*)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      PARAMETER(MP=1024)
      DIMENSION ISC(0:MP-1),ISP(0:MP-1),IRC(0:MP-1),IRP(0:MP-1)

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.GT.MP) THEN
        CALL BSDMSG('E','P3GMSA','Please increase MP!')
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
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        IF(LED.GE.LSD) THEN
          LCD=LED-LSD+1
          ISC(IPD)=KM*JC*LCD
          ISP(IPD)=KM*JC*LSD
        ELSE
          LCD=0
          ISC(IPD)=0
          ISP(IPD)=0
        END IF
      END DO

      DO IPD=0,NP-1
        JSD=JP*IPD
        JED=MIN(JP*(IPD+1)-1,JM-1)
        IF(JED.GE.JSD) THEN
          JCD=JED-JSD+1
          IRC(IPD)=KM*LC*JCD
          IRP(IPD)=KM*LC*JSD
        ELSE
          JCD=0
          IRC(IPD)=0
          IRP(IPD)=0
        END IF
      END DO

      IF(JC.GT.0) THEN
        CALL P3GMS4(KM,JM,IM,JS,JE,G,W)
        CALL P3GMS3(LM,KM,IM,JC,NP,W,G,ITI,TI)
      END IF

      IF(ISC(IP).NE.0) THEN
        CALL BSCOPY(ISC(IP),G(ISP(IP)),W(IRP(IP)))
        CALL BSCOPY(ISC(IP),G(KM*JC*(LM+1)+ISP(IP)),W(KM*JM*LC+IRP(IP)))
      END IF

      ISC(IP)=0
      IRC(IP)=0
      CALL MPI_ALLTOALLV(G,ISC,ISP,MPI_REAL8,
     &  W,IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLTOALLV(G(KM*JC*(LM+1)),ISC,ISP,MPI_REAL8,
     &  W(KM*JM*LC),IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)

      IF(LC.GT.0) THEN
        CALL P3GMS2(MM,KM,JM,LC,W,G,ITJ,TJ)        
        CALL P3GMS1(NM,MM,KM,LS,LC,G,W,ITK,TK)
      END IF

      DO IPD=0,NP-1
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        IF(LED.GE.LSD) THEN
          LCD=LED-LSD+1
          IRC(IPD)=(2*MM+1)*(2*NM+1)*LCD
          IRP(IPD)=(2*MM+1)*(2*NM+1)*(LSD+LM)
        ELSE
          LCD=0
          IRC(IPD)=0
          IRP(IPD)=0
        END IF
      END DO

      CALL MPI_ALLGATHERV(W,(2*MM+1)*(2*NM+1)*LC,MPI_REAL8,
     &  S,IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)

      DO IPD=0,NP-1
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        IF(LED.GE.LSD) THEN
          LCD=LED-LSD+1
          IRC(IPD)=(2*MM+1)*(2*NM+1)*LCD
          IRP(IPD)=(2*MM+1)*(2*NM+1)*(LM-LED)
        ELSE
          LCD=0
          IRC(IPD)=0
          IRP(IPD)=0
        END IF
      END DO
      IRC(0)=IRC(0)-(2*MM+1)*(2*NM+1)

      IS=(2*MM+1)*(2*NM+1)*LC

      IF(LS.EQ.0.AND.LC.GT.0) THEN
        IC=(2*MM+1)*(2*NM+1)*(LC-1)
      ELSE
        IC=(2*MM+1)*(2*NM+1)*LC
      END IF
      
      CALL MPI_ALLGATHERV(W(IS),IC,MPI_REAL8,
     &  S,IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)
      
      END
************************************************************************
      SUBROUTINE P3GMS4(KM,JM,IM,JS,JE,G,W)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:KM-1,0:JM-1,0:IM-1)      
      DIMENSION W(0:KM-1,0:IM-1,JS:JE)      

      DO I=0,IM-1
        DO J=JS,JE
          DO K=0,KM-1
            W(K,I,J)=G(K,J,I)
          END DO
        END DO
      END DO
      
      END
************************************************************************
*     TRANSFORM GRID TO SPECTRA (分散配置用)                  2002/05/06
************************************************************************
*     GSは入力および出力である.
*
*     入力では, G(0:KM-1,0:IM-1,JS:JE) が格納されているものとして扱う.
*      
*     出力では本来の S(*,*, LS),...,S(*,*, LE),     
*                    S(*,*,-LE),...,S(*,*,-LS)
*     が順に格納される. 但し, LS=0 の場合は      
*     最後の S(*,*,-LS) に対応する部分には代入は行われない.
*      
*     GS および W に必要な領域は
*       KM * MAX( IM*((JM-1)/NP+1), JM*2*(LM/NP+1) )
*     以上である.      
*           
************************************************************************
      SUBROUTINE P3GMSB(NM,MM,LM,KM,JM,IM,GS,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION GS(0:*),W(0:*)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      PARAMETER(MP=1024)
      DIMENSION ISC(0:MP-1),ISP(0:MP-1),IRC(0:MP-1),IRP(0:MP-1)

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.GT.MP) THEN
        CALL BSDMSG('E','P3GMSB','Please increase MP!')
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
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        IF(LED.GE.LSD) THEN
          LCD=LED-LSD+1
          ISC(IPD)=KM*JC*LCD
          ISP(IPD)=KM*JC*LSD
        ELSE
          LCD=0
          ISC(IPD)=0
          ISP(IPD)=0
        END IF
      END DO

      DO IPD=0,NP-1
        JSD=JP*IPD
        JED=MIN(JP*(IPD+1)-1,JM-1)
        IF(JED.GE.JSD) THEN
          JCD=JED-JSD+1
          IRC(IPD)=KM*LC*JCD
          IRP(IPD)=KM*LC*JSD
        ELSE
          JCD=0
          IRC(IPD)=0
          IRP(IPD)=0
        END IF
      END DO

      IF(JC.GT.0) THEN
        CALL P3GMS3(LM,KM,IM,JC,NP,GS,W,ITI,TI)
      END IF

      IF(ISC(IP).NE.0) THEN
        CALL BSCOPY(ISC(IP),W(ISP(IP)),GS(IRP(IP)))
        CALL BSCOPY(ISC(IP),W(KM*JC*(LM+1)+ISP(IP)),
     &    GS(KM*JM*LC+IRP(IP)))
      END IF

      ISC(IP)=0
      IRC(IP)=0
      CALL MPI_ALLTOALLV(W,ISC,ISP,MPI_REAL8,
     &  GS,IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLTOALLV(W(KM*JC*(LM+1)),ISC,ISP,MPI_REAL8,
     &  GS(KM*JM*LC),IRC,IRP,MPI_REAL8,MPI_COMM_WORLD,IERR)

      IF(LC.GT.0) THEN
        CALL P3GMS2(MM,KM,JM,LC,GS,W,ITJ,TJ)        
        CALL P3GMS1(NM,MM,KM,LS,LC,W,GS,ITK,TK)
      END IF

      END
************************************************************************
      SUBROUTINE P3GMS1(NM,MM,KM,LS,LC,G,S,ITK,TK)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(-MM:MM,0:KM-1,2,0:LC-1)      
      DIMENSION S(-NM:NM,-MM:MM,0:*)
      DIMENSION ITK(5),TK(KM*2)

      LC2=2*LC-1
      IF(LS.EQ.0) THEN
        L1=1
      ELSE
        L1=0
      END IF

      DO L=0,LC-1
        CALL FTTZUF(2*MM+1,KM,G(-MM,0,1,L),S,ITK,TK)
      END DO

      DO L=L1,LC-1
        DO N=1,NM
          DO M=-MM,MM                  
            S( N, M,    L)=G(M,   N,1,L)
            S(-N, M,    L)=G(M,KM-N,1,L)
          END DO
          DO M=-MM,MM                  
            S(-N,-M,LC2-L)=G(M,   N,2,L)
            S( N,-M,LC2-L)=G(M,KM-N,2,L)
          END DO
        END DO
      END DO
* N=0
      DO L=L1,LC-1
        DO M=-MM,MM
          S(0, M,    L)=G(M,0,1,L)
        END DO
        DO M=-MM,MM                  
          S(0,-M,LC2-L)=G(M,0,2,L)
        END DO
      END DO

      IF(LS.EQ.0) THEN
* L=0
        DO N=1,NM
          DO M=1,MM        
            S( N, M,0)=G( M,   N,1,0)
            S(-N, M,0)=G( M,KM-N,1,0)
            S(-N,-M,0)=G( M,   N,2,0)
            S( N,-M,0)=G( M,KM-N,2,0)
          END DO
        END DO
* L=M=0
        DO N=1,NM
          S( N,0,0)=G(0,N,1,0)
          S(-N,0,0)=G(0,N,2,0)
        END DO
* L=N=0
        DO M=1,MM        
          S(0, M,0)=G(M,0,1,0)
          S(0,-M,0)=G(M,0,2,0)
        END DO
* L=M=N=0
        S(0,0,0)=G(0,0,1,0)
      END IF

      END
************************************************************************
      SUBROUTINE P3GMS2(MM,KM,JM,LC,S,G,ITJ,TJ)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(0:KM-1,0:LC-1,0:JM-1,2)      
      DIMENSION G(-MM:MM,0:KM-1,2,0:LC-1)
      DIMENSION ITJ(5),TJ(JM*2)

      CALL FTTZUF(KM*LC,JM,S,G,ITJ,TJ)      

      DO L=0,LC-1
        DO M=1,MM
          DO K=0,KM-1
            G( M,K,1,L)=S(K,L,   M,1)
            G(-M,K,1,L)=S(K,L,JM-M,1)
            G( M,K,2,L)=S(K,L,   M,2)
            G(-M,K,2,L)=S(K,L,JM-M,2)
          END DO
        END DO
        DO K=0,KM-1
          G(0,K,1,L)=S(K,L,0,1)
          G(0,K,2,L)=S(K,L,0,2)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE P3GMS3(LM,KM,IM,JC,NP,S,G,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:KM-1,0:JC*(LM+1)-1,2)
      DIMENSION S(0:KM-1,2,0:IM/2-1,0:JC-1)
      DIMENSION ITI(5),TI(IM*2)

      DO J=0,JC-1
        CALL FTTRUF(KM,IM,S(0,1,0,J),G,ITI,TI)
      END DO

      LP=LM/NP+1

      DO IPD=0,NP-1
        LSD=LP*IPD
        LED=MIN(LP*(IPD+1)-1,LM)
        LCD=LED-LSD+1
        DO L=MAX(LSD,1),LED
          DO J=0,JC-1            
            DO K=0,KM-1
              G(K,LSD*JC+LCD*J+L-LSD,1)=S(K,1,L,J)
              G(K,LSD*JC+LCD*J+L-LSD,2)=S(K,2,L,J)
            END DO
          END DO
        END DO
      END DO
* L=0
      DO J=0,JC-1        
        DO K=0,KM-1
          G(K,LP*J,1)=S(K,1,0,J)
          G(K,LP*J,2)=0
        END DO
      END DO

      END
