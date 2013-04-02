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
************************************************************************
*     CALCULATE NONLINEAR TERM FOR 3D EULER EQ.               2002/05/09
************************************************************************
      SUBROUTINE P3EMNL(NM,MM,LM,KM,JM,IM,Z,DZ,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      
      DIMENSION Z(-NM:NM,-MM:MM,2,0:*)
      DIMENSION DZ(-NM:NM,-MM:MM,2,0:*)      
      DIMENSION W(0:*)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

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

      IF(LS.EQ.0) THEN
        L2=2*LC-2
      ELSE
        L2=2*LC-1
      END IF

      LT=2*LC-1+LS

      M2=MM*2+1
      N2=NM*2+1
      IS=KM*MAX(IM*((JM-1)/NP+1),JM*2*(LM/NP+1))
      IJKM=IM*JC*KM      

* U --> W(IS*1)

      CALL P3GMTU(NM,MM,LM,Z,W(IS*1),1)
      CALL P3SMGB(NM,MM,LM,KM,JM,IM,W(IS*1),W,ITK,TK,ITJ,TJ,ITI,TI)

* V --> W(IS*2)      

      CALL P3GMTU(NM,MM,LM,Z,W(IS*2),2)
      CALL P3SMGB(NM,MM,LM,KM,JM,IM,W(IS*2),W,ITK,TK,ITJ,TJ,ITI,TI)

* UV --> W(IS*3) --> DZ(*,1)      
      
      CALL P3EMN1(IJKM,W(IS*1),W(IS*2),W(IS*3))
      CALL P3GMSB(NM,MM,LM,KM,JM,IM,W(IS*3),W,ITK,TK,ITJ,TJ,ITI,TI)
      DO L=0,L2
        CALL BSCOPY(M2*N2,W(IS*3+M2*N2*L),DZ(-MM,-NM,1,L))
      END DO
      
* V*V-U*U--> W(IS*3) --> DZ(*,2)
      
      CALL P3EMN2(IJKM,W(IS*2),W(IS*1),W(IS*3))
      CALL P3GMSB(NM,MM,LM,KM,JM,IM,W(IS*3),W,ITK,TK,ITJ,TJ,ITI,TI)
      DO L=0,L2
        CALL BSCOPY(M2*N2,W(IS*3+M2*N2*L),DZ(-MM,-NM,2,L))
      END DO
      
* W --> W(IS*3)

      CALL P3GMTU(NM,MM,LM,Z,W(IS*3),3)
      CALL P3SMGB(NM,MM,LM,KM,JM,IM,W(IS*3),W,ITK,TK,ITJ,TJ,ITI,TI)

* VW --> W(*)

      CALL P3EMN1(IJKM,W(IS*2),W(IS*3),W)
      CALL P3GMSB(NM,MM,LM,KM,JM,IM,W,W(IS*2),ITK,TK,ITJ,TJ,ITI,TI)

* COMPUTING DZ (part1)
      
      DO L=MAX(LS,1),LE
        DO M=-MM,MM
          DO N=-NM,NM
            DZ(N,M,2,L-LS)=L*M*DZ(N,M,2,L-LS)+(L*L-M*M)*DZ(N,M,1,L-LS)
     &        +N*L*W(N+NM+N2*(M+MM+M2*(L-LS)))
            DZ(N,M,1,L-LS)=M*(N*DZ(N,M,1,L-LS)
     &        -L*W(N+NM+N2*(M+MM+M2*(L-LS))))
            DZ(N,M,2,LT-L)=(-L)*M*DZ(N,M,2,LT-L)
     &        +(L*L-M*M)*DZ(N,M,1,LT-L)
     &        +N*(-L)*W(N+NM+N2*(M+MM+M2*(LT-L)))
            DZ(N,M,1,LT-L)=M*(N*DZ(N,M,1,LT-L)
     &        -(-L)*W(N+NM+N2*(M+MM+M2*(LT-L))))
          END DO
        END DO
      END DO

      IF(LS.EQ.0) THEN

        L=0
        DO M=-MM,-1
          DO N=-NM,NM
            DZ(N,M,2,0)=-M*N*DZ(N,M,2,0)
     &        +(M*M-N*N)*W(N+NM+N2*(M+MM))
            DZ(N,M,1,0)=-M*M*DZ(N,M,1,0)
          END DO
        END DO
        DO M=1,MM
          DO N=-NM,NM
            DZ(N,M,2,0)=-M*N*DZ(N,M,2,0)
     &        +(M*M-N*N)*W(N+NM+N2*(M+MM))
            DZ(N,M,1,0)=-M*M*DZ(N,M,1,0)
          END DO
        END DO
      
        L=0
        M=0
        DO N=-NM,-1
          DZ(N,M,1,0)=-N*N*W(N+NM+N2*(M+MM))
        END DO
        DO N=1,NM
          DZ(N,M,1,0)=-N*N*W(N+NM+N2*(M+MM))
        END DO

      END IF
        
* WU --> W(*)

      CALL P3EMN1(IJKM,W(IS*3),W(IS*1),W)
      CALL P3GMSB(NM,MM,LM,KM,JM,IM,W,W(IS*2),ITK,TK,ITJ,TJ,ITI,TI)
      
* COMPUTING DZ (part2)
      
      DO L=MAX(LS,1),LE
        DO M=-MM,MM
          DO N=-NM,NM
            DZ(N,M,1,L-LS)=DZ(N,M,1,L-LS)
     &        +(N*N-L*L)*W(N+NM+N2*(M+MM+M2*(L-LS)))
            DZ(N,M,2,L-LS)=DZ(N,M,2,L-LS)
     &        -N*M*W(N+NM+N2*(M+MM+M2*(L-LS)))
            DZ(N,M,1,LT-L)=DZ(N,M,1,LT-L)
     &        +(N*N-L*L)*W(N+NM+N2*(M+MM+M2*(LT-L)))
            DZ(N,M,2,LT-L)=DZ(N,M,2,LT-L)
     &        -N*M*W(N+NM+N2*(M+MM+M2*(LT-L)))
          END DO
        END DO
      END DO

      IF(LS.EQ.0) THEN

        L=0
        DO M=-MM,-1
          DO N=-NM,NM
            DZ(N,M,1,0)=DZ(N,M,1,0)-M*N*W(N+NM+N2*(M+MM))
          END DO
        END DO
        DO M=1,MM
          DO N=-NM,NM
            DZ(N,M,1,0)=DZ(N,M,1,0)-M*N*W(N+NM+N2*(M+MM))
          END DO
        END DO
        
        L=0
        M=0
        DO N=-NM,-1
          DZ(N,M,2,0)=N*N*W(N+NM+N2*(M+MM))
        END DO
        DO N=1,NM
          DZ(N,M,2,0)=N*N*W(N+NM+N2*(M+MM))
        END DO

      END IF

* U*U-W*W--> W(*)
      
      CALL P3EMN2(IJKM,W(IS*1),W(IS*3),W)      
      CALL P3GMSB(NM,MM,LM,KM,JM,IM,W,W(IS*2),ITK,TK,ITJ,TJ,ITI,TI)
      
* COMPUTING DZ (part3)
      
      DO L=MAX(LS,1),LE
        DO M=-MM,MM
          DO N=-NM,NM
            DZ(N,M,1,L-LS)=DZ(N,M,1,L-LS)
     &        +L*N*W(N+NM+N2*(M+MM+M2*(L-LS)))
            DZ(N,M,1,LT-L)=DZ(N,M,1,LT-L)
     &        +(-L)*N*W(N+NM+N2*(M+MM+M2*(LT-L)))
          END DO
        END DO
      END DO

      IF(LS.EQ.0) THEN      

        L=0
        DO M=-MM,-1
          DO N=-NM,NM
            DZ(N,M,2,0)=DZ(N,M,2,0)-M*N*W(N+NM+N2*(M+MM))
          END DO
        END DO
        DO M=1,MM
          DO N=-NM,NM
            DZ(N,M,2,0)=DZ(N,M,2,0)-M*N*W(N+NM+N2*(M+MM))
          END DO
        END DO
      
        L=0
        M=0
        N=0
        DZ(N,M,1,0)=0
        DZ(N,M,2,0)=0

      END IF
        
      END
************************************************************************
      SUBROUTINE P3EMN1(N,A,B,C)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N),C(N)

      DO I=1,N
        C(I)=A(I)*B(I)
      END DO

      END
************************************************************************
      SUBROUTINE P3EMN2(N,A,B,C)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N),C(N)

      DO I=1,N
        C(I)=A(I)*A(I)-B(I)*B(I)
      END DO

      END      
