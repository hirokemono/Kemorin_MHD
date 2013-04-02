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
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'
      PARAMETER(KM=32,JM=32,IM=32)
      PARAMETER(NM=10,MM=10,LM=10)
      PARAMETER(NPROC=1)
      PARAMETER(ISIZE1=KM*IM*((JM-1)/NPROC+1))
      DIMENSION SG(0:ISIZE1-1),W(ISIZE1)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      CALL MPI_INIT(IERR)
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

      LT=2*LC-1+LS      
      
      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

      IF(LC.GT.0) THEN
        DO L=MAX(1,LS),LE
          DO M=-MM,MM
            DO N=-NM,NM
              SG(N+NM+(2*NM+1)*((M+MM)+(2*MM+1)*(L-LS)))
     &          =L+1D-3*(M+1D-3*N)
              SG(N+NM+(2*NM+1)*((M+MM)+(2*MM+1)*(LT-L)))
     &          =-L+1D-3*(M+1D-3*N)
            END DO
          END DO
        END DO      
        IF(LS.EQ.0) THEN
          DO N=-NM,NM
            DO M=-MM,MM
              L=0
              SG(N+NM+(2*NM+1)*(M+MM))=1D-3*(M+1D-3*N)
            END DO
          END DO
        END IF
      END IF

      CALL P3SMGB(NM,MM,LM,KM,JM,IM,SG,W,ITK,TK,ITJ,TJ,ITI,TI)
      CALL P3GMSB(NM,MM,LM,KM,JM,IM,SG,W,ITK,TK,ITJ,TJ,ITI,TI)

      EPS=1D-13

      IF(LC.GT.0) THEN
        DO L=MAX(1,LS),LE
          DO N=-NM,NM
            DO M=-MM,MM
              SGTMP=SG(N+NM+(2*NM+1)*((M+MM)+(2*MM+1)*(L-LS)))
              SGINI=L+1D-3*(M+1D-3*N)
              IF(ABS(SGTMP-SGINI).GT.EPS) THEN
                WRITE(6,*) '*ERROR* ',N,M,L,
     &            ABS(SGTMP-SGINI),SGTMP,SGINI
              END IF
              SGTMP=SG(N+NM+(2*NM+1)*((M+MM)+(2*MM+1)*(LT-L)))
              SGINI=-L+1D-3*(M+1D-3*N)
              IF(ABS(SGTMP-SGINI).GT.EPS) THEN
                WRITE(6,*) '*ERROR* ',N,M,L,
     &            ABS(SGTMP-SGINI),SGTMP,SGINI
              END IF
            END DO
          END DO
        END DO      
        IF(LS.EQ.0) THEN
          DO N=-NM,NM
            DO M=-MM,MM
              L=0
              SGTMP=SG(N+NM+(2*NM+1)*((M+MM)))
              SGINI=1D-3*(M+1D-3*N)
              IF(ABS(SGTMP-SGINI).GT.EPS) THEN
                WRITE(6,*) '*ERROR* ',N,M,L,
     &            ABS(SGTMP-SGINI),SGTMP,SGINI
              END IF
            END DO
          END DO
        END IF
      END IF

      CALL MPI_FINALIZE(IERR)                  

      END
