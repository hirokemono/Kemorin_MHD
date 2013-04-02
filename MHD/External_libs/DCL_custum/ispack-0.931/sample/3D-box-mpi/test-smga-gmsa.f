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
      PARAMETER(NM=10,MM=10,LM=10)      
      PARAMETER(KM=32,JM=32,IM=32)
      DIMENSION S(-NM:NM,-MM:MM,-LM:LM)
      DIMENSION G(0:KM-1,0:JM-1,0:IM-1)
      DIMENSION W(KM*JM*IM)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      CALL MPI_INIT(IERR)      

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

      DO N=-NM,NM
        DO M=-MM,MM
          DO L=-LM,LM
            S(N,M,L)=L+1D-3*(M+1D-3*N)
          END DO
        END DO
      END DO

      CALL P3S2GA(NM,MM,LM,KM,JM,IM,S,G,W,ITK,TK,ITJ,TJ,ITI,TI)
      CALL P3GMSA(NM,MM,LM,KM,JM,IM,G,S,W,ITK,TK,ITJ,TJ,ITI,TI)
      CALL P3SMGA(NM,MM,LM,KM,JM,IM,S,G,W,ITK,TK,ITJ,TJ,ITI,TI)
      CALL P3G2SA(NM,MM,LM,KM,JM,IM,G,S,W,ITK,TK,ITJ,TJ,ITI,TI)

      EPS=1D-13

      DO N=-NM,NM
        DO M=-MM,MM
          DO L=-LM,LM
            IF(ABS(S(N,M,L)-(L+1D-3*(M+1D-3*N))).GT.EPS) THEN
              WRITE(6,*) '*ERROR* ',N,M,L,
     &          ABS(S(N,M,L)-(L+1D-3*(M+1D-3*N))),
     &          S(N,M,L),L+1D-3*(M+1D-3*N)
            END IF
          END DO
        END DO
      END DO

      CALL MPI_FINALIZE(IERR)                  

      END
