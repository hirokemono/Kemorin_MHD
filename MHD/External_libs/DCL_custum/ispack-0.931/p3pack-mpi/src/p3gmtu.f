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
*     CALCULATE VELOCITY VECTOR                               2002/04/25
************************************************************************
      SUBROUTINE P3GMTU(NM,MM,LM,Z,U,ISW)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'
      DIMENSION Z(-NM:NM,-MM:MM,2,0:*)
      DIMENSION U(-NM:NM,-MM:MM,0:*)

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

      IF(LC.EQ.0) THEN
        RETURN
      END IF

      IF(ISW.EQ.1) THEN

        DO L=MAX(LS,1),LE
          DO M=-MM,MM
            DO N=-NM,NM
              U(-N,-M,L-LS)
     &          =(M*Z(N,M,2,LT-L)-N*Z(N,M,1,LT-L))/(L*L+M*M+N*N)
              U(-N,-M,LT-L)
     &          =(M*Z(N,M,2,L-LS)-N*Z(N,M,1,L-LS))/(L*L+M*M+N*N)              
            END DO
          END DO
        END DO

        IF(LS.NE.0) THEN
          RETURN
        END IF
        
        L=0
        DO M=-MM,-1
          DO N=-NM,NM
            U(-N,-M,0)=Z(N,M,1,0)/M
          END DO
        END DO
        DO M=1,MM
          DO N=-NM,NM
            U(-N,-M,0)=Z(N,M,1,0)/M
          END DO
        END DO
      
        L=0
        M=0
        DO N=-NM,-1
          U(-N,-M,0)=-Z(N,M,2,0)/N
        END DO
        DO N=1,NM
          U(-N,-M,0)=-Z(N,M,2,0)/N
        END DO

        L=0
        M=0
        N=0
        U(-N,-M,0)=0

      ELSE IF(ISW.EQ.2) THEN

        DO L=MAX(LS,1),LE
          DO M=-MM,MM
            DO N=-NM,NM
              U(-N,-M,LT-L)=-(N*M*Z(N,M,1,L-LS)+(L*L+N*N)*Z(N,M,2,L-LS))
     &          /((L*L+M*M+N*N)*L)
              U(-N,-M,L-LS)=-(N*M*Z(N,M,1,LT-L)+(L*L+N*N)*Z(N,M,2,LT-L))
     &          /((L*L+M*M+N*N)*(-L))
            END DO
          END DO
        END DO

        IF(LS.NE.0) THEN
          RETURN
        END IF

        L=0
        DO M=-MM,-1
          DO N=-NM,NM
            U(-N,-M,0)=N*Z(N,M,2,0)/(M*M+N*N)
          END DO
        END DO
        DO M=1,MM
          DO N=-NM,NM
            U(-N,-M,0)=N*Z(N,M,2,0)/(M*M+N*N)          
          END DO
        END DO

        L=0
        M=0
        DO N=-NM,-1
          U(-N,-M,0)=Z(N,M,1,0)/N
        END DO
        DO N=1,NM
          U(-N,-M,0)=Z(N,M,1,0)/N
        END DO

        L=0
        M=0
        N=0
        U(-N,-M,0)=0

      ELSE IF(ISW.EQ.3) THEN

        DO L=MAX(LS,1),LE
          DO M=-MM,MM
            DO N=-NM,NM
              U(-N,-M,LT-L)=((L*L+M*M)*Z(N,M,1,L-LS)+N*M*Z(N,M,2,L-LS))
     &          /((L*L+M*M+N*N)*L)
              U(-N,-M,L-LS)=((L*L+M*M)*Z(N,M,1,LT-L)+N*M*Z(N,M,2,LT-L))
     &          /((L*L+M*M+N*N)*(-L))
            END DO
          END DO
        END DO

        IF(LS.NE.0) THEN
          RETURN
        END IF

        L=0
        DO M=-MM,-1
          DO N=-NM,NM
            U(-N,-M,0)=-M*Z(N,M,2,0)/(M*M+N*N)
          END DO
        END DO
        DO M=1,MM
          DO N=-NM,NM
            U(-N,-M,0)=-M*Z(N,M,2,0)/(M*M+N*N)          
          END DO
        END DO
      
        L=0
        M=0
        DO N=-NM,NM
          U(-N,-M,0)=0
        END DO

      END IF

      END
