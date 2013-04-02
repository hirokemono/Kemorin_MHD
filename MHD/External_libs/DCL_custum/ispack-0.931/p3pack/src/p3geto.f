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
*     CALCULATE VORTICITY VECTOR                              2002/03/14
************************************************************************
      SUBROUTINE P3GETO(NM,MM,LM,Z,O,ISW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)
      DIMENSION O(-NM:NM,-MM:MM,-LM:LM)

      IF(ISW.EQ.1) THEN
        DO L=-LM,-1
          DO M=-MM,MM
            DO N=-NM,NM
              O(N,M,L)=-(M*Z(N,M,L,1)+N*Z(N,M,L,2))/L
            END DO
          END DO
        END DO
        DO L=1,LM
          DO M=-MM,MM
            DO N=-NM,NM
              O(N,M,L)=-(M*Z(N,M,L,1)+N*Z(N,M,L,2))/L            
            END DO
          END DO
        END DO
*       L=0
        CALL BSCOPY((2*NM+1)*(2*MM+1),Z(-NM,-MM,0,2),O(-NM,-MM,0))
*       L=M=0        
        CALL BSCOPY(2*NM+1,Z(-NM,0,0,1),O(-NM,0,0))
*       L=M=N=0
        O(0,0,0)=0
      ELSE IF(ISW.EQ.2) THEN
        CALL BSCOPY((2*NM+1)*(2*MM+1)*(2*LM+1),Z,O)
        L=0
        DO M=-MM,-1
          DO N=-NM,NM
            O(N,M,L)=-(N*Z(N,M,L,1))/M
          END DO
        END DO
        DO M=1,MM
          DO N=-NM,NM
            O(N,M,L)=-(N*Z(N,M,L,1))/M          
          END DO
        END DO
*       L=M=0
        CALL BSCOPY(2*NM+1,Z(-NM,0,0,2),O(-NM,0,0))
*       L=M=N=0
        O(0,0,0)=0      
      ELSE IF(ISW.EQ.3) THEN
        CALL BSCOPY((2*NM+1)*(2*MM+1)*(2*LM+1),Z(-NM,-MM,-LM,2),O)
*       L=0
        CALL BSCOPY((2*NM+1)*(2*MM+1),Z(-NM,-MM,0,1),O(-NM,-MM,0))
*       L=M=0
        CALL BSSET0(2*NM+1,O(-NM,0,0))
      END IF

      END
