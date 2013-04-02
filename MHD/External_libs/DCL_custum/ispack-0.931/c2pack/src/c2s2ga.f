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
*     TRANSFORM SPECTRA TO GRID (including everything)        2000/09/29
************************************************************************
*     LM°ÂJM-1, W((JM+1)*IM)
*         S(-KM:KM,0:LM-1) for ISW=1,3
*      or S(-KM:KM,0:LM)   for ISW=2,4
*      
*     ISW=1: SINE TRAPEZOIDAL
*     ISW=2: COSINE TRAPEZOIDAL
*     ISW=3: SINE MIDPOINT
*     ISW=4: COSINE MIDPOINT
*-----------------------------------------------------------------------
      SUBROUTINE C2S2GA(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI,ISW)

      IMPLICIT REAL*8(A-H,O-Z)
*      DIMENSION S(-KM:KM,0:LM)
      DIMENSION S(-KM:KM,0:*)
      DIMENSION G(0:JM,0:IM-1)
      DIMENSION W(-KM:KM,0:JM)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      IF(ISW.EQ.1) THEN
        CALL BSSET0((2*KM+1),W)
        CALL BSCOPY((2*KM+1)*LM,S,W(-KM,1))
        CALL BSSET0((2*KM+1)*(JM-LM),W(-KM,LM+1))
      ELSE IF(ISW.EQ.3) THEN
        CALL BSCOPY((2*KM+1)*LM,S,W)
        CALL BSSET0((2*KM+1)*(JM-LM+1),W(-KM,LM))
      ELSE
        DO K=-KM,KM
          W(K,0)=2*S(K,0)
        END DO
        CALL BSCOPY((2*KM+1)*LM,S(-KM,1),W(-KM,1))
        CALL BSSET0((2*KM+1)*(JM-LM),W(-KM,LM+1))
      END IF

      IF(ISW.EQ.1) THEN
        CALL FTTSTB(2*KM+1,JM,W(-KM,1),G,ITJ,TJ)
      ELSE IF(ISW.EQ.2) THEN
        CALL FTTCTB(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.3) THEN
        CALL FTTSMB(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.4) THEN
        CALL FTTCMB(2*KM+1,JM,W,G,ITJ,TJ)        
      ELSE
        CALL BSDMSG('E','ISW IS INVALID.')
      END IF

      DO J=0,JM
        G(J,0)=W(0,J)
        G(J,1)=0
      END DO
      DO K=1,KM        
        DO J=0,JM
          G(J,2*K  )=W( K,J)
          G(J,2*K+1)=W(-K,J)
        END DO
      END DO
      DO I=2*KM+2,IM-1
        DO J=0,JM
          G(J,I)=0
        END DO
      END DO

      CALL FTTRUB(JM+1,IM,G,W,ITI,TI)

      END
************************************************************************
*     TRANSPOSE FOR C2S2GA
*-----------------------------------------------------------------------      
      SUBROUTINE C2S2GT(JM,IM,W,G)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:IM-1,0:JM)
      DIMENSION W(0:JM,0:IM-1)

      DO J=0,JM
        DO I=0,IM-1
          G(I,J)=W(J,I)
        END DO
      END DO

      END
