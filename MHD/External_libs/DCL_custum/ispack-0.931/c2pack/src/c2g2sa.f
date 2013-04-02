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
*     TRANSFORM GRID TO SPECTRA (including everything)        2000/09/29
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
      SUBROUTINE C2G2SA(LM,KM,JM,IM,G,S,W,ITJ,TJ,ITI,TI,ISW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:JM,0:IM-1)
*      DIMENSION S(-KM:KM,0:LM)
      DIMENSION S(-KM:KM,0:*)
      DIMENSION W(-KM:KM,0:JM)            
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      CALL FTTRUF(JM+1,IM,G,W,ITI,TI)

      DO K=1,KM
        DO J=0,JM
          W( K,J)=G(J,2*K  )
          W(-K,J)=G(J,2*K+1)
        END DO
      END DO
      DO J=0,JM
        W(0,J)=G(J,0)
      END DO

      IF(ISW.EQ.1) THEN
        CALL FTTSTF(2*KM+1,JM,W(-KM,1),G,ITJ,TJ)
      ELSE IF(ISW.EQ.2) THEN
        CALL FTTCTF(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.3) THEN
        CALL FTTSMF(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.4) THEN
        CALL FTTCMF(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE
        CALL BSDMSG('E','ISW IS INVALID.')
      END IF

      IF(ISW.EQ.1) THEN
        CALL BSCOPY((2*KM+1)*LM,W(-KM,1),S)
      ELSE IF(ISW.EQ.3) THEN
        CALL BSCOPY((2*KM+1)*LM,W,S)
      ELSE
        DO K=-KM,KM
          S(K,0)=0.5D0*W(K,0)
        END DO
        CALL BSCOPY((2*KM+1)*LM,W(-KM,1),S(-KM,1))
      END IF

      END
************************************************************************
*     TRANSPOSE FOR C2G2SA
*-----------------------------------------------------------------------
      SUBROUTINE C2G2ST(JM,IM,G,W)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(0:JM,0:IM-1)      
      DIMENSION G(0:IM-1,0:JM)

      DO J=0,JM
        DO I=0,IM-1
          W(J,I)=G(I,J)
        END DO
      END DO

      END
