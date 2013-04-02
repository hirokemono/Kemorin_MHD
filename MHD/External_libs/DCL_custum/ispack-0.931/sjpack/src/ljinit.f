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
*     INITIALIZATION OF LJPACK                                2010/09/22
*-----------------------------------------------------------------------      
*          MM=0 でも対応できるように修正.
*          上記の修正が変だったので再修正
*-----------------------------------------------------------------------
      SUBROUTINE LJINIT(MM,NM,JM,P,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM/2,MM+4),R((MM+1)*(2*NM-MM-1)+1)

      JH=JM/2

      CALL LJGAUS(JM,P(1,1),P(1,2))

      DO J=1,JH
        P(J,3)=SQRT(1-P(J,1)*P(J,1))
        P(J,4)=1/P(J,3)
      END DO

*/ CALCULATION OF FUNCTION FOR RECURRENCE FORMULA /*

      IF(MM.GE.1) THEN
        M=1
        A=SQRT(1D0*(2*M+1)/(2*M))      
        DO J=1,JH
          P(J,4+M)=A*P(J,3)
        END DO

        DO M=2,MM
          A=SQRT(1D0*(2*M+1)/(2*M))
          DO J=1,JH
            P(J,4+M)=A*P(J,4+M-1)*P(J,3)
          END DO
        END DO
      END IF

      IND=0
      DO M=0,MM
        N=M+1
        IF(N.LE.NM) THEN
          IND=IND+1
          R(IND)=SQRT(1D0*(2*N+1)*(2*N-1)/(1D0*(N+M)*(N-M)))
        END IF
        N=M+2
        IF(N.LE.NM) THEN
          IND=IND+1
          R(IND)=-SQRT((1D0*(2*N+1)*(N-1+M)*(N-1-M))
     &      /(1D0*(2*N-3)*(N+M)*(N-M)))
        END IF
        DO N=M+3,NM
          IND=IND+1
          A=-SQRT((1D0*(2*N+1)*(N-1+M)*(N-1-M))
     &      /(1D0*(2*N-3)*(N+M)*(N-M)))
          R(IND)=A*R(IND-2)
        END DO
        A=1        
        DO N=M+1,NM-1
          IND=IND+1
          INDD=IND-(NM-M)
          A=-A
          R(IND)=A*R(INDD)*R(INDD)
        END DO
      END DO

      END
