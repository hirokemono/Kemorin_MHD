!***********************************************************************
! ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
! Copyright (C) 1998--2024 Keiichi Ishioka <ishioka@gfd-dennou.org>
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
! 02110-1301 USA.
!***********************************************************************
SUBROUTINE LXXGWS(JB,AC,SD,Q,IL,ILEV)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JV=256
  INTEGER(8) :: J,JB,IR,ILEV,IL
  INTEGER(8) :: I,K
  REAL(8) :: AC(*),SD(2,*),Q(JV,0:6,JB)
  REAL(8) :: SDV(JV,2,10)

  IF(ILEV.EQ.0) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)
           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)
           SDV(J,1,6)=SDV(J,1,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,6)=SDV(J,2,6)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,5)=SDV(J,1,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,5)=SDV(J,2,5)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,8)=SDV(J,1,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,8)=SDV(J,2,8)+Q(J,5,IR)*Q(J,2,IR)    
           SDV(J,1,7)=SDV(J,1,7)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,7)=SDV(J,2,7)+Q(J,6,IR)*Q(J,2,IR)
           Q(J,1,IR)=Q(J,1,IR)+(AC(IL+4)*Q(J,0,IR)+AC(IL+5))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+6)*Q(J,0,IR)+AC(IL+7))*Q(J,1,IR)
        END DO
     END DO
     DO K=1,8
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.9) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)

           SDV(J,1,6)=SDV(J,1,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,6)=SDV(J,2,6)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,5)=SDV(J,1,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,5)=SDV(J,2,5)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,8)=SDV(J,1,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,8)=SDV(J,2,8)+Q(J,5,IR)*Q(J,2,IR)    
           SDV(J,1,7)=SDV(J,1,7)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,7)=SDV(J,2,7)+Q(J,6,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL+4)*Q(J,0,IR)+AC(IL+5))*Q(J,2,IR)
           SDV(J,1,10)=SDV(J,1,10)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,10)=SDV(J,2,10)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,9)=SDV(J,1,9)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,9)=SDV(J,2,9)+Q(J,6,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,10
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.8) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)
           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)
           SDV(J,1,6)=SDV(J,1,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,6)=SDV(J,2,6)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,5)=SDV(J,1,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,5)=SDV(J,2,5)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,8)=SDV(J,1,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,8)=SDV(J,2,8)+Q(J,5,IR)*Q(J,2,IR)    
           SDV(J,1,7)=SDV(J,1,7)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,7)=SDV(J,2,7)+Q(J,6,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL+4)*Q(J,0,IR)+AC(IL+5))*Q(J,2,IR)
           SDV(J,1,9)=SDV(J,1,9)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,9)=SDV(J,2,9)+Q(J,6,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,9
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.7) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)
           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)
           SDV(J,1,6)=SDV(J,1,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,6)=SDV(J,2,6)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,5)=SDV(J,1,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,5)=SDV(J,2,5)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,8)=SDV(J,1,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,8)=SDV(J,2,8)+Q(J,5,IR)*Q(J,2,IR)    
           SDV(J,1,7)=SDV(J,1,7)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,7)=SDV(J,2,7)+Q(J,6,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,8
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.6) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)
           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)
           SDV(J,1,6)=SDV(J,1,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,6)=SDV(J,2,6)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,5)=SDV(J,1,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,5)=SDV(J,2,5)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,7)=SDV(J,1,7)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,7)=SDV(J,2,7)+Q(J,6,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,7
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.5) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           SDV(J,1,6)=SDV(J,1,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,6)=SDV(J,2,6)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,5)=SDV(J,1,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,5)=SDV(J,2,5)+Q(J,6,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,6
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.4) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           SDV(J,1,5)=SDV(J,1,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,5)=SDV(J,2,5)+Q(J,6,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,5
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.3) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,4)=SDV(J,1,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,2,4)=SDV(J,2,4)+Q(J,5,IR)*Q(J,2,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,4
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  ELSE IF(ILEV.EQ.2) THEN
     SDV=0
     DO IR=1,JB
        DO J=1,JV
           SDV(J,1,2)=SDV(J,1,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,2,2)=SDV(J,2,2)+Q(J,5,IR)*Q(J,1,IR)
           SDV(J,1,1)=SDV(J,1,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,2,1)=SDV(J,2,1)+Q(J,6,IR)*Q(J,1,IR)
           SDV(J,1,3)=SDV(J,1,3)+Q(J,4,IR)*Q(J,2,IR)
           SDV(J,2,3)=SDV(J,2,3)+Q(J,6,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,3
        DO I=1,2
           DO J=1,JV
              SD(I,K)=SD(I,K)+SDV(J,I,K)
           END DO
        END DO
     END DO
  END IF

END SUBROUTINE LXXGWS
