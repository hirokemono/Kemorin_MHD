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
SUBROUTINE LXXGZS(JB,AC,SD,Q,IL,ILEV)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JV=256
  INTEGER(8) :: J,JB,IR,ILEV,IL
  INTEGER(8) :: K  
  REAL(8) :: AC(*),SD(*),Q(JV,0:4,JB)
  REAL(8) :: SDV(JV,10)  

  IF(ILEV.EQ.0) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)
           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)
           SDV(J,6)=SDV(J,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,8)=SDV(J,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,5)=SDV(J,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,7)=SDV(J,7)+Q(J,4,IR)*Q(J,2,IR)
           Q(J,1,IR)=Q(J,1,IR)+(AC(IL+4)*Q(J,0,IR)+AC(IL+5))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+6)*Q(J,0,IR)+AC(IL+7))*Q(J,1,IR)
        END DO
     END DO
     DO K=1,8
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.9) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)

           SDV(J,6)=SDV(J,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,8)=SDV(J,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,5)=SDV(J,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,7)=SDV(J,7)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL+4)*Q(J,0,IR)+AC(IL+5))*Q(J,2,IR)
           SDV(J,10)=SDV(J,10)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,9)=SDV(J,9)+Q(J,4,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,10
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.8) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)

           SDV(J,6)=SDV(J,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,8)=SDV(J,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,5)=SDV(J,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,7)=SDV(J,7)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL+4)*Q(J,0,IR)+AC(IL+5))*Q(J,2,IR)
           SDV(J,9)=SDV(J,9)+Q(J,4,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,9
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.7) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)

           SDV(J,6)=SDV(J,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,8)=SDV(J,8)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,5)=SDV(J,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,7)=SDV(J,7)+Q(J,4,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,8
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.6) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)
           Q(J,2,IR)=Q(J,2,IR)+(AC(IL+2)*Q(J,0,IR)+AC(IL+3))*Q(J,1,IR)

           SDV(J,6)=SDV(J,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,5)=SDV(J,5)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,7)=SDV(J,7)+Q(J,4,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,7
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.5) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)

           SDV(J,6)=SDV(J,6)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,5)=SDV(J,5)+Q(J,4,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,6
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.4) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)

           Q(J,1,IR)=Q(J,1,IR)+(AC(IL)*Q(J,0,IR)+AC(IL+1))*Q(J,2,IR)

           SDV(J,5)=SDV(J,5)+Q(J,4,IR)*Q(J,1,IR)
        END DO
     END DO
     DO K=1,5
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.3) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,4)=SDV(J,4)+Q(J,3,IR)*Q(J,2,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,4
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  ELSE IF(ILEV.EQ.2) THEN
     SDV=0     
     DO IR=1,JB
        DO J=1,JV        
           SDV(J,2)=SDV(J,2)+Q(J,3,IR)*Q(J,1,IR)
           SDV(J,1)=SDV(J,1)+Q(J,4,IR)*Q(J,1,IR)
           SDV(J,3)=SDV(J,3)+Q(J,4,IR)*Q(J,2,IR)
        END DO
     END DO
     DO K=1,3
        DO J=1,JV
           SD(K)=SD(K)+SDV(J,K)
        END DO
     END DO
  END IF
  
END SUBROUTINE LXXGZS
