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
SUBROUTINE LXOGWS(JB,AC,SD,Q,IL,ILEV)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JV=8
  INTEGER(8) :: JB,IR,ILEV,IL
  INTEGER(8) :: I,K
  REAL(8) :: AC(*),SD(2,*),Q(JV,0:6,JB)
  REAL(8) :: Q0V(JV),Q1V(JV),Q2V(JV),Q3V(JV),Q4V(JV),Q5V(JV),Q6V(JV)
  REAL(8) :: SDV(JV,2,10)

  IF(ILEV.EQ.0) THEN
     SDV=0
     DO IR=1,JB
        Q0V=Q(:,0,IR)
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V
        Q1V=Q1V+(AC(IL)*Q0V+AC(IL+1))*Q2V
        Q(:,1,IR)=Q1V
        Q(:,2,IR)=Q2V+(AC(IL+2)*Q0V+AC(IL+3))*Q1V
     END DO
     DO IR=1,JB
        Q0V=Q(:,0,IR)
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,6)=SDV(:,1,6)+Q3V*Q1V
        SDV(:,2,6)=SDV(:,2,6)+Q5V*Q1V
        SDV(:,1,5)=SDV(:,1,5)+Q4V*Q1V
        SDV(:,2,5)=SDV(:,2,5)+Q6V*Q1V
        SDV(:,1,8)=SDV(:,1,8)+Q3V*Q2V
        SDV(:,2,8)=SDV(:,2,8)+Q5V*Q2V    
        SDV(:,1,7)=SDV(:,1,7)+Q4V*Q2V
        SDV(:,2,7)=SDV(:,2,7)+Q6V*Q2V
        Q1V=Q1V+(AC(IL+4)*Q0V+AC(IL+5))*Q2V
        Q(:,1,IR)=Q1V
        Q(:,2,IR)=Q2V+(AC(IL+6)*Q0V+AC(IL+7))*Q1V
     END DO
     DO K=1,8
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.9) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V

        Q1V=Q1V+(AC(IL)*Q0V+AC(IL+1))*Q2V
        Q2V=Q2V+(AC(IL+2)*Q0V+AC(IL+3))*Q1V

        SDV(:,1,6)=SDV(:,1,6)+Q3V*Q1V
        SDV(:,2,6)=SDV(:,2,6)+Q5V*Q1V
        SDV(:,1,5)=SDV(:,1,5)+Q4V*Q1V
        SDV(:,2,5)=SDV(:,2,5)+Q6V*Q1V
        SDV(:,1,8)=SDV(:,1,8)+Q3V*Q2V
        SDV(:,2,8)=SDV(:,2,8)+Q5V*Q2V    
        SDV(:,1,7)=SDV(:,1,7)+Q4V*Q2V
        SDV(:,2,7)=SDV(:,2,7)+Q6V*Q2V

        Q1V=Q1V+(AC(IL+4)*Q0V+AC(IL+5))*Q2V
        SDV(:,1,10)=SDV(:,1,10)+Q3V*Q1V
        SDV(:,2,10)=SDV(:,2,10)+Q5V*Q1V
        SDV(:,1,9)=SDV(:,1,9)+Q4V*Q1V
        SDV(:,2,9)=SDV(:,2,9)+Q6V*Q1V
     END DO
     DO K=1,10
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.8) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V
        Q1V=Q1V+(AC(IL)*Q0V+AC(IL+1))*Q2V
        Q2V=Q2V+(AC(IL+2)*Q0V+AC(IL+3))*Q1V
        SDV(:,1,6)=SDV(:,1,6)+Q3V*Q1V
        SDV(:,2,6)=SDV(:,2,6)+Q5V*Q1V
        SDV(:,1,5)=SDV(:,1,5)+Q4V*Q1V
        SDV(:,2,5)=SDV(:,2,5)+Q6V*Q1V
        SDV(:,1,8)=SDV(:,1,8)+Q3V*Q2V
        SDV(:,2,8)=SDV(:,2,8)+Q5V*Q2V    
        SDV(:,1,7)=SDV(:,1,7)+Q4V*Q2V
        SDV(:,2,7)=SDV(:,2,7)+Q6V*Q2V

        Q1V=Q1V+(AC(IL+4)*Q0V+AC(IL+5))*Q2V
        SDV(:,1,9)=SDV(:,1,9)+Q4V*Q1V
        SDV(:,2,9)=SDV(:,2,9)+Q6V*Q1V
     END DO
     DO K=1,9
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.7) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V
        Q1V=Q1V+(AC(IL)*Q0V+AC(IL+1))*Q2V
        Q2V=Q2V+(AC(IL+2)*Q0V+AC(IL+3))*Q1V
        SDV(:,1,6)=SDV(:,1,6)+Q3V*Q1V
        SDV(:,2,6)=SDV(:,2,6)+Q5V*Q1V
        SDV(:,1,5)=SDV(:,1,5)+Q4V*Q1V
        SDV(:,2,5)=SDV(:,2,5)+Q6V*Q1V
        SDV(:,1,8)=SDV(:,1,8)+Q3V*Q2V
        SDV(:,2,8)=SDV(:,2,8)+Q5V*Q2V    
        SDV(:,1,7)=SDV(:,1,7)+Q4V*Q2V
        SDV(:,2,7)=SDV(:,2,7)+Q6V*Q2V
     END DO
     DO K=1,8
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.6) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V
        Q1V=Q1V+(AC(IL)*Q0V+AC(IL+1))*Q2V
        Q2V=Q2V+(AC(IL+2)*Q0V+AC(IL+3))*Q1V
        SDV(:,1,6)=SDV(:,1,6)+Q3V*Q1V
        SDV(:,2,6)=SDV(:,2,6)+Q5V*Q1V
        SDV(:,1,5)=SDV(:,1,5)+Q4V*Q1V
        SDV(:,2,5)=SDV(:,2,5)+Q6V*Q1V
        SDV(:,1,7)=SDV(:,1,7)+Q4V*Q2V
        SDV(:,2,7)=SDV(:,2,7)+Q6V*Q2V
     END DO
     DO K=1,7
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.5) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V

        Q1V=Q1V+(AC(IL)*Q0V+AC(IL+1))*Q2V
        SDV(:,1,6)=SDV(:,1,6)+Q3V*Q1V
        SDV(:,2,6)=SDV(:,2,6)+Q5V*Q1V
        SDV(:,1,5)=SDV(:,1,5)+Q4V*Q1V
        SDV(:,2,5)=SDV(:,2,5)+Q6V*Q1V
     END DO
     DO K=1,6
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.4) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V

        Q1V=Q1V+(AC(IL)*Q0V+AC(IL+1))*Q2V
        SDV(:,1,5)=SDV(:,1,5)+Q4V*Q1V
        SDV(:,2,5)=SDV(:,2,5)+Q6V*Q1V
     END DO
     DO K=1,5
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.3) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,4)=SDV(:,1,4)+Q3V*Q2V
        SDV(:,2,4)=SDV(:,2,4)+Q5V*Q2V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V
     END DO
     DO K=1,4
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  ELSE IF(ILEV.EQ.2) THEN
     SDV=0
     DO IR=1,JB
        Q1V=Q(:,1,IR)
        Q2V=Q(:,2,IR)
        Q0V=Q(:,0,IR)
        Q3V=Q(:,3,IR)
        Q4V=Q(:,4,IR)
        Q5V=Q(:,5,IR)
        Q6V=Q(:,6,IR)           
        SDV(:,1,2)=SDV(:,1,2)+Q3V*Q1V
        SDV(:,2,2)=SDV(:,2,2)+Q5V*Q1V
        SDV(:,1,1)=SDV(:,1,1)+Q4V*Q1V
        SDV(:,2,1)=SDV(:,2,1)+Q6V*Q1V
        SDV(:,1,3)=SDV(:,1,3)+Q4V*Q2V
        SDV(:,2,3)=SDV(:,2,3)+Q6V*Q2V
     END DO
     DO K=1,3
        DO I=1,2
           SD(I,K)=SD(I,K)+SUM(SDV(:,I,K))
        END DO
     END DO
  END IF

END SUBROUTINE LXOGWS
