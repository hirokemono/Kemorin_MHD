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
SUBROUTINE LXQVWS(JB,AC,SD,Q,IL,ILEV)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JV=4    
  INTEGER(8) :: J,JB,IR,ILEV,IL
  REAL(8) :: AC(*),SD(4,*),Q(JV,0:10,JB),Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10

  IF(ILEV.EQ.0) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           

           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1
           SD(1,6)=SD(1,6)+Q3*Q1
           SD(2,6)=SD(2,6)+Q5*Q1           
           SD(1,5)=SD(1,5)+Q4*Q1
           SD(2,5)=SD(2,5)+Q6*Q1
           SD(3,6)=SD(3,6)+Q7*Q1
           SD(4,6)=SD(4,6)+Q9*Q1           
           SD(3,5)=SD(3,5)+Q8*Q1
           SD(4,5)=SD(4,5)+Q10*Q1
           
           SD(1,8)=SD(1,8)+Q3*Q2
           SD(2,8)=SD(2,8)+Q5*Q2           
           SD(1,7)=SD(1,7)+Q4*Q2
           SD(2,7)=SD(2,7)+Q6*Q2           
           SD(3,8)=SD(3,8)+Q7*Q2
           SD(4,8)=SD(4,8)+Q9*Q2           
           SD(3,7)=SD(3,7)+Q8*Q2
           SD(4,7)=SD(4,7)+Q10*Q2           
           
           Q1=Q1+(AC(IL+4)*Q0+AC(IL+5))*Q2
           Q(J,1,IR)=Q1
           Q(J,2,IR)=Q2+(AC(IL+6)*Q0+AC(IL+7))*Q1
       END DO
    END DO
  ELSE IF(ILEV.EQ.9) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           

           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1
           SD(1,6)=SD(1,6)+Q3*Q1
           SD(2,6)=SD(2,6)+Q5*Q1           
           SD(1,5)=SD(1,5)+Q4*Q1
           SD(2,5)=SD(2,5)+Q6*Q1
           SD(3,6)=SD(3,6)+Q7*Q1
           SD(4,6)=SD(4,6)+Q9*Q1           
           SD(3,5)=SD(3,5)+Q8*Q1
           SD(4,5)=SD(4,5)+Q10*Q1
           
           SD(1,8)=SD(1,8)+Q3*Q2
           SD(2,8)=SD(2,8)+Q5*Q2           
           SD(1,7)=SD(1,7)+Q4*Q2
           SD(2,7)=SD(2,7)+Q6*Q2           
           SD(3,8)=SD(3,8)+Q7*Q2
           SD(4,8)=SD(4,8)+Q9*Q2           
           SD(3,7)=SD(3,7)+Q8*Q2
           SD(4,7)=SD(4,7)+Q10*Q2           
           
           Q1=Q1+(AC(IL+4)*Q0+AC(IL+5))*Q2
           SD(1,10)=SD(1,10)+Q3*Q1
           SD(2,10)=SD(2,10)+Q5*Q1           
           SD(1,9)=SD(1,9)+Q4*Q1
           SD(2,9)=SD(2,9)+Q6*Q1           
           SD(3,10)=SD(3,10)+Q7*Q1
           SD(4,10)=SD(4,10)+Q9*Q1           
           SD(3,9)=SD(3,9)+Q8*Q1
           SD(4,9)=SD(4,9)+Q10*Q1           
       END DO
    END DO
  ELSE IF(ILEV.EQ.8) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           

           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1
           SD(1,6)=SD(1,6)+Q3*Q1
           SD(2,6)=SD(2,6)+Q5*Q1           
           SD(1,5)=SD(1,5)+Q4*Q1
           SD(2,5)=SD(2,5)+Q6*Q1
           SD(3,6)=SD(3,6)+Q7*Q1
           SD(4,6)=SD(4,6)+Q9*Q1           
           SD(3,5)=SD(3,5)+Q8*Q1
           SD(4,5)=SD(4,5)+Q10*Q1
           
           SD(1,8)=SD(1,8)+Q3*Q2
           SD(2,8)=SD(2,8)+Q5*Q2           
           SD(1,7)=SD(1,7)+Q4*Q2
           SD(2,7)=SD(2,7)+Q6*Q2           
           SD(3,8)=SD(3,8)+Q7*Q2
           SD(4,8)=SD(4,8)+Q9*Q2           
           SD(3,7)=SD(3,7)+Q8*Q2
           SD(4,7)=SD(4,7)+Q10*Q2           
           
           Q1=Q1+(AC(IL+4)*Q0+AC(IL+5))*Q2
           SD(1,9)=SD(1,9)+Q4*Q1
           SD(2,9)=SD(2,9)+Q6*Q1           
           SD(3,9)=SD(3,9)+Q8*Q1
           SD(4,9)=SD(4,9)+Q10*Q1           
        END DO
     END DO
  ELSE IF(ILEV.EQ.7) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           

           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1
           SD(1,6)=SD(1,6)+Q3*Q1
           SD(2,6)=SD(2,6)+Q5*Q1           
           SD(1,5)=SD(1,5)+Q4*Q1
           SD(2,5)=SD(2,5)+Q6*Q1
           SD(3,6)=SD(3,6)+Q7*Q1
           SD(4,6)=SD(4,6)+Q9*Q1           
           SD(3,5)=SD(3,5)+Q8*Q1
           SD(4,5)=SD(4,5)+Q10*Q1
           
           SD(1,8)=SD(1,8)+Q3*Q2
           SD(2,8)=SD(2,8)+Q5*Q2           
           SD(1,7)=SD(1,7)+Q4*Q2
           SD(2,7)=SD(2,7)+Q6*Q2           
           SD(3,8)=SD(3,8)+Q7*Q2
           SD(4,8)=SD(4,8)+Q9*Q2           
           SD(3,7)=SD(3,7)+Q8*Q2
           SD(4,7)=SD(4,7)+Q10*Q2           
        END DO
     END DO
  ELSE IF(ILEV.EQ.6) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           

           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1
           SD(1,6)=SD(1,6)+Q3*Q1
           SD(2,6)=SD(2,6)+Q5*Q1           
           SD(1,5)=SD(1,5)+Q4*Q1
           SD(2,5)=SD(2,5)+Q6*Q1
           SD(3,6)=SD(3,6)+Q7*Q1
           SD(4,6)=SD(4,6)+Q9*Q1           
           SD(3,5)=SD(3,5)+Q8*Q1
           SD(4,5)=SD(4,5)+Q10*Q1
           
           SD(1,7)=SD(1,7)+Q4*Q2
           SD(2,7)=SD(2,7)+Q6*Q2           
           SD(3,7)=SD(3,7)+Q8*Q2
           SD(4,7)=SD(4,7)+Q10*Q2           
        END DO
     END DO
  ELSE IF(ILEV.EQ.5) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           

           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           SD(1,6)=SD(1,6)+Q3*Q1
           SD(2,6)=SD(2,6)+Q5*Q1           
           SD(1,5)=SD(1,5)+Q4*Q1
           SD(2,5)=SD(2,5)+Q6*Q1
           SD(3,6)=SD(3,6)+Q7*Q1
           SD(4,6)=SD(4,6)+Q9*Q1           
           SD(3,5)=SD(3,5)+Q8*Q1
           SD(4,5)=SD(4,5)+Q10*Q1
        END DO
     END DO
  ELSE IF(ILEV.EQ.4) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           

           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           SD(1,5)=SD(1,5)+Q4*Q1
           SD(2,5)=SD(2,5)+Q6*Q1
           SD(3,5)=SD(3,5)+Q8*Q1
           SD(4,5)=SD(4,5)+Q10*Q1
        END DO
     END DO
  ELSE IF(ILEV.EQ.3) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,4)=SD(1,4)+Q3*Q2
           SD(2,4)=SD(2,4)+Q5*Q2           
           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,4)=SD(3,4)+Q7*Q2
           SD(4,4)=SD(4,4)+Q9*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           
        END DO
     END DO
  ELSE IF(ILEV.EQ.2) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q3=Q(J,3,IR)
           Q4=Q(J,4,IR)
           Q5=Q(J,5,IR)
           Q6=Q(J,6,IR)
           Q7=Q(J,7,IR)
           Q8=Q(J,8,IR)
           Q9=Q(J,9,IR)
           Q10=Q(J,10,IR)
           SD(1,2)=SD(1,2)+Q3*Q1
           SD(2,2)=SD(2,2)+Q5*Q1           
           SD(1,1)=SD(1,1)+Q4*Q1
           SD(2,1)=SD(2,1)+Q6*Q1
           SD(3,2)=SD(3,2)+Q7*Q1
           SD(4,2)=SD(4,2)+Q9*Q1           
           SD(3,1)=SD(3,1)+Q8*Q1
           SD(4,1)=SD(4,1)+Q10*Q1

           SD(1,3)=SD(1,3)+Q4*Q2
           SD(2,3)=SD(2,3)+Q6*Q2           
           SD(3,3)=SD(3,3)+Q8*Q2
           SD(4,3)=SD(4,3)+Q10*Q2           
        END DO
     END DO
  END IF

END SUBROUTINE LXQVWS
