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
SUBROUTINE LXOSWG(JB,AC,SD,Q,IL,ILEV)

  IMPLICIT NONE
  INTEGER(8),PARAMETER :: JV=8
  INTEGER(8) :: J,JB,IR,ILEV,IL
  REAL(8) :: AC(*),SD(2,*),Q(JV,0:6,JB),Q0,Q1,Q2,Q3,Q4,Q5,Q6

  IF(ILEV.EQ.0) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)                      
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2
           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1

           Q(J,3,IR)=Q3+SD(1,6)*Q1+SD(1,8)*Q2
           Q(J,4,IR)=Q4+SD(1,5)*Q1+SD(1,7)*Q2
           Q(J,5,IR)=Q5+SD(2,6)*Q1+SD(2,8)*Q2
           Q(J,6,IR)=Q6+SD(2,5)*Q1+SD(2,7)*Q2
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
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2
           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1

           Q3=Q3+SD(1,6)*Q1+SD(1,8)*Q2
           Q4=Q4+SD(1,5)*Q1+SD(1,7)*Q2
           Q5=Q5+SD(2,6)*Q1+SD(2,8)*Q2
           Q6=Q6+SD(2,5)*Q1+SD(2,7)*Q2
           Q1=Q1+(AC(IL+4)*Q0+AC(IL+5))*Q2

           Q(J,3,IR)=Q3+SD(1,10)*Q1
           Q(J,4,IR)=Q4+SD(1,9)*Q1
           Q(J,5,IR)=Q5+SD(2,10)*Q1
           Q(J,6,IR)=Q6+SD(2,9)*Q1
        END DO
     END DO
  ELSE IF(ILEV.EQ.8) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)                      
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2
           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1

           Q3=Q3+SD(1,6)*Q1+SD(1,8)*Q2
           Q4=Q4+SD(1,5)*Q1+SD(1,7)*Q2
           Q5=Q5+SD(2,6)*Q1+SD(2,8)*Q2
           Q6=Q6+SD(2,5)*Q1+SD(2,7)*Q2
           Q1=Q1+(AC(IL+4)*Q0+AC(IL+5))*Q2

           Q(J,3,IR)=Q3
           Q(J,4,IR)=Q4+SD(1,9)*Q1
           Q(J,5,IR)=Q5
           Q(J,6,IR)=Q6+SD(2,9)*Q1
        END DO
     END DO
  ELSE IF(ILEV.EQ.7) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)                      
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2
           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1

           Q3=Q3+SD(1,6)*Q1+SD(1,8)*Q2
           Q4=Q4+SD(1,5)*Q1+SD(1,7)*Q2
           Q5=Q5+SD(2,6)*Q1+SD(2,8)*Q2
           Q6=Q6+SD(2,5)*Q1+SD(2,7)*Q2

           Q(J,3,IR)=Q3
           Q(J,4,IR)=Q4
           Q(J,5,IR)=Q5
           Q(J,6,IR)=Q6
        END DO
     END DO
  ELSE IF(ILEV.EQ.6) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)                      
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2
           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2
           Q2=Q2+(AC(IL+2)*Q0+AC(IL+3))*Q1

           Q3=Q3+SD(1,6)*Q1
           Q4=Q4+SD(1,5)*Q1+SD(1,7)*Q2
           Q5=Q5+SD(2,6)*Q1
           Q6=Q6+SD(2,5)*Q1+SD(2,7)*Q2

           Q(J,3,IR)=Q3
           Q(J,4,IR)=Q4
           Q(J,5,IR)=Q5
           Q(J,6,IR)=Q6
        END DO
     END DO
  ELSE IF(ILEV.EQ.5) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)                      
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2
           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2

           Q3=Q3+SD(1,6)*Q1
           Q4=Q4+SD(1,5)*Q1
           Q5=Q5+SD(2,6)*Q1
           Q6=Q6+SD(2,5)*Q1

           Q(J,3,IR)=Q3
           Q(J,4,IR)=Q4
           Q(J,5,IR)=Q5
           Q(J,6,IR)=Q6
        END DO
     END DO
  ELSE IF(ILEV.EQ.4) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q0=Q(J,0,IR)                      
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2
           Q1=Q1+(AC(IL)*Q0+AC(IL+1))*Q2

           Q4=Q4+SD(1,5)*Q1
           Q6=Q6+SD(2,5)*Q1

           Q(J,3,IR)=Q3
           Q(J,4,IR)=Q4
           Q(J,5,IR)=Q5
           Q(J,6,IR)=Q6
        END DO
     END DO
  ELSE IF(ILEV.EQ.3) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q3=Q(J,3,IR)+SD(1,2)*Q1+SD(1,4)*Q2
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1+SD(2,4)*Q2
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2

           Q(J,3,IR)=Q3
           Q(J,4,IR)=Q4
           Q(J,5,IR)=Q5
           Q(J,6,IR)=Q6
        END DO
     END DO
  ELSE IF(ILEV.EQ.2) THEN
     DO IR=1,JB
        DO J=1,JV
           Q1=Q(J,1,IR)
           Q2=Q(J,2,IR)
           Q3=Q(J,3,IR)+SD(1,2)*Q1
           Q4=Q(J,4,IR)+SD(1,1)*Q1+SD(1,3)*Q2
           Q5=Q(J,5,IR)+SD(2,2)*Q1
           Q6=Q(J,6,IR)+SD(2,1)*Q1+SD(2,3)*Q2

           Q(J,3,IR)=Q3
           Q(J,4,IR)=Q4
           Q(J,5,IR)=Q5
           Q(J,6,IR)=Q6
        END DO
     END DO
  END IF

END SUBROUTINE LXOSWG
