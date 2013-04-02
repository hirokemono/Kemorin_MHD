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
*     TRANSFORM SPECTRA TO GRID                               2009/05/23
*                                       ÊÂÎó²½¤Î¥Ð¥°¤ò½¤Àµ    2009/05/26
************************************************************************
      SUBROUTINE PZS2GA(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM*IM-1)
      DIMENSION W(0:JM*IM-1)
      DIMENSION ITJ(2,2),TJ(JM*3,2),ITI(2,2),TI(IM*4,2)

      W(0)=S(0,0)
      W(1)=0
!$omp parallel do
      DO K=1,KM
        W(2*K       )= S(0, K)
        W(2*K+1     )= S(0,-K)
        W(2*(IM-K)  )= S(0, K)
        W(2*(IM-K)+1)=-S(0,-K)
      END DO
!$omp end parallel do

!$omp parallel do
      DO K=KM+1,IM-KM-1      
        W(2*K)  =0
        W(2*K+1)=0        
      END DO
!$omp end parallel do            

!$omp parallel do      
      DO L=1,LM
        W(0+2*IM*L)=S( L,0)
        W(1+2*IM*L)=S(-L,0)
        DO K=1,KM
          W(2*K       +2*IM*L)= S( L, K)
          W(2*K+1     +2*IM*L)= S(-L,-K)
          W(2*(IM-K)  +2*IM*L)= S(-L, K)
          W(2*(IM-K)+1+2*IM*L)=-S( L,-K)
        END DO
        DO K=KM+1,IM-KM-1
          W(2*K   +2*IM*L)=0
          W(2*K+1 +2*IM*L)=0
        END DO
      END DO
!$omp end parallel do

!$omp parallel do      
      DO L=0,LM,2
        CALL FJCRUN(W(2*IM*L),G(2*IM*L),G(2*IM*(L+1)),TI(1,1),ITI(1,1))
        IF(L.NE.LM) THEN
          CALL FJCRUN(W(2*IM*(L+1)),G(2*IM*L),
     &                G(2*IM*(L+1)),TI(1,1),ITI(1,1))
        END IF
      END DO
!$omp end parallel do

!$omp parallel do      
      DO I=0,IM-1
        G(0+JM*I)=W(2*I)
        G(1+JM*I)=0
        DO L=1,LM
          G(2*L  +JM*I)=W(2*I  +2*IM*L)
          G(2*L+1+JM*I)=W(2*I+1+2*IM*L)
        END DO
        DO L=2*LM+2,JM-1
          G(L+JM*I)=0
        END DO
      END DO
!$omp end parallel do

!$omp parallel do
      DO I=0,IM-1,2
        CALL FJRRUN(G(JM*I),W(JM*I),W(JM*(I+1)),TJ(1,1),ITJ(1,1))
        CALL FJRRUN(G(JM*(I+1)),W(JM*I),W(JM*(I+1)),TJ(1,1),ITJ(1,1))
      END DO
!$omp end parallel do

      END
