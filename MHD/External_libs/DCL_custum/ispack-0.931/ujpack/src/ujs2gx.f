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
*     TRANSFORM SPECTRA TO GRID (X-gradient)                  2009/06/15
************************************************************************
      SUBROUTINE UJS2GX(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM*IM-1)      
      DIMENSION W(0:JM*IM-1)
      DIMENSION ITJ(2,2),TJ(0:JM/2-1,14),ITI(2,2),TI(IM*3,2)


      W(0)=0
      W(1)=0
      DO K=1,KM
        W(2*K  )=-K*S(0,-K)
        W(2*K+1)= K*S(0, K)
      END DO
      DO I=2*KM+2,IM-1
        W(I)=0
      END DO

!$omp parallel do
      DO L=1,LM
        W(0+IM*(2*L-1))=0
        W(0+IM*(2*L  ))=0
        W(1+IM*(2*L-1))=0
        W(1+IM*(2*L  ))=0
        DO K=1,KM
          W(2*K  +IM*(2*L-1))=-K*( TJ(L,13)*S( L,-K)+TJ(L,14)*S(-L,-K))
          W(2*K+1+IM*(2*L-1))= K*( TJ(L,13)*S( L, K)+TJ(L,14)*S(-L, K))
          W(2*K  +IM*(2*L  ))=-K*(-TJ(L,13)*S(-L,-K)+TJ(L,14)*S( L,-K))
          W(2*K+1+IM*(2*L  ))= K*(-TJ(L,13)*S(-L, K)+TJ(L,14)*S( L, K))
        END DO
        DO I=2*KM+2,IM-1
          W(I+IM*(2*L-1))=0
          W(I+IM*(2*L  ))=0
        END DO
      END DO
!$omp end parallel do      

      CALL FJRRUN(W(0),G(0),G(IM),TI(1,1),ITI(1,1))      

!$omp parallel do
      DO L=1,LM
        CALL FJRRUN(W(0+IM*(2*L-1)),
     &    G(2*IM*(L-1)),G(IM*(2*L-1)),TI(1,1),ITI(1,1))
        CALL FJRRUN(W(0+IM*(2*L)),
     &    G(2*IM*(L-1)),G(IM*(2*L-1)),TI(1,1),ITI(1,1))        
      END DO
!$omp end parallel do      

!$omp parallel do
      DO I=0,IM-1
        G(0+JM*I)=W(I)
        G(1+JM*I)=0
        DO L=1,LM
          G(2*L  +JM*I)=W(I+IM*(2*L-1))
          G(2*L+1+JM*I)=W(I+IM*(2*L))
        END DO
        DO J=2*LM+2,JM-1
          G(J+JM*I)=0
        END DO
      END DO
!$omp end parallel do

!$omp parallel do
      DO I=0,IM-1,2
        CALL FJRRUN(G(JM*I),    W(JM*I),W(JM*(I+1)),TJ(0,1),ITJ(1,1))
        CALL FJRRUN(G(JM*(I+1)),W(JM*I),W(JM*(I+1)),TJ(0,1),ITJ(1,1))        
      END DO
!$omp end parallel do        

      END
