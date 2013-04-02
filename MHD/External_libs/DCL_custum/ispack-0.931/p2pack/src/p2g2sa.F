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
*     TRANSFORM GRID TO SPECTRA                               2005/07/27
************************************************************************
#ifdef FFTW2
      SUBROUTINE P2G2SA(LM,KM,JM,IM,G,S,W,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(IFWD=-1,IBWD=1)
      PARAMETER(IR2C=-1,IC2R=1)
      PARAMETER(IEST=0,IMES=1)
      PARAMETER(IOOP=0,IINP=8)
      PARAMETER(IWSD=16)
      PARAMETER(ITRD=128)
#ifdef MEASURE
      PARAMETER(ISWTCH=IMES+ITRD+IWSD)
#else
      PARAMETER(ISWTCH=IEST+ITRD+IWSD)
#endif      
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM*IM-1)
      DIMENSION W(0:JM*IM-1)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      INTEGER*8 IPLAN,IPR1,IPR2

      call fftw_f77_create_plan(IPLAN,IM,IFWD,ISWTCH)
      call rfftw_f77_create_plan(IPR1,IM,IR2C,ISWTCH)
      call rfftw_f77_create_plan(IPR2,JM,IR2C,ISWTCH)

!$omp parallel do
      DO I=0,IM-1
        call rfftw_f77_one(IPR2,G(JM*I),W(JM*I))
      END DO
!$omp end parallel do      

!$omp parallel do
      DO I=0,IM-1
        DO L=0,LM
          G(L    +(2*LM+1)*I)=W(L         +JM*I)
        END DO
        DO L=1,LM
          G(LM+L +(2*LM+1)*I)=W(JM-LM-1+L +JM*I)
        END DO
      END DO
!$omp end parallel do

      FIJM=1D0/(IM*JM)

!$omp parallel do
      DO L=1,LM
        DO I=0,IM-1
          W(2*I   +2*IM*(L-1))=G(L          +(2*LM+1)*I)*FIJM
          W(2*I+1 +2*IM*(L-1))=G((2*LM+1)-L +(2*LM+1)*I)*FIJM
        END DO
      END DO
!$omp end parallel do

!$omp parallel do      
      DO I=0,IM-1
        W(I +2*IM*LM)=G(0 +(2*LM+1)*I)*FIJM
      END DO
!$omp end parallel do

!$omp parallel do      
      DO L=0,LM
        IF(L.EQ.0) THEN
          call rfftw_f77_one(IPR1,W(2*IM*LM),G(2*IM*LM))
        ELSE
          call fftw_f77_one(IPLAN,W(2*IM*(L-1)),G(2*IM*(L-1)))
        END IF
      END DO
!$omp end parallel do      

!$omp parallel do      
      DO L=1,LM
        S( L,0)=G(0 +2*IM*(L-1))
        S(-L,0)=G(1 +2*IM*(L-1))
        DO K=1,KM
          S( L, K)= G(2*K        +2*IM*(L-1))
          S(-L,-K)= G(2*K+1      +2*IM*(L-1))
          S(-L, K)= G(2*(IM-K)   +2*IM*(L-1)) 
          S( L,-K)=-G(2*(IM-K)+1 +2*IM*(L-1))
        END DO
      END DO
!$omp end parallel do      

!$omp parallel do      
      DO K=1,KM
        S(0,K)=G(K    +2*IM*LM)
        S(0,-K)=G(IM-K +2*IM*LM)
      END DO
!$omp end parallel do      
      
      S(0,0)=G(0 +2*IM*LM)

      call fftw_f77_destroy_plan(IPLAN)
      call rfftw_f77_destroy_plan(IPR1)      
      call rfftw_f77_destroy_plan(IPR2)
      
      END
************************************************************************
#else
      SUBROUTINE P2G2SA(LM,KM,JM,IM,G,S,W,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:JM-1,2,0:IM/2-1)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION W(KM,0:JM-1,2)      
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      CALL FTTRUF(JM,IM,G,W,ITI,TI)

      CALL BSSET0(JM,G(0,2,0))

      CALL FTTZUF(1,JM,G,W,ITJ,TJ)

      DO L=1,LM
        S(L,0)=G(L,1,0)
        S(-L,0)=G(L,2,0)
      END DO

      S(0,0)=G(0,1,0)

      DO IR=1,2
        DO K=1,KM
          DO J=0,JM-1
            W(K,J,IR)=G(J,IR,K)
          END DO
        END DO
      END DO

      CALL FTTZUF(KM,JM,W,G,ITJ,TJ)

      DO L=1,LM
        DO K=1,KM
          S(L,K)=W(K,L,1)
          S(-L,-K)=W(K,L,2)
          S(-L,K)=W(K,JM-L,1)
          S(L,-K)=W(K,JM-L,2)
        END DO
      END DO

      DO K=1,KM
        S(0,K)=W(K,0,1)
        S(0,-K)=W(K,0,2)
      END DO

      END
#endif
