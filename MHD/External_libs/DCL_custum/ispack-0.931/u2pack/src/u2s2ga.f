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
*     TRANSFORM SPECTRA TO GRID                               2004/08/26
************************************************************************
      SUBROUTINE U2S2GA(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM-1,2,0:IM/2-1)      
      DIMENSION W(-KM:KM,0:JM-1)
      DIMENSION ITJ(5),TJ(0:JM/2-1,6),ITI(5),TI(IM*2)

      CALL BSSET0((2*KM+1)*JM,W)

      DO K=-KM,KM
        W(K,0)=S(0,K)
      END DO

      DO L=1,LM
        DO K=-KM,KM
          W(K,2*L  )=0.5D0*(-1)**L*( TJ(L,5)*S( L,K)+TJ(L,6)*S(-L,K))
          W(K,2*L+1)=0.5D0*(-1)**L*(-TJ(L,5)*S(-L,K)+TJ(L,6)*S( L,K))
        END DO
      END DO

      CALL FTTRUB(2*KM+1,JM,W,G,ITJ,TJ)

      CALL BSSET0(JM*IM,G)

      DO K=1,KM
        DO J=0,JM-1
          G(J,1,K)=W( K,J)
          G(J,2,K)=W(-K,J)          
        END DO
      END DO

      DO J=0,JM-1
        G(J,1,0)=W(0,J)
      END DO

      CALL FTTRUB(JM,IM,G,W,ITI,TI)

      END
