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
*     TRANSFORM GRID TO SPECTRA (x-integral)                  2004/09/09
************************************************************************
      SUBROUTINE U2G2SX(LM,KM,JM,IM,G,S,W,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM-1,2,0:IM/2-1)      
      DIMENSION W(-KM:KM,0:JM-1)
      DIMENSION ITJ(5),TJ(0:JM/2-1,6),ITI(5),TI(IM*2)

      CALL FTTRUF(JM,IM,G,W,ITI,TI)

      DO K=1,KM
        DO J=0,JM-1
          W( K,J)=G(J,1,K)
          W(-K,J)=G(J,2,K)
        END DO
      END DO

      DO J=0,JM-1
        W(0,J)=G(J,1,0)
      END DO

      CALL FTTRUF(2*KM+1,JM,W,G,ITJ,TJ)
      
      DO L=1,LM
        DO K=-KM,-1
          S( L,-K)= 2*(-1)**L*(TJ(L,5)*W(K,2*L  )
     &                         +TJ(L,6)*W(K,2*L+1))/(-K)
          S(-L,-K)=-2*(-1)**L*(TJ(L,5)*W(K,2*L+1)
     &                         -TJ(L,6)*W(K,2*L))/(-K)
        END DO
        S( L,0)= 2*(-1)**L*(TJ(L,5)*W(0,2*L  )+TJ(L,6)*W(0,2*L+1))
        S(-L,0)=-2*(-1)**L*(TJ(L,5)*W(0,2*L+1)-TJ(L,6)*W(0,2*L))
        DO K=1,KM
          S( L,-K)= 2*(-1)**L*(TJ(L,5)*W(K,2*L  )
     &                         +TJ(L,6)*W(K,2*L+1))/(-K)
          S(-L,-K)=-2*(-1)**L*(TJ(L,5)*W(K,2*L+1)
     &                         -TJ(L,6)*W(K,2*L))/(-K)
        END DO
      END DO

      DO K=-KM,-1
        S(0,-K)=W(K,0)/(-K)
      END DO
      DO K=1,KM
        S(0,-K)=W(K,0)/(-K)
      END DO
      S(0,0)=W(0,0)
      
      END
