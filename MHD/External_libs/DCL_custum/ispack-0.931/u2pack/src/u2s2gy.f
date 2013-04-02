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
*     TRANSFORM SPECTRA TO GRID (y-gradient)                  2004/08/26
************************************************************************
      SUBROUTINE U2S2GY(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI,Y,R)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM-1,2,0:IM/2-1)      
      DIMENSION W(-KM:KM,0:JM-1)
      DIMENSION ITJ(5),TJ(0:JM/2-1,6),ITI(5),TI(IM*2)
      DIMENSION Y(0:JM-1)

      CALL BSSET0((2*KM+1)*JM,W)

      DO L=1,LM
        DO K=-KM,KM
          W(K,2*L  )=-L*0.5D0*(-1)**L*(-TJ(L,5)*S(-L,K)+TJ(L,6)*S( L,K))
          W(K,2*L+1)= L*0.5D0*(-1)**L*( TJ(L,5)*S( L,K)+TJ(L,6)*S(-L,K))
        END DO
      END DO

      CALL FTTRUB(2*KM+1,JM,W,G,ITJ,TJ)

      CALL BSSET0(JM*IM,G)

      DO K=1,KM
        DO J=0,JM-1
          DR=1/(R*(1+(Y(J)/(2*R))*(Y(J)/(2*R))))
          G(J,1,K)=W( K,J)*DR
          G(J,2,K)=W(-K,J)*DR
        END DO
      END DO

      DO J=0,JM-1
        G(J,1,0)=W(0,J)/(R*(1+(Y(J)/(2*R))*(Y(J)/(2*R))))
      END DO

      CALL FTTRUB(JM,IM,G,W,ITI,TI)

      END
