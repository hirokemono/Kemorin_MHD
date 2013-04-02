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
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(JM=16,IM=10,LM=5,KM=3)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM-1,2,0:IM/2-1)
      DIMENSION W(JM*IM)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      CALL N2INIT(JM,IM,ITJ,TJ,ITI,TI)

      DO K=-KM,KM
        DO L=-LM,LM
          S(L,K)=K+1D-3*L
        END DO
      END DO

      CALL N2S2GA(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI)
      CALL N2G2SA(LM,KM,JM,IM,G,S,W,ITJ,TJ,ITI,TI)

      DO K=-KM,KM
        DO L=-LM,LM
          WRITE(6,*) L,K,S(L,K),K+1D-3*L
        END DO
      END DO

      END
