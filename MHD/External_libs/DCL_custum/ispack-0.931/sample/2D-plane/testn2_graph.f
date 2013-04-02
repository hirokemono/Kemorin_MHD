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
*      PARAMETER(JM=16,IM=10,LM=5,KM=3)
      PARAMETER(JM=16,IM=16,LM=5,KM=5)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(0:JM-1,0:IM-1)
      DIMENSION W(JM*IM)
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      REAL RG(0:IM,0:JM)

      CALL N2INIT(JM,IM,ITJ,TJ,ITI,TI)

      CALL BSSET0((2*LM+1)*(2*KM+1),S)
      
      WRITE(6,*) 'K,L ?'
      READ(5,*) K,L
      S(L,K)=1

      CALL N2S2GA(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI)

      DO I=0,IM-1
        DO J=0,JM-1
          RG(I,J)=G(J,I)
        END DO
      END DO

      DO J=0,JM-1
        RG(IM,J)=RG(0,J)
      END DO
      DO I=0,IM-1
        RG(I,JM)=RG(I,0)
      END DO
      RG(IM,JM)=RG(0,0)

      CALL GROPN(1)
      CALL GRFRM
      CALL SGSVPT(0.2,0.9,0.2,0.9)
      CALL SGSWND(0.0,1.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL USDAXS
      CALL UDCNTR(RG,IM+1,IM+1,JM+1)
      CALL GRCLS

      END
