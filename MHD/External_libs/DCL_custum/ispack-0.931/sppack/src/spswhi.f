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
*     浅水方程式の線形項による発展のための行列の初期化    
*                        (角運動量保存の高階粘性項含む)       2000/08/16
************************************************************************
      SUBROUTINE SPSWHI(MM,BARPHI,DNU,ALPHA,LEV,DT,CL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION CL((MM+1)*(MM+1),5)

      CL(1,1)=1
      CL(1,2)=1
      CL(1,3)=0
      CL(1,4)=1
      CL(1,5)=0
      DO L=2,(MM+1)*(MM+1)
        N=SQRT(1D0*(L-1))
        DNUD=DNU*(N*(N+1)-2D0)**LEV
        C=-DNUD*((2-ALPHA)*(-N*(N+1))+2)/2
        S=-BARPHI*(-N*(N+1))
        FREQ2=S-C*C
        ECD=EXP(-C*DT)
        CL(L,1)=EXP(DNUD*DT*(-N*(N+1)+2))
        IF(FREQ2.GT.0) THEN
          FREQ=SQRT(FREQ2)
          CFD=COS(FREQ*DT)
          SFDF=SIN(FREQ*DT)/FREQ
        ELSE IF(FREQ2.LT.0) THEN
          FREQ=SQRT(-FREQ2)
          CFD=COSH(FREQ*DT)
          SFDF=SINH(FREQ*DT)/FREQ
        ELSE
          CFD=1
          SFDF=DT
        END IF
        CL(L,2)=ECD*(CFD-C*SFDF)
        CL(L,3)=ECD*N*(N+1)*SFDF
        CL(L,4)=ECD*(CFD+C*SFDF)
        CL(L,5)=ECD*(-BARPHI)*SFDF
      ENDDO

      END
