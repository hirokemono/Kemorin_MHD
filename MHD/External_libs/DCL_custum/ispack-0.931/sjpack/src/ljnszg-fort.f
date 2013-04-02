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
      SUBROUTINE LJNSZG(JH,S1,S2,R,Y,QA,QB,W1,W2)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W1(JH),W2(JH),Y(JH)
      DIMENSION QA(JH),QB(JH)

      DO J=1,JH
        W1(J)=W1(J)+S1*QA(J)
        W2(J)=W2(J)+S2*QA(J)        
        QB(J)=QB(J)+R*Y(J)*QA(J)
      END DO

      END
