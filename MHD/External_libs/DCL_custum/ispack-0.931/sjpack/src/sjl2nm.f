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
*     CALCULATE THE POSITION OF A SPECTRUM COEFFICIENT OF P_N^M
*                                                             2009/08/18
*-----------------------------------------------------------------------
      SUBROUTINE SJL2NM(NN,L,N,M)

      IF(L.LE.NN+1) THEN
        M=0
        N=L-1
      ELSE
        M=NN+1.5D0-SQRT(1D0*(NN+1)*(NN+1)-L+1)
        LD=1+NN+1+(M-1)*(2*NN+2-M)
        N=M+(L-LD)/2
        IF(L-LD.NE.2*(N-M)) THEN
          M=-M
        END IF
      END IF

      END
