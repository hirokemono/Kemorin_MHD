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
      SUBROUTINE SJNM2L(NN,N,M,L)

      IF(M.EQ.0) THEN
        L=N+1
      ELSE IF(M.GT.0) THEN
        L=1+NN+1+(M-1)*(2*NN+2-M)+2*(N-M)
      ELSE
        L=1+NN+1+(-M-1)*(2*NN+2+M)+2*(N+M)+1
      END IF

      END
