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
*     CALCULATE THE POSITION OF A SPECTRUM COEFFICIENT OF P_N^M 99/02/09
*-----------------------------------------------------------------------
*     IF M.GE.0 --> REAL PART
*     IF M.LT.0 --> IMAGINARY PART
************************************************************************
      SUBROUTINE SNNM2L(N,M,L)

      L=M+1+N*(N+1)

      END
************************************************************************
      SUBROUTINE SNL2NM(L,N,M)

      N=SQRT(1D0*(L-1))
      M=L-N*(N+1)-1

      END
