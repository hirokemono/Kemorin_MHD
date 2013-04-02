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
*     Àõ¿åÊýÄø¼°¤ÎÀþ·Á¹à¤Ë¤è¤ëÈ¯Å¸(³Ñ±¿Æ°ÎÌÊÝÂ¸¤Î»¶°ï´Þ¤à)    2000/04/08
************************************************************************
      SUBROUTINE SPSWLV(MM,AVT,DIV,PHI,CL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION CL((MM+1)*(MM+1),5)

      DO L=1,(MM+1)*(MM+1)
        AVT(L)=CL(L,1)*AVT(L)
        TMPDIV=DIV(L)
        DIV(L)=CL(L,2)*DIV(L)+CL(L,3)*PHI(L)
        PHI(L)=CL(L,4)*PHI(L)+CL(L,5)*TMPDIV
      END DO

      END
