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
*************************************************************************
*     ∑–≈Ÿ»˘ ¨§Œ±Èªª                                          1999/03/29
************************************************************************
      SUBROUTINE SPCLAM(MM,A,B,IRM)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION B((MM+1)*(MM+1))
      DIMENSION IRM((MM+1)*(MM+1),2)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        B(IRM(L,1))=IRM(L,2)*A(L)
      END DO

      END
