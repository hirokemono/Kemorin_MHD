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
*     INITIALIZATION OF SJPACK                                2009/08/11
*-----------------------------------------------------------------------
      SUBROUTINE SJINIT(MM,NM,JM,IM,P,R,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM/2,MM+4),R((MM+1)*(2*NM-MM-1)+1),IT(2,2),T(IM*3,2)

      CALL LJINIT(MM,NM,JM,P,R)
      CALL FJRINI(IM,1,1,IT(1,1),T(1,1))
      CALL FJRINI(IM,2,2,IT(1,2),T(1,2))

      END
