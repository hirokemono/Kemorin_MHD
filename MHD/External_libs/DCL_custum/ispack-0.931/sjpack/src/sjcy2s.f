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
*     REPACKING SPECTRA FOR y-div                             2009/08/17
************************************************************************
      SUBROUTINE SJCY2S(MM,SG,S,C)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)),SG((MM+4)*MM+2)
      DIMENSION C((MM+1)*(MM+1))      

      M=0
      CALL LJCYZS(MM,SG,S,C)              
      DO M=1,MM
        NS=MM+1+(MM+(MM-(M-2)))*(M-1)+1
        NSG=MM+2+(MM+1+(MM+1-(M-2)))*(M-1)+1        
        CALL LJCYWS(MM,M,SG(NSG),S(NS),C)
      END DO

      END
