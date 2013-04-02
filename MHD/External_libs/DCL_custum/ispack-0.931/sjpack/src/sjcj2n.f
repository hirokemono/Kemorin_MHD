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
*     REPACKING SPECTRA FROM SJ TO SN                         2009/08/11
************************************************************************
      SUBROUTINE SJCJ2N(MM,S,SN)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SN((MM+1)*(MM+1)),S((MM+1)*(MM+1))

      M=0
      DO N=0,MM
        L=M+1+N*(N+1)        
        SN(L)=S(N+1)
      END DO

      F=SQRT(2D0)

      IND=MM+1

      DO M=1,MM
        DO N=M,MM
          L=M+1+N*(N+1)                  
          IND=IND+1          
          SN(L)=S(IND)*F
          L=-M+1+N*(N+1)
          IND=IND+1
          SN(L)=S(IND)*F
        END DO
      END DO

      END
