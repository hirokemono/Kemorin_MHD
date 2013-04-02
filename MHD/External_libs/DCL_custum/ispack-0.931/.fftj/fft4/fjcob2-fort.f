************************************************************************
* FTTJ:  An FFT library
* Copyright (C) 2008 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
* fft 4 out-of-place backward
*-----------------------------------------------------------------------
      SUBROUTINE FJCOB2(X,X1,Z2,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      DIMENSION X(0:1,0:3),X1(0:1,0:3),Z2(0:3)
      DIMENSION ZT(2*4)

      XTMP00=X(0,0)+X(0,2)
      XTMP10=X(1,0)+X(1,2)
      XTMP02=X(0,0)-X(0,2)
      XTMP12=X(1,0)-X(1,2)
      XTMP01=X(0,1)+X(0,3)
      XTMP11=X(1,1)+X(1,3)
      XTMP03=X(1,3)-X(1,1)
      XTMP13=X(0,1)-X(0,3)

      X1(0,0)=XTMP00+XTMP01
      X1(1,0)=XTMP10+XTMP11        
      X1(0,2)=XTMP00-XTMP01
      X1(1,2)=XTMP10-XTMP11        
      X1(0,1)=XTMP02+XTMP03
      X1(1,1)=XTMP12+XTMP13
      X1(0,3)=XTMP02-XTMP03
      X1(1,3)=XTMP12-XTMP13        

      END
