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
      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(NALIGN=64)
      PARAMETER(NMAX=2048)
      CHARACTER C(0:8*NMAX*8-1+NALIGN-1)*1
      DIMENSION IP(2)

      CALL FJGTAD(C(0),IP)

      IF(IP(1).LT.0) THEN
        ID=NALIGN+MOD(IP(1),NALIGN)
      ELSE
        ID=MOD(IP(1),NALIGN)
      END IF
      ID=MOD(NALIGN-ID,NALIGN)

      N=1
      DO I=1,11
        N=N*2
        NH=N/2
        CALL RCHKSB(N,C(ID+0),C(ID+NH*16),C(ID+2*NH*16),C(ID+3*NH*16),
     &    C(ID+6*NH*16),C(ID+7*NH*16))
      END DO

      print *,'************** Passed all checks **************'

      END
