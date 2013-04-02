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
      PARAMETER(NB=23476,IU=10)
      PARAMETER(N=8)

      CHARACTER C*(N)
      INTEGER I(N)
      REAL R(N)
      REAL*8 D(N)

      DO J=1,N
        R(J)=(-1)**J*J*10000D0**J
        D(J)=(-1)**J*J*10000D0**J
        I(J)=(-1)**J*J*10**J
        WRITE(C(J:J),'(I1)') J
      END DO

      CALL FHUOPN(IU,'data.dat','W',NB) 
                                !データセットの書き込みオープン
      CALL FEPUTC(IU,N,C)       !文字配列の書き込み
      CALL FEPUTI(IU,N,I)       !整数配列の書き込み
      CALL FEPUTR(IU,N,R)       !単精度配列の書き込み
      CALL FEPUTD(IU,N,D)       !倍精度配列の書き込み
      CALL FHUCLS(IU)           !データセットのクローズ

      END
