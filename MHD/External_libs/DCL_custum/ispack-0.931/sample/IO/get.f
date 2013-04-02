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

      CALL FHUOPN(IU,'data.dat','R',NB) 
                                !データセットの読み出しオープン
      CALL FEGETC(IU,N,C)       !文字配列の読み出し
      CALL FEGETI(IU,N,I)       !整数配列の読み出し
      CALL FEGETR(IU,N,R)       !単精度配列の読み出し
      CALL FEGETD(IU,N,D)       !倍精度配列の読み出し
      CALL FHUCLS(IU)           !データセットのクローズ

      DO J=1,N
        WRITE(6,*) C(J:J),I(J),R(J),D(J)
      END DO


      END
