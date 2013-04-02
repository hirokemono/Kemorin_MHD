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
                                !�ǡ������åȤν񤭹��ߥ����ץ�
      CALL FEPUTC(IU,N,C)       !ʸ������ν񤭹���
      CALL FEPUTI(IU,N,I)       !��������ν񤭹���
      CALL FEPUTR(IU,N,R)       !ñ��������ν񤭹���
      CALL FEPUTD(IU,N,D)       !����������ν񤭹���
      CALL FHUCLS(IU)           !�ǡ������åȤΥ�����

      END
