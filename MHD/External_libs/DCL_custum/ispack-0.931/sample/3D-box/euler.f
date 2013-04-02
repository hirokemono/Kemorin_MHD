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
***********************************************************************
*     euler.f: P3PACK��Ȥä�3������ȯ��Euler��������򤯥ץ����
*                                                  2002/03/16 K.Ishioka
*                      (P3ELNL��ɬ�פʺ���ΰ褬���ä��Τ��б������ѹ�)
*                                                  2002/05/06 K.Ishioka      
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* �ѥ�᥿�������� 

      PARAMETER(NM=21,MM=21,LM=21) ! �����ȿ�
      PARAMETER(N=(2*LM+1)*(2*MM+1)*(2*NM+1)*2)
      DIMENSION Z(N)   ! ���٥٥��ȥ���Ǽ���뽾°�ѿ�
      DIMENSION W(N,3) ! ����ΰ�
      EXTERNAL SBGDZN

      NSTEP=10                 !����ȯŸ���륹�ƥå׿�
      DT=0.01D0                !��¸�̤Υ����å��Τ���λ��ֳִ�
      M=1                      !Runge-Kutta�ǤΥ��ƥå�ʬ���

* �ѥå������Ƚ�°�ѿ��ν����

      CALL SBINIT
      CALL SBINIZ(Z)

* ����ȯŸ�������¸�̤Υ����å�

      T=0                       !T�ϻ����ɽ���ѿ�
      CALL SBGDCK(T,Z) 
      DO ISTEP=1,NSTEP
        CALL TDRK4U(N,M,DT,T,Z,W,SBGDZN)
        CALL SBGDCK(T,Z)
      END DO

      END
***********************************************************************
*     ������ʬ����ɬ�פʼ�ν�����ޤȤ᤿���֥롼����ѥå�����
*----------------------------------------------------------------------
*     �ѥå������ν�����򤹤륵�֥롼����
*----------------------------------------------------------------------
      SUBROUTINE SBINIT

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NM=21,MM=21,LM=21) ! main program��Ʊ���ͤ�Ϳ���뤳��.
      PARAMETER(KM=64,JM=64,IM=64) ! �ʻ����Υ�����
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)  ! ���٥٥��ȥ��2��ʬ
      DIMENSION DZ(-NM:NM,-MM:MM,-LM:LM,2) ! Z�λ����Ѳ�Ψ
      DIMENSION WS(-NM:NM,-MM:MM,-LM:LM) ! ����ΰ�
      DIMENSION W(KM*JM*IM*4)              ! ����ΰ�
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                           ! P3PACK�ǻȤ�������
      SAVE

* P3PACK�ν����

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     ��°�ѿ�(���٥٥��ȥ��2��ʬ���б�)�ν����
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

      CALL BSSET0((2*NM+1)*(2*MM+1)*(2*LM+1)*2,Z)
      
      Z( 0, 0, 1, 1)= 1D0/4
      Z( 0, 0,-1, 1)= 1/(4*SQRT(3D0))
      Z( 0, 0, 1, 2)= -1/(2*SQRT(3D0))
      Z( 0, 0,-1, 2)= 0
      
      Z( 0, 1, 0, 1)= 1D0/4
      Z( 0,-1, 0, 1)= 1/(4*SQRT(3D0))
      Z( 0, 1, 0, 2)= -1/(2*SQRT(3D0))
      Z( 0,-1, 0, 2)= 0
      
      Z( 1, 0, 0, 1)= 1D0/4
      Z(-1, 0, 0, 1)= 1/(4*SQRT(3D0))
      Z( 1, 0, 0, 2)= -1/(2*SQRT(3D0))
      Z(-1, 0, 0, 2)= 0

      RETURN
*----------------------------------------------------------------------
*     ��°�ѿ�(���٥٥��ȥ��2��ʬ���б�)�λ����Ѳ�Ψ�η׻�
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P3ELNL(NM,MM,LM,KM,JM,IM,Z,DZ,WS,W,ITK,TK,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     ��¸�̤Υ����å�(���ͥ륮��(E)����ӥإꥷ�ƥ�(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CNSV(NM,MM,LM,Z,E,H)
      WRITE(6,'(A,F5.2,2(A,F17.15))')
     &  'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H

      END
