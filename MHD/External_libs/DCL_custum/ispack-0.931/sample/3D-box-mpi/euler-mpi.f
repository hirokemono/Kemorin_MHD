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
*     euler-mpi.f:
*              P3PACK-MPI��Ȥä�3������ȯ��Euler��������򤯥ץ����
*                                                  2002/05/07 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      

* �ѥ�᥿�������� 

      PARAMETER(NM=21,MM=21,LM=21) ! �����ȿ�
      PARAMETER(NPROC=1) ! ����¹Ի����Ѥ���ץ������ΤȤꤦ��Ǿ���
                         ! �⤷¿���Υץ������Ǥ����׻����ʤ��ʤ�
                         ! ����򤽤Υץ������˹�碌�뤳�Ȥˤ�ä�
                         ! ɬ�פʥ����̤�︺�Ǥ���.      
      PARAMETER(N=(2*MM+1)*(2*NM+1)*2*2*(LM/NPROC+1))
      DIMENSION Z(N)   ! ���٥٥��ȥ���Ǽ���뽾°�ѿ�
      DIMENSION W(N*3) ! ����ΰ�
      EXTERNAL SBGDZN

* MPI�ν�����ȥץ����ֹ�, �ץ������μ���.

      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.LT.NPROC) THEN
        WRITE(6,*) 'NPROC MUST .LE. NUMBER OF PROCESSES.'
        STOP
      END IF

* ���Υץ�����ʬ�����֤���륹�ڥ��ȥ�ǡ����ΤˤĤ���,
* L �θĿ�(LC), L �������ͤκǾ���(LS), L �������ͤκ�����(LE),
* ��������ǡ����� ND �����.

      LP=LM/NP+1
      LS=LP*IP
      LE=MIN(LP*(IP+1)-1,LM)
      IF(LE.GE.LS) THEN
        LC=LE-LS+1
      ELSE
        LC=0
        LS=0
        LE=0
      END IF

      IF(LS.EQ.0.AND.LC.NE.0) THEN
        ND=(2*LC-1)*(2*MM+1)*(2*NM+1)*2
      ELSE IF(LC.NE.0) THEN
        ND=(2*LC)*(2*MM+1)*(2*NM+1)*2
      ELSE
        ND=0
      END IF

* ������ʬ�Τ���Υѥ�᥿������

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
        CALL TDRK4U(ND,M,DT,T,Z,W,SBGDZN)
        CALL SBGDCK(T,Z)
      END DO

* MPI�ν�λ

      CALL MPI_FINALIZE(IERR)      

      END
***********************************************************************
*     ������ʬ����ɬ�פʼ�ν�����ޤȤ᤿���֥롼����ѥå�����
*----------------------------------------------------------------------
*     �ѥå������ν�����򤹤륵�֥롼����
*----------------------------------------------------------------------
      SUBROUTINE SBINIT

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'            
      PARAMETER(NM=21,MM=21,LM=21) ! main program��Ʊ���ͤ�Ϳ���뤳��.
      PARAMETER(KM=64,JM=64,IM=64) ! �ʻ����Υ�����
      PARAMETER(NPROC=1) ! ����¹Ի����Ѥ���ץ������ΤȤꤦ��Ǿ���
                         ! �⤷¿���Υץ������Ǥ����׻����ʤ��ʤ�
                         ! ����򤽤Υץ������˹�碌�뤳�Ȥˤ�ä�
                         ! ɬ�פʥ����̤�︺�Ǥ���.      
      PARAMETER(N=(2*MM+1)*(2*NM+1)*2*2*(LM/NPROC+1))
      DIMENSION Z(-NM:NM,-MM:MM,2,0:*)  ! ���٥٥��ȥ��2��ʬ
      DIMENSION DZ(-NM:NM,-MM:MM,2,0:*) ! Z�λ����Ѳ�Ψ
      DIMENSION W(KM*IM*4*((JM-1)/NPROC+1))   ! ����ΰ�
      ! JM=IM �ξ��Ϥ���Ǥ褤��, JM��IM �ξ��ϰۤʤ��礭����
      ! ���ʤ���Фʤ�ʤ����⤢��Τ���դ��뤳��.
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                           ! P3PACK�ǻȤ�������
      SAVE

* �ץ����ֹ�, �ץ������μ���.

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      IF(NP.LT.NPROC) THEN
        WRITE(6,*) 'NPROC MUST .LE. NUMBER OF PROCESSES.'
        STOP
      END IF

* ���Υץ�����ʬ�����֤���륹�ڥ��ȥ�ǡ����ΤˤĤ���,
* L �θĿ�(LC), L �������ͤκǾ���(LS), L �������ͤκ�����(LE),
* ��������ǡ����� ND �����.
* �ʤ�, LT �� L ��������Υǡ����򻲾Ȥ��뤿��˻Ȥ��ͤǤ���.

      LP=LM/NP+1
      LS=LP*IP
      LE=MIN(LP*(IP+1)-1,LM)
      IF(LE.GE.LS) THEN
        LC=LE-LS+1
      ELSE
        LC=0
        LS=0
        LE=0
      END IF

      LT=2*LC-1+LS

      IF(LS.EQ.0.AND.LC.NE.0) THEN
        ND=(2*LC-1)*(2*MM+1)*(2*NM+1)*2
      ELSE IF(LC.NE.0) THEN
        ND=(2*LC)*(2*MM+1)*(2*NM+1)*2
      ELSE
        ND=0
      END IF
      
* P3PACK�ν����

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     ��°�ѿ�(���٥٥��ȥ��2��ʬ���б�)�ν����
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

      CALL BSSET0(ND,Z)

      IF(LE.GE.1.AND.LS.LE.1) THEN
        Z( 0, 0, 1, 1-LS)= 1D0/4
        Z( 0, 0, 1, LT-1)= 1/(4*SQRT(3D0))
        Z( 0, 0, 2, 1-LS)= -1/(2*SQRT(3D0))
        Z( 0, 0, 2, LT-1)= 0
      END IF

      IF(IP.EQ.0) THEN
        Z( 0, 1, 1, 0)= 1D0/4
        Z( 0,-1, 1, 0)= 1/(4*SQRT(3D0))
        Z( 0, 1, 2, 0)= -1/(2*SQRT(3D0))
        Z( 0,-1, 2, 0)= 0
      
        Z( 1, 0, 1, 0)= 1D0/4
        Z(-1, 0, 1, 0)= 1/(4*SQRT(3D0))
        Z( 1, 0, 2, 0)= -1/(2*SQRT(3D0))
        Z(-1, 0, 2, 0)= 0
      END IF

      RETURN
*----------------------------------------------------------------------
*     ��°�ѿ�(���٥٥��ȥ��2��ʬ���б�)�λ����Ѳ�Ψ�η׻�
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P3EMNL(NM,MM,LM,KM,JM,IM,Z,DZ,W,ITK,TK,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     ��¸�̤Υ����å�(���ͥ륮��(E)����ӥإꥷ�ƥ�(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CMSV(NM,MM,LM,Z,E,H)
      IF(IP.EQ.0) THEN ! �ץ���0�ǤΤ߽���
        WRITE(6,'(A,F5.2,2(A,F17.15))')
     &    'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H
      END IF

      END
