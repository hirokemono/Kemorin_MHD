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
*    navier-stokes.f:
*    P3PACK-MPI��Ȥä�3���� Navier-Stokes ��������򤯥ץ����
*                                                  2002/05/07 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'      

* �ѥ�᥿�������� 

      PARAMETER(NM=10,MM=10,LM=10) ! �����ȿ�
      PARAMETER(NPROC=1) ! ����¹Ի����Ѥ���ץ������ΤȤꤦ��Ǿ���
                         ! �⤷¿���Υץ������Ǥ����׻����ʤ��ʤ�
                         ! ����򤽤Υץ������˹�碌�뤳�Ȥˤ�ä�
                         ! ɬ�פʥ����̤�︺�Ǥ���.      
      PARAMETER(N=(2*MM+1)*(2*NM+1)*2*2*(LM/NPROC+1))
      DIMENSION Z(N)   ! ���٥٥��ȥ���Ǽ���뽾°�ѿ�
      DIMENSION W(N*3) ! ����ΰ�
      EXTERNAL SBGDZN,SBGDZL

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

* ����ȯŸ�������¸�̤Υ����å�

      NSTEP=10                 !����ȯŸ���륹�ƥå׿�
      H=1D0                    !��¸�̤Υ����å��Τ���λ��ֳִ�      
      M=20                     !Runge-Kutta�ǤΥ��ƥå�ʬ���
      DELTAT=H/M               !Runge-Kutta�����λ��ֹ��
      DNU=1D-2                 !ưǴ������
      
* �ѥå������Ƚ�°�ѿ��ν����

      CALL SBINIT(DNU,DELTAT)
      CALL SBINIZ(Z)

* ����ȯŸ�������¸�̤Υ����å�

      T=0                       !T�ϻ����ɽ���ѿ�
      CALL SBGDCK(T,Z)
      CALL SBPUTD(Z)
      DO ISTEP=1,NSTEP
        CALL TDRKNU(ND,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGDCK(T,Z)
        CALL SBPUTD(Z)
      END DO

* �ѥå������ν�λ

      CALL SBCLOS

* MPI�ν�λ      

      CALL MPI_FINALIZE(IERR)      

      END
***********************************************************************
*     ������ʬ����ɬ�פʼ�ν�����ޤȤ᤿���֥롼����ѥå�����
*----------------------------------------------------------------------
*     �ѥå������ν�����򤹤륵�֥롼����
*----------------------------------------------------------------------
      SUBROUTINE SBINIT(DNU,DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'            
      PARAMETER(NM=10,MM=10,LM=10) ! main program��Ʊ���ͤ�Ϳ���뤳��.
      PARAMETER(KM=32,JM=32,IM=32) ! �ʻ����Υ�����
      PARAMETER(IFLAG=1) ! ������Ϳ���뤫�ɤ����Υե饰
      PARAMETER(IU=10,NB=1024) ! ���ϥե������ֹ椪��ӥХåե�Ĺ
      PARAMETER(NPROC=1) ! ����¹Ի����Ѥ���ץ������ΤȤꤦ��Ǿ���
                         ! �⤷¿���Υץ������Ǥ����׻����ʤ��ʤ�
                         ! ����򤽤Υץ������˹�碌�뤳�Ȥˤ�ä�
                         ! ɬ�פʥ����̤�︺�Ǥ���.      
      DIMENSION Z(-NM:NM,-MM:MM,2,0:*)  ! ���٥٥��ȥ��2��ʬ
      DIMENSION DZ(-NM:NM,-MM:MM,2,0:*) ! Z�λ����Ѳ�Ψ
      DIMENSION DL(-NM:NM,-MM:MM,2,0:LM/NPROC*2+1)
                         ! Ǵ���θ��̤��Ǽ��������
      DIMENSION W(KM*IM*4*((JM-1)/NPROC+1)) ! ����ΰ�
      ! JM=IM �ξ��Ϥ���Ǥ褤��, JM��IM �ξ��ϰۤʤ��礭����
      ! ���ʤ���Фʤ�ʤ����⤢��Τ���դ��뤳��.
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                       ! P3PACK�ǻȤ�������
      DIMENSION ISTAT(MPI_STATUS_SIZE) ! MPI_SEND,MPI_RECV�ǻȤ�������
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
* �ޤ�, L2 �������ޤ᤿ L �ο��ι�� -1 ���ͤǤ���.

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

      IF(LS.EQ.0.AND.LC.NE.0) THEN
        L2=2*LC-2
      ELSE IF(LC.NE.0) THEN
        L2=2*LC-1
      ELSE
        L2=-1
      END IF

      IF(LC.GT.0) THEN
        DO L=LS,LE
          DO M=-MM,MM
            DO N=-NM,NM
              DL(N,M,1,L-LS)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
              DL(N,M,2,L-LS)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))            
              DL(N,M,1,LT-L)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
              DL(N,M,2,LT-L)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
            END DO
          END DO
        END DO
      END IF

      IF(IFLAG.EQ.1) THEN
        IF(LE.GE.1.AND.LS.LE.1) THEN
          DO IV=1,2
            DL(0,0,IV,1-LS)=1
            DL(0,0,IV,LT-1)=1            
          END DO
        END IF
        IF(IP.EQ.0) THEN      
          DO IV=1,2
            DO I=-1,1,2
              DL(0,I,IV,0)=1
              DL(I,0,IV,0)=1
            END DO
          END DO
        END IF
      END IF

* P3PACK�ν����

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

* FHPACK�ν����

      IF(IP.EQ.0) THEN
        CALL FHUOPN(IU,'test.dat','W',NB)
      END IF

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

      IF(IFLAG.EQ.1) THEN
        IF(LE.GE.1.AND.LS.LE.1) THEN
          DO IV=1,2
            DZ(0,0,IV,1-LS)=0
            DZ(0,0,IV,LT-1)=0            
          END DO
        END IF
        IF(IP.EQ.0) THEN      
          DO IV=1,2
            DO I=-1,1,2
              DZ(0,I,IV,0)=0
              DZ(I,0,IV,0)=0
            END DO
          END DO
        END IF
      END IF

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӻ��ֹ��(DT)�����ϤȤ���, ����(Ǵ��)��ˤ��Z��
*     T��T+DT�ˤ�����ȯŸ��򤯥��֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO L=0,L2
        DO M=-MM,MM
          DO N=-NM,NM
            Z(N,M,1,L)=DL(N,M,1,L)*Z(N,M,1,L)
            Z(N,M,2,L)=DL(N,M,2,L)*Z(N,M,2,L)
          END DO
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     �ǡ������̾����֥⡼�ɤؤν��󤪤�ӥե�����ؤν���
*     (�ץ���0�˽���, �ץ���0�ǥե�����˽���)
*----------------------------------------------------------------------
      ENTRY SBPUTD(Z)

      DO L=-LM,-LM/NP-1
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,1,LT+L),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,LM+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,LM+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      IF(IP.EQ.0) THEN
        DO L=-LM/NP,-1
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,1,LT+L))
        END DO
        DO L=0,LM/NP
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,1,L))
        END DO
      END IF

      DO L=LM/NP+1,LM
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,1,L-LS),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,LM+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,LM+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      DO L=-LM,-LM/NP-1
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,2,LT+L),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,3*LM+1+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,3*LM+1+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      IF(IP.EQ.0) THEN
        DO L=-LM/NP,-1
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,2,LT+L))
        END DO
        DO L=0,LM/NP
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),Z(-NM,-MM,2,L))
        END DO
      END IF

      DO L=LM/NP+1,LM
        IPD=ABS(L)/LP
        IF(IP.EQ.IPD) THEN
          CALL MPI_SEND(Z(-NM,-MM,2,L-LS),(2*NM+1)*(2*MM+1),
     &      MPI_REAL8,0,3*LM+1+L,MPI_COMM_WORLD,IERR)
        END IF
        IF(IP.EQ.0) THEN
          CALL MPI_RECV(W,(2*NM+1)*(2*MM+1),MPI_REAL8,IPD,3*LM+1+L,
     &      MPI_COMM_WORLD,ISTAT,IERR)
          CALL FEPUTD(IU,(2*NM+1)*(2*MM+1),W)
        END IF
      END DO

      RETURN
*----------------------------------------------------------------------
*     ��¸�̤Υ����å�(���ͥ륮��(E)����ӥإꥷ�ƥ�(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CMSV(NM,MM,LM,Z,E,H)
      IF(IP.EQ.0) THEN
        WRITE(6,'(A,F5.2,2(A,F17.15))')
     &    'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H
      END IF

      RETURN
*----------------------------------------------------------------------
*     �ѥå������ν�λ�����򤹤륵�֥롼����
*----------------------------------------------------------------------
      ENTRY SBCLOS

      IF(IP.EQ.0) THEN
        CALL FHUCLS(IU)
      END IF

      END
