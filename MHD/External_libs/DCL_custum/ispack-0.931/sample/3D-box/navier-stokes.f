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
*    P3PACK��Ȥä�3���� Navier-Stokes ��������򤯥ץ����
*                                                  2002/03/16 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* �ѥ�᥿�������� 

      PARAMETER(NM=10,MM=10,LM=10) ! �����ȿ�     
      PARAMETER(N=(2*LM+1)*(2*MM+1)*(2*NM+1)*2)
      PARAMETER(IU=10,NB=1024)
      DIMENSION Z(N)    ! ���٥٥��ȥ���Ǽ���뽾°�ѿ�
      DIMENSION W(N,3)  ! ����ΰ�
      EXTERNAL SBGDZN,SBGDZL ! �����Ѳ�����륵�֥롼����

      NSTEP=10                 !����ȯŸ���륹�ƥå׿�
      H=1D0                    !�ե�������ϤΤ���λ��ֳִ�
      M=20                     !Runge-Kutta�ǤΥ��ƥå�ʬ���
      DELTAT=H/M               !Runge-Kutta�����λ��ֹ��
      DNU=1D-2                 !ưǴ������

* �����

      CALL FHUOPN(IU,'test.dat','W',NB)
      CALL SBINIT(DNU,DELTAT)
      CALL SBINIZ(Z)

* ����ȯŸ����ӥե��������

      T=0                       !T�ϻ����ɽ���ѿ�
      CALL SBGDCK(T,Z)
      CALL FEPUTD(IU,N,Z)
      
      DO ISTEP=1,NSTEP
        CALL TDRKNU(N,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGDCK(T,Z)
        CALL FEPUTD(IU,N,Z)        
      END DO

      CALL FHUCLS(IU)      

      END
***********************************************************************
*     ������ʬ����ɬ�פʼ�ν�����ޤȤ᤿���֥롼����ѥå�����
*----------------------------------------------------------------------
*     �ѥå������ν�����򤹤륵�֥롼����
*----------------------------------------------------------------------
      SUBROUTINE SBINIT(DNU,DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NM=10,MM=10,LM=10) ! main program��Ʊ���ͤ�Ϳ���뤳��.
      PARAMETER(KM=32,JM=32,IM=32) ! �ʻ����Υ�����
      PARAMETER(IFLAG=1) ! ������Ϳ���뤫�ɤ����Υե饰
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)  ! ���٥٥��ȥ��2��ʬ
      DIMENSION DZ(-NM:NM,-MM:MM,-LM:LM,2) ! Z�λ����Ѳ�Ψ
      DIMENSION DL(-NM:NM,-MM:MM,-LM:LM,2) !Ǵ����θ��̤�ɽ������
      DIMENSION WS(-NM:NM,-MM:MM,-LM:LM) !����ΰ�
      DIMENSION W(KM*JM*IM*4)              !����ΰ�
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
                                           ! P3PACK�ǻȤ�������      
      SAVE

* Ǵ����θ��̤�ɽ������ν����

      DO L=-LM,LM
        DO M=-MM,MM
          DO N=-NM,NM
            DL(N,M,L,1)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))
            DL(N,M,L,2)=EXP(-DNU*DELTAT/2*(L*L+M*M+N*N))            
          END DO
        END DO
      END DO

      IF(IFLAG.EQ.1) THEN
        DO IV=1,2
          DO I=-1,1,2
            DL(0,0,I,IV)=1
            DL(0,I,0,IV)=1
            DL(I,0,0,IV)=1
          END DO
        END DO
      END IF

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
*     ��������ˤ������Ѳ�Ψ�η׻�
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P3ELNL(NM,MM,LM,KM,JM,IM,Z,DZ,WS,W,ITK,TK,ITJ,TJ,ITI,TI)

      IF(IFLAG.EQ.1) THEN
        DO IV=1,2
          DO I=-1,1,2
            DZ(0,0,I,IV)=0
            DZ(0,I,0,IV)=0
            DZ(I,0,0,IV)=0
          END DO
        END DO
      END IF

      RETURN
*----------------------------------------------------------------------
*     Ǵ����ˤ��ȯŸ��򤯥��֥롼����
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO L=-LM,LM
        DO M=-MM,MM
          DO N=-NM,NM
            Z(N,M,L,1)=DL(N,M,L,1)*Z(N,M,L,1)
            Z(N,M,L,2)=DL(N,M,L,2)*Z(N,M,L,2)
          END DO
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     ��¸�̤Υ����å�(���ͥ륮��(E)����ӥإꥷ�ƥ�(H))
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,Z)

      CALL P3CNSV(NM,MM,LM,Z,E,H)
      WRITE(6,'(A,F5.2,2(A,F17.15))')
     &  'TIME = ',T,'  ENERGY = ',E,'  HELICITY = ',H

      END
