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
*     baro.f: c2pack��Ȥä�2������ȯ��������������򤯥ץ����
*                                                  2000/10/24 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* �ѥ�᥿�������� 

      PARAMETER(LM=42,KM=42)    !�����ȿ�������
      PARAMETER(N=LM*(2*KM+1))  !���ڥ��ȥ��ѿ���������礭��
      DIMENSION Z(N)            !���٤Υ��ڥ��ȥ뷸��������
      DIMENSION W(N,3)          !Runge-Kuttaˡ�Τ���κ���ΰ�
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kuttaˡ���ƤӽФ����֥롼����

      NSTEP=200                 !����ȯŸ���륹�ƥå׿�
      H=1D0                     !����ե��å���ɽ���Τ���λ��ֳִ�
      M=5                       !Runge-Kutta�ǤΥ��ƥå�ʬ���

      NV=5                      !�ⳬǴ����Υ�ץ饷����γ���
      DNU=1D-17                 !�ⳬǴ������
      DELTAT=H/M                !Runge-Kutta�����λ��ֹ��

* �����

      CALL SBINIT(NV,DNU,DELTAT)
      CALL SBINIZ(Z)

* ����ȯŸ����ӥ���ե��å���ɽ��

      T=0                       !T�ϻ����ɽ���ѿ�
      CALL SBGRPH(T,Z)

      DO ISTEP=1,NSTEP
        CALL TDRKNU(N,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGRPH(T,Z)
      END DO

* ��λ����

      CALL SBCLOS

      END
***********************************************************************
*     ������ʬ����ɬ�פʼ�ν�����ޤȤ᤿���֥롼����ѥå�����
*----------------------------------------------------------------------
*     �ѥå������ν�����򤹤륵�֥롼����
*----------------------------------------------------------------------
      SUBROUTINE SBINIT(NV,DNU,DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CTIME*5
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(JM=64,IM=128)    !�ʻ�����������(JM: y����, IM: x����)
      PARAMETER(LM=42,KM=42)     !�����ȿ�������
                                 !(�ᥤ��ץ����˹�碌�뤳��)
      DIMENSION Z(-KM:KM,LM)     !���٤Υ��ڥ��ȥ뷸��������
      DIMENSION DZ(-KM:KM,LM)    !��������Υ��ڥ��ȥ뷸��������
      DIMENSION WS(-KM:KM,0:LM)  !����ΰ�
      DIMENSION WG(0:JM,0:IM-1,3) !����ΰ�
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)
                                !C2PACK�ǻȤ�������
      DIMENSION DL(-KM:KM,LM)   !Ǵ�����̤Τ���˻Ȥ�������
                                
      REAL RG(0:IM,0:JM)        !����ե��å����˻Ȥ�������
      REAL DLEV,TLEV1,TLEV2     !����ե��å����˻Ȥ����ѿ�
      SAVE

* Ϳ����줿��������, SBGDZN�ǻȤ�������DL(Ǵ���ˤ�븺����̤�Ϳ����)
* �����ꤹ��. �ʤ�, SBGDZN��Runge-Kutta�������ǸƤӽФ���뤬, ���κ�,
* ���DT=DELTAT/2�ǸƤӽФ���뤫��, ������θ��������, DL�ϰʲ��Τ�
* ��������Ǥ���.

      DO L=1,LM
        DO K=-KM,KM
          DL(K,L)=EXP(-DNU*DELTAT/2*(1D0*(K*K+L*L))**NV)
        END DO
      END DO

* C2PACK�ν����

      CALL C2INIT(JM,IM,ITJ,TJ,ITI,TI)

* ����ե��å����ν����

      CALL SWISET('IWIDTH', 500)
      CALL SWISET('IHEIGHT',300)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL SLRAT(1.0,0.6)
      CALL UDGCLA(0.0,1.0,0.1)

      IPAT0=25
      DLEV=0.05
      NLEV=20
      IDLEV=3

      DO I=1,NLEV
        IPAT=(IPAT0+I*IDLEV)*1000+999
        IF(I.EQ.1) THEN
          TLEV1=999.0
        ELSE
          TLEV1=(I-1)*DLEV
        END IF
        IF(I.EQ.NLEV) THEN
          TLEV2=999.0
        ELSE
          TLEV2=I*DLEV
        END IF
        CALL UESTLV(TLEV1,TLEV2,IPAT)
      END DO

      RETURN
*----------------------------------------------------------------------
*     ���٤Υ��ڥ��ȥ뷸��(Z)�ν����
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

* �����Ǥ�, �԰���ʱ��ؤ����������ä������Ϳ��, �����C2G2SA��
* �Ȥäƥ��ڥ��ȥ뷸�����Ѵ�����, �����Z�ν���ͤȤ��Ƥ���.

      X1=PI
      Y1=PI/2
      SIGMA=0.1D0
      AMP=1D-2

      DO I=0,IM-1
        X=2*PI*I/IM
        DO J=0,JM
          Y=PI*J/JM
          WG(J,I,1)=SIN(Y)**41D0
     &      + AMP*EXP(-((X-X1)**2+(Y-Y1)**2)/(2*SIGMA**2))
        END DO
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG,Z,WG(0,0,3),ITJ,TJ,ITI,TI,1)

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӱ��٤Υ��ڥ��ȥ뷸��(Z)�����ϤȤ���, ��������Υ���
*     ���ȥ뷸��������(DZ)����륵�֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL C2AJBS(LM,KM,JM,IM,1D0,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӻ��ֹ��(DT)�����ϤȤ���, ����(Ǵ��)��ˤ��Z��
*     T��T+DT�ˤ�����ȯŸ��򤯥��֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO L=1,LM
        DO K=-KM,KM
          Z(K,L)=DL(K,L)*Z(K,L)
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     Z���б����뱲�پ�򥰥�ե��å���ɽ������.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,Z)

      WRITE(CTIME,'(F5.1)') T

      CALL C2S2GA(LM,KM,JM,IM,Z,WG,WG(0,0,3),ITJ,TJ,ITI,TI,1)

      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)
        END DO
      END DO

      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.10,0.50)
      CALL SGSWND(0.0,2.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UETONE(RG,IM+1,IM+1,JM+1)
      CALL USDAXS
      CALL UXSTTL('T','T='//CTIME,1.0)

      RETURN
*----------------------------------------------------------------------
*     ��λ����
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
