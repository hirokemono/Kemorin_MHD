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
*     model2d-p2pack.f:
*               p2pack��Ȥä�2������ȯ��������������򤯥ץ����
*
*                                                  2001/07/23 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* �ѥ�᥿�������� 

*      PARAMETER(LM=63,KM=63)    !�����ȿ�������(aliasing��ȤäƤ��ʤ�)
      PARAMETER(LM=42,KM=42)    !�����ȿ�������(aliasing��Ȥ���)
      PARAMETER(N=(2*LM+1)*(2*KM+1)) !���ڥ��ȥ��ѿ���������礭��
      DIMENSION Z(N)            !���٤Υ��ڥ��ȥ뷸��������
      DIMENSION W(N,3)          !Runge-Kuttaˡ�Τ���κ���ΰ�
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kuttaˡ���ƤӽФ����֥롼����

      NSTEP=100                 !����ȯŸ���륹�ƥå׿�
      H=1D0                     !����ե��å���ɽ���Τ���λ��ֳִ�
      M=3                       !Runge-Kutta�ǤΥ��ƥå�ʬ���

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
      PARAMETER(JM=128,IM=128)    !�ʻ�����������(JM: y����, IM: x����)
*      PARAMETER(LM=63,KM=63)    !�����ȿ�������(aliasing��ȤäƤ��ʤ�)
      PARAMETER(LM=42,KM=42)    !�����ȿ�������(aliasing��Ȥ���)
      PARAMETER(R=1)    !x������y�����Υ����ڥ�����
      DIMENSION Z(-LM:LM,-KM:KM) !���٤Υ��ڥ��ȥ뷸��������
      DIMENSION DZ(-LM:LM,-KM:KM) !��������Υ��ڥ��ȥ뷸��������
      DIMENSION WS(-LM:LM,-KM:KM) !����ΰ�
      DIMENSION WG(0:JM-1,0:IM-1,3) !����ΰ�
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2) !P2PACK�ǻȤ�������
      DIMENSION DL(-LM:LM,-KM:KM) !Ǵ�����̤Τ���˻Ȥ�������
      REAL RG(0:IM,0:JM)        !����ե��å����˻Ȥ�������
      REAL DLEV,TLEV1,TLEV2     !����ե��å����˻Ȥ����ѿ�
      SAVE

* Ϳ����줿��������, SBGDZN�ǻȤ�������DL(Ǵ���ˤ�븺����̤�Ϳ����)
* �����ꤹ��. �ʤ�, SBGDZN��Runge-Kutta�������ǸƤӽФ���뤬, ���κ�,
* ���DT=DELTAT/2�ǸƤӽФ���뤫��, ������θ��������, DL�ϰʲ��Τ�
* ��������Ǥ���.

      DO K=-KM,KM
        DO L=-LM,LM
          DL(L,K)=EXP(-DNU*DELTAT/2*(1D0*(K*K+L*L))**NV)
        END DO
      END DO

* P2PACK�ν����

      CALL P2INIT(JM,IM,ITJ,TJ,ITI,TI)

* ����ե��å����ν����

      CALL SWISET('IWIDTH',  400)
      CALL SWISET('IHEIGHT', 400)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL SLRAT(1.0,1.0)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL SLRAT(1.0,1.0)
      CALL UDGCLA(0.0,1.0,0.1)

      IPAT0=25
      DLEV=0.05
      NLEV=20
      IDLEV=3

      CALL UESTLV(999.0,0.0,IPAT0*1000+999)
      DO I=1,NLEV-1
        IPAT=(IPAT0+I*IDLEV)*1000+999
        TLEV1=(I-1)*DLEV
        TLEV2=I*DLEV
        CALL UESTLV(TLEV1,TLEV2,IPAT)
      END DO
      IPAT=(IPAT0+NLEV*IDLEV)*1000+999
      CALL UESTLV(TLEV2,999.0,IPAT)

      RETURN
*----------------------------------------------------------------------
*     ���٤Υ��ڥ��ȥ뷸��(Z)�ν����
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

* �����Ǥ�, 2�ĤΥ��������󷿤α��������뱲��ʬ�ۤ�ʻ������Ϳ��, 
* �����P2G2SA��Ȥäƥ��ڥ��ȥ뷸�����Ѵ�����, �����Z�ν���ͤ�
* ���Ƥ���

      X1=0.4D0
      Y1=0.4D0
      X2=0.6D0
      Y2=0.6D0
      SIGMA=0.05D0

      DO I=0,IM-1
        X=1D0*I/IM
        DO J=0,JM-1
          Y=1D0*J/JM
          WG(J,I,1)= EXP(-((X-X1)**2+(Y-Y1)**2)/(2*SIGMA**2))
     &              +EXP(-((X-X2)**2+(Y-Y2)**2)/(2*SIGMA**2))
        END DO
      END DO

      CALL P2G2SA(LM,KM,JM,IM,WG,Z,WG(0,0,3),ITJ,TJ,ITI,TI)

      Z(0,0)=0                  !���٤�ʿ�Ѥ�0�Ǥʤ���Фʤ�ʤ�����

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӱ��٤Υ��ڥ��ȥ뷸��(Z)�����ϤȤ���, ��������Υ���
*     ���ȥ뷸��������(DZ)����륵�֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL P2AJBS(LM,KM,JM,IM,R,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӻ��ֹ��(DT)�����ϤȤ���, ����(Ǵ��)��ˤ��Z��
*     T��T+DT�ˤ�����ȯŸ��򤯥��֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      DO K=-KM,KM
        DO L=-LM,LM
          Z(L,K)=DL(L,K)*Z(L,K)
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     Z���б����뱲�پ�򥰥�ե��å���ɽ������.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,Z)

      WRITE(CTIME,'(F5.1)') T

      CALL P2S2GA(LM,KM,JM,IM,Z,WG,WG(0,0,3),ITJ,TJ,ITI,TI)

      DO I=0,IM-1
        DO J=0,JM-1
          RG(I,J)=WG(J,I,1)
        END DO
      END DO

      DO J=0,JM-1
        RG(IM,J)=RG(0,J)
      END DO
      DO I=0,IM-1
        RG(I,JM)=RG(I,0)
      END DO
      RG(IM,JM)=RG(0,0)

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(0.0,1.0,0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UETONE(RG,IM+1,IM+1,JM+1)
*      CALL UDCNTR(RG,IM+1,IM+1,JM+1)
      CALL USDAXS
      CALL UXSTTL('T','T='//CTIME,1.0)
*      CALL GRFRM

      RETURN
*----------------------------------------------------------------------
*     ��λ����
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
