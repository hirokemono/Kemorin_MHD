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
*   model2d-beta.f: n2pack��Ȥä�2������ȯ��������������򤯥ץ����
*
*                                                 2000/02/14 K.Ishioka
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
      H=0.1D0                   !����ե��å���ɽ���Τ���λ��ֳִ�
      M=5                       !Runge-Kutta�ǤΥ��ƥå�ʬ���

      NV=5                      !�ⳬǴ����Υ�ץ饷����γ���
      DNU=1D-15                 !�ⳬǴ������
      DELTAT=H/M                !Runge-Kutta�����λ��ֹ��

* �����

      CALL SBINIT(NV,DNU,DELTAT)
      CALL SBINIZ(Z)

* ����ȯŸ����ӥ���ե��å���ɽ��

      T=0                       !T�ϻ����ɽ���ѿ�
      CALL SBGRPH(T,Z)
*      CALL SBSPEC(T,Z)

      DO ISTEP=1,NSTEP
        CALL TDRKNU(N,M,H,T,Z,W,SBGDZL,SBGDZN)
        CALL SBGRPH(T,Z) 
*        CALL SBSPEC(T,Z)
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
*      PARAMETER(LM=63,KM=63)   !�����ȿ�������(aliasing��ȤäƤ��ʤ�)
      PARAMETER(LM=42,KM=42)    !�����ȿ�������(aliasing��Ȥ���)
      PARAMETER(K0=7,GAMMA=18) !�����Υ��ͥ륮�����ڥ��ȥ��������
      PARAMETER(BETA=200)        ! �¸���
      DIMENSION Z(-LM:LM,-KM:KM) !���٤Υ��ڥ��ȥ뷸��������
      DIMENSION DZ(-LM:LM,-KM:KM) !��������Υ��ڥ��ȥ뷸��������
      DIMENSION WS(-LM:LM,-KM:KM) !����ΰ�
      DIMENSION WG(0:JM-1,0:IM-1,3) !����ΰ�
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2) !N2PACK�ǻȤ�������
      DIMENSION DL(-LM:LM,-KM:KM) !Ǵ�����̤Τ���˻Ȥ�������
      DIMENSION DBS(-LM:LM,KM),DBC(-LM:LM,KM)
                                 !�¸��̤Τ���˻Ȥ�������
      DIMENSION ENE(KM+LM),DIST(KM+LM)      !���������˻Ȥ�������
      REAL RG(0:IM,0:JM)        !����ե��å����˻Ȥ�������
      REAL DLEV,TLEV1,TLEV2     !����ե��å����˻Ȥ����ѿ�
      REAL RENE(KM),RK(KM)
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

* Ϳ����줿��������, SBGDZN�ǻȤ�������
*      DBS, DBC(�¸��̤ˤ���������ʸ��̤�Ϳ����)
* �����ꤹ��. �ʤ�, SBGDZN��Runge-Kutta�������ǸƤӽФ���뤬, ���κ�,
* ���DT=DELTAT/2�ǸƤӽФ���뤫��, ������θ��������, DBS,DBC
* �ϰʲ��Τ褦������Ǥ���.

      DO K=1,KM
        DO L=-LM,LM
          ALPHA=BETA*DELTAT/2*K/(K*K+L*L)
          DBC(L,K)=COS(ALPHA)
          DBS(L,K)=SIN(ALPHA)
        END DO
      END DO

* N2PACK�ν����

      CALL N2INIT(JM,IM,ITJ,TJ,ITI,TI)

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

      IPAT0=55
      DLEV=3
      IDLEV=3
      NLEV=10

      IPAT=(IPAT0-NLEV*IDLEV)*1000+999
      TLEV2=(-NLEV+0.5D0)*DLEV
      CALL UESTLV(-999.0,TLEV2,IPAT)
      
      IPAT=(IPAT0+NLEV*IDLEV)*1000+999
      TLEV1=(NLEV+0.5D0)*DLEV
      CALL UESTLV(TLEV1,999.0,IPAT)
      
      DO I=-NLEV,NLEV
        IPAT=(IPAT0+I*IDLEV)*1000+999
        TLEV1=(I-0.5D0)*DLEV
        TLEV2=(I+0.5D0)*DLEV
        CALL UESTLV(TLEV1,TLEV2,IPAT)
      END DO

      RETURN
*----------------------------------------------------------------------
*     ���٤Υ��ڥ��ȥ뷸��(Z)�ν����
*----------------------------------------------------------------------
      ENTRY SBINIZ(Z)

* |K|=k0 ���濴�Ȥ����ȿ��˥��ͥ륮���ԡ�����Ϳ����.
* Ʊ��|K|���Ф����������������Ȥäƥ������Ϳ����.

      CALL BSSET0(KM+LM,DIST)
      SDIST=0
      DO K=1,KM
        DIST(K)=(1D0*K)**(GAMMA/2)/(1D0*K+K0)**GAMMA
        SDIST=SDIST+DIST(K)
      END DO
      DO K=1,KM
        DIST(K)=DIST(K)/SDIST
      END DO

      ISEED=0 !����μ�
      DO K=-KM,KM
        DO L=-LM,LM
          CALL ISNORM(ISEED,Z(L,K))
        END DO
      END DO

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+Z(L,K)*Z(L,K)/(K*K+L*L)
          END IF
        END DO
      END DO

      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            Z(L,K)=Z(L,K)/SQRT(ENE(KT)/DIST(KT))
          END IF
        END DO
      END DO

      Z(0,0)=0                  !���٤�ʿ�Ѥ�0�Ǥʤ���Фʤ�ʤ�����

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӱ��٤Υ��ڥ��ȥ뷸��(Z)�����ϤȤ���, ��������Υ���
*     ���ȥ뷸��������(DZ)����륵�֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL N2AJBS(LM,KM,JM,IM,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

*     �⤷�¸��̤���������η׻��Ȱ��˲ä�����ˤϰʲ��Τ褦�ˤ���.
*      (���ξ���, SBGDZL �Φ¸��̤���ʬ�򳰤�����)
*      
*      DO K=-KM,KM
*        DO L=-LM,LM
*          IF(K.NE.0.OR.L.NE.0) THEN
*            DZ(L,K)=DZ(L,K)-BETA*Z(-L,-K)*K/(K*K+L*L)
*          END IF
*        END DO
*      END DO

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӻ��ֹ��(DT)�����ϤȤ���, ����(Ǵ��)��ˤ��Z��
*     T��T+DT�ˤ�����ȯŸ��򤯥��֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

* �¸��̤���ʬ      

      DO K=1,KM
        DO L=-LM,LM
          TMP=DBS(L,K)*Z(L,K)+DBC(L,K)*Z(-L,-K)
          Z(L,K)=DBC(L,K)*Z(L,K)-DBS(L,K)*Z(-L,-K)
          Z(-L,-K)=TMP
        END DO
      END DO

* �ⳬǴ�����̤���ʬ      

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

      CALL N2S2GA(LM,KM,JM,IM,Z,WG,WG(0,0,3),ITJ,TJ,ITI,TI)

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
      CALL USDAXS
      CALL UXSTTL('T','T='//CTIME,1.0)

      RETURN
*----------------------------------------------------------------------
*     Z���б����륨�ͥ륮�����ڥ��ȥ�򥰥�ե��å���ɽ������.
*----------------------------------------------------------------------
      ENTRY SBSPEC(T,Z)

      WRITE(CTIME,'(F5.1)') T

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+Z(L,K)*Z(L,K)/(K*K+L*L)
          END IF
        END DO
      END DO

      ENET=0
      DO K=1,KM+LM
        ENET=ENET+ENE(K)
      END DO

      DO K=1,KM
        RK(K)=K
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-6,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      
      CALL UXSTTL('T','T='//CTIME,1.0)

      print *,'TIME=',T,' TOTAL ENERGY=',ENET

      RETURN
*----------------------------------------------------------------------
*     ��λ����
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
********************************************************************
      SUBROUTINE ISNORM(ISEED,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DATA IFLAG/0/
      SAVE

      IF(IFLAG.EQ.0) THEN
        CALL ISRAND(ISEED,X1)
        CALL ISRAND(ISEED,X2)
        Y1=SQRT(-2*LOG(X1))*COS(2*PI*X2)
        Y2=SQRT(-2*LOG(X1))*SIN(2*PI*X2)
        R=Y1
        IFLAG=1
      ELSE
        R=Y2
        IFLAG=0
      END IF

      END
********************************************************************
      SUBROUTINE ISRAND(ISEED,R)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(IA=1664525,IC=1013904223,AM=2D0**32)

      ISEED=IA*ISEED+IC
      R=(IBCLR(ISEED,31)+0.5D0)/AM

      IF(BTEST(ISEED,31)) THEN
        R=R+0.5D0
      END IF

      END
