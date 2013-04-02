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
*   shallow2d.f: p2pack��Ȥä�fʿ��������������򤯥ץ����
*
*                                                 2001/07/23 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* �ѥ�᥿�������� 

      PARAMETER(LM=42,KM=42)    !�����ȿ�������
      PARAMETER(N=(2*LM+1)*(2*KM+1)) !���ڥ��ȥ��ѿ���������礭��
      DIMENSION VAR(N,3)        !�ѿ��Υ��ڥ��ȥ뷸��������
      DIMENSION W(N*3,3)        !Runge-Kuttaˡ�Τ���κ���ΰ�
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kuttaˡ���ƤӽФ����֥롼����

      NSTEP=200                 !����ȯŸ���륹�ƥå׿�
*      H=1D0                     !����ե��å���ɽ���Τ���λ��ֳִ�      
*      M=3                       !Runge-Kutta�ǤΥ��ƥå�ʬ���      
      H=0.1D0                   !����ե��å���ɽ���Τ���λ��ֳִ�
      M=5                       !Runge-Kutta�ǤΥ��ƥå�ʬ���
      
      NV=5                      !�ⳬǴ����Υ�ץ饷����γ���
      DNU=1D-14                 !�ⳬǴ������
      DELTAT=H/M                !Runge-Kutta�����λ��ֹ��

* �����

      CALL SBINIT(NV,DNU,DELTAT)
      
*      CALL SBINIA(VAR) ! 2�Ĥθ�Ω���ν����(H=1, M=3���٤�Ŭ��)
*                       ! BARPHI=0.05D0, F=0 �ʤɤȤ��Ƥ���������.
      CALL SBINIB(VAR) ! ��ήŪ�����(H=0.1D0, M=5���٤�Ŭ��)
                       ! BARPHI=15D0, F=10 �ʤɤȤ��Ƥ���������.
      
* ����ȯŸ����ӥ���ե��å���ɽ��

      T=0                       !T�ϻ����ɽ���ѿ�
      CALL SBGDCK(T,VAR)      
      CALL SBGRPH(T,VAR)
      CALL SBSPEC(T,VAR)

      CALL APTIME(TIM1)
      DO ISTEP=1,NSTEP
        CALL TDRKNU(N*3,M,H,T,VAR,W,SBGDZL,SBGDZN)
        CALL SBGRPH(T,VAR)        
        CALL SBGDCK(T,VAR)
        CALL SBSPEC(T,VAR)
      END DO
      CALL APTIME(TIM2)
      print *,'TIME=',TIM2-TIM1

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
      PARAMETER(JM=128,IM=128)  !�ʻ�����������(JM: y����, IM: x����)
      PARAMETER(LM=42,KM=42)    !�����ȿ�������
      PARAMETER(K0=7,GAMMA=18)  !�����Υ��ͥ륮�����ڥ��ȥ��������
*      PARAMETER(F=0)            ! �Ϥβ�ž����(F)                  
*      PARAMETER(BARPHI=0.05D0)  ! ʿ�ѿ忼      
      PARAMETER(F=10)           ! �Ϥβ�ž����(F)
      PARAMETER(R=1)    !x������y�����Υ����ڥ�����      
      PARAMETER(BARPHI=15D0)    ! ʿ�ѿ忼
      
      PARAMETER(ISPLIT=1)       ! �����Ȥ�ʬΥ���ư������ɤ����Υե饰
      DIMENSION VAR(-LM:LM,-KM:KM,3)  !���٤Υ��ڥ��ȥ뷸��������
      DIMENSION DVAR(-LM:LM,-KM:KM,3) !��������Υ��ڥ��ȥ뷸��������
      DIMENSION WS(-LM:LM,-KM:KM)   !����ΰ�
      DIMENSION WG(0:JM-1,0:IM-1,4)   !����ΰ�
      DIMENSION ITJ(5),TJ(JM*2),ITI(5),TI(IM*2) !P2PACK�ǻȤ�������
      DIMENSION DL(-LM:LM,-KM:KM)     !Ǵ�����̤Τ���˻Ȥ�������
      DIMENSION DB(-LM:LM,-KM:KM,3,3)
                                      !�������ȯŸ�Τ���˻Ȥ�������
      DIMENSION ENE(KM+LM),DIST(KM+LM)      !���������˻Ȥ�������

      PARAMETER(NBR=(((JM+3)*(IM+3)+15)/16+1)*3) !UDCNTZ�Τ�������
      REAL RG(0:IM,0:JM),IBR(NBR)   !����ե��å����˻Ȥ�������
      REAL TLEV1,TLEV2         !����ե��å����˻Ȥ����ѿ�
      REAL RENE(KM),RK(KM)          !����ե��å����˻Ȥ�������
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

* Ϳ����줿��������, SBGDZN�ǻȤ������� DB(�����Ȥΰ����Ѳ���Ϳ����)
* �����ꤹ��. �ʤ�, SBGDZN��Runge-Kutta�������ǸƤӽФ���뤬, ���κ�,
* ���DT=DELTAT/2�ǸƤӽФ���뤫��, ������θ��������, DB �ϰʲ���
* �褦������Ǥ���.

      DO K=0,KM
        DO L=0,LM
          K2=K*K+L*L
          SIGMA2=BARPHI*K2+F*F
          IF(SIGMA2.EQ.0) SIGMA2=1
          SIGMA=SQRT(SIGMA2)
          FRAC=1/SIGMA2
          ALPHA=(DELTAT/2)*SIGMA
          CA=COS(ALPHA)
          SA=SIN(ALPHA)
          DB(L,K,1,1)=FRAC*(K2*BARPHI+F*F*CA)
          DB(L,K,1,2)=FRAC*(-SIGMA*F*SA)
          DB(L,K,1,3)=FRAC*(-F*K2+F*K2*CA)
          DB(L,K,2,1)=FRAC*(SIGMA*F*SA)
          DB(L,K,2,2)=FRAC*(SIGMA2*CA)
          DB(L,K,2,3)=FRAC*(SIGMA*K2*SA)
          DB(L,K,3,1)=FRAC*(-F*BARPHI+F*BARPHI*CA)
          DB(L,K,3,2)=FRAC*(-SIGMA*BARPHI*SA)
          DB(L,K,3,3)=FRAC*(F*F+K2*BARPHI*CA)
        END DO
      END DO

      DO IV1=1,3
        DO IV2=1,3
          DB(0,0,IV1,IV2)=0
        END DO
        DB(0,0,IV1,IV1)=1
      END DO

      DO IV1=1,3
        DO IV2=1,3
          DO K=0,KM
            DO L=0,LM
              DB(-L,K,IV1,IV2)=DB(L,K,IV1,IV2)
              DB(-L,-K,IV1,IV2)=DB(L,K,IV1,IV2)
              DB(L,-K,IV1,IV2)=DB(L,K,IV1,IV2)
            END DO
          END DO
        END DO
      END DO

* P2PACK�ν����

      CALL P2INIT(JM,IM,ITJ,TJ,ITI,TI)

* ����ե��å����ν����

      CALL SWISET('IWIDTH',  900)
      CALL SWISET('IHEIGHT', 600)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)      
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL SLDIV('Y',3,2)
      CALL SLRAT(1.0,1.0)

      RETURN
*----------------------------------------------------------------------
*     ���ڥ��ȥ뷸���ν����(2�Ĥθ�Ω��)
*----------------------------------------------------------------------
      ENTRY SBINIA(VAR)

* |K|=k0 ���濴�Ȥ����ȿ��˥��ͥ륮���ԡ�����Ϳ����.
* Ʊ��|K|���Ф����������������Ȥäƥ������Ϳ����.

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

      CALL P2G2SA(LM,KM,JM,IM,WG,VAR,WG(0,0,3),ITJ,TJ,ITI,TI)

      VAR(0,0,1)=F
      
* �������Х�󥹤ˤ�����Ͳ��򤹤�Ȥ�
      CALL P2SWBL(LM,KM,JM,IM,R,BARPHI,VAR(-LM,-KM,1),VAR(-LM,-KM,3),
     &  WS,WG,ITJ,TJ,ITI,TI)
      
* �����Ǥʤ��Ȥ�
*      VAR(0,0,3)=BARPHI

* UETONE �Υ�٥�����      

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
*     ���ڥ��ȥ뷸���ν����(��ή����)      
*----------------------------------------------------------------------
      ENTRY SBINIB(VAR)

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
          CALL ISNORM(ISEED,VAR(L,K,1))
        END DO
      END DO

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=INT(SQRT(1D0*K*K+L*L)+0.5D0)
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,1)*VAR(L,K,1)/(K*K+L*L)
          END IF
        END DO
      END DO

      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            VAR(L,K,1)=VAR(L,K,1)/SQRT(ENE(KT))*SQRT(DIST(KT))
          END IF
        END DO
      END DO

      VAR(0,0,1)=F
      
* �������Х�󥹤ˤ�����Ͳ��򤹤�Ȥ�
      CALL P2SWBL(LM,KM,JM,IM,R,BARPHI,VAR(-LM,-KM,1),VAR(-LM,-KM,3),
     &  WS,WG,ITJ,TJ,ITI,TI)
      
* �����Ǥʤ��Ȥ�
*      VAR(0,0,3)=BARPHI

* UETONE �Υ�٥�����      

      IPAT0=55
      DLEV=2
      NLEV=10
      IDLEV=3

      DO I=-NLEV,NLEV
        IPAT=(IPAT0+I*IDLEV)*1000+999
        IF(I.EQ.-NLEV) THEN
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
*     ����(T)����ӱ��٤Υ��ڥ��ȥ뷸��(Z)�����ϤȤ���, ��������Υ���
*     ���ȥ뷸��������(DZ)����륵�֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,VAR,DVAR)

      IF(ISPLIT.EQ.1) THEN
        CALL P2SWNN(LM,KM,JM,IM,R,BARPHI,F,
     &    VAR(-LM,-KM,1),VAR(-LM,-KM,2),VAR(-LM,-KM,3),
     &    DVAR(-LM,-KM,1),DVAR(-LM,-KM,2),DVAR(-LM,-KM,3),
     &    WS,WG,ITJ,TJ,ITI,TI)
      ELSE
        CALL P2SWNL(LM,KM,JM,IM,R,
     &    VAR(-LM,-KM,1),VAR(-LM,-KM,2),VAR(-LM,-KM,3),
     &    DVAR(-LM,-KM,1),DVAR(-LM,-KM,2),DVAR(-LM,-KM,3),
     &    WS,WG,ITJ,TJ,ITI,TI)
      END IF

      RETURN
*----------------------------------------------------------------------
*     ��¸�̤Υ����å�
*----------------------------------------------------------------------
      ENTRY SBGDCK(T,VAR)

      CALL P2SWCK(LM,KM,JM,IM,R,
     &  VAR(-LM,-KM,1),VAR(-LM,-KM,2),VAR(-LM,-KM,3),AENE,AENS,
     &  WS,WG,ITJ,TJ,ITI,TI)

      PRINT *,'T=',T,' AENE=',AENE,' AENS=',AENS

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӻ��ֹ��(DT)�����ϤȤ���, ����(Ǵ��)��ˤ��Z��
*     T��T+DT�ˤ�����ȯŸ��򤯥��֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,VAR)

* �����Ȥ���ʬ      

      IF(ISPLIT.EQ.1) THEN
        DO K=-KM,KM
          DO L=-LM,LM
            Z1=VAR(L,K,1)
            Z2=VAR(L,K,2)
            Z3=VAR(L,K,3)
            VAR(L,K,1)=DB(L,K,1,1)*Z1+DB(L,K,1,2)*Z2+DB(L,K,1,3)*Z3
            VAR(L,K,2)=DB(L,K,2,1)*Z1+DB(L,K,2,2)*Z2+DB(L,K,2,3)*Z3
            VAR(L,K,3)=DB(L,K,3,1)*Z1+DB(L,K,3,2)*Z2+DB(L,K,3,3)*Z3
          END DO
        END DO
      END IF

* �ⳬǴ�����̤���ʬ      

      DO IV=1,3
        DO K=-KM,KM
          DO L=-LM,LM
            VAR(L,K,IV)=DL(L,K)*VAR(L,K,IV)
          END DO
        END DO
      END DO

      RETURN
*----------------------------------------------------------------------
*     Z���б����뱲�پ�򥰥�ե��å���ɽ������.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,VAR)

      WRITE(CTIME,'(F5.1)') T
      
* ���پ�            

      CALL P2S2GA(LM,KM,JM,IM,VAR(-LM,-KM,1),
     &  WG,WG(0,0,2),ITJ,TJ,ITI,TI)
      DO I=0,IM-1
        DO J=0,JM-1
          RG(I,J)=WG(J,I,1)-F
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
      CALL UXSTTL('T','Vorticity  T='//CTIME,-1.0)

* ȯ����                  

      CALL P2S2GA(LM,KM,JM,IM,VAR(-LM,-KM,2),
     &  WG,WG(0,0,2),ITJ,TJ,ITI,TI)
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

      CALL USDAXS
      CALL UXSTTL('T','Divergence',-1.0)      
      CALL UDCNTZ(RG,IM+1,IM+1,JM+1,IBR,NBR)

* �����ݥƥ󥷥���                  

      CALL P2S2GA(LM,KM,JM,IM,VAR(-LM,-KM,3),
     &  WG,WG(0,0,2),ITJ,TJ,ITI,TI)
      DO I=0,IM-1
        DO J=0,JM-1
          RG(I,J)=WG(J,I,1)
          IF(WG(J,I,1).LE.0) THEN
            PRINT *, 'PHI IS NEGATIVE!! ',J,I,WG(J,I,1)
          END IF
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

      CALL USDAXS
      CALL UXSTTL('T','Geopotential',-1.0)
      CALL UDCNTZ(RG,IM+1,IM+1,JM+1,IBR,NBR)      

      RETURN
*----------------------------------------------------------------------
*     Z���б����륨�ͥ륮�����ڥ��ȥ�򥰥�ե��å���ɽ������.
*----------------------------------------------------------------------
      ENTRY SBSPEC(T,VAR)

      DO K=1,KM
        RK(K)=K
      END DO
      WRITE(CTIME,'(F5.1)') T

* ���پ�Υѥ���ڥ��ȥ�      

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,1)*VAR(L,K,1)
          END IF
        END DO
      END DO

      DO K=1,KM
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-20,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      CALL UXSTTL('T','Vorticity  T='//CTIME,-1.0)

* ȯ����Υѥ���ڥ��ȥ�      

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,2)*VAR(L,K,2)
          END IF
        END DO
      END DO

      DO K=1,KM
        RENE(K)=ENE(K)
      END DO
      
      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-20,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      CALL UXSTTL('T','Divergence',-1.0)

* �����ݥƥ󥷥���Υѥ���ڥ��ȥ�      

      CALL BSSET0(KM+LM,ENE)
      DO K=-KM,KM
        DO L=-LM,LM
          KT=SQRT(1D0*K*K+L*L)+0.5D0
          IF(KT.NE.0) THEN
            ENE(KT)=ENE(KT)+VAR(L,K,3)*VAR(L,K,3)
          END IF
        END DO
      END DO

      DO K=1,KM
        RENE(K)=ENE(K)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(1.0,KM*1.0,1E-20,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL ULXLOG( 'B', 1, 9 )
      CALL ULXLOG( 'T', 1, 9 )
      CALL ULYLOG( 'L', 1, 9 )
      CALL ULYLOG( 'R', 1, 9 )

      CALL SGPLU(KM,RK,RENE)
      CALL UXSTTL('T','Geopotential',-1.0)

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
