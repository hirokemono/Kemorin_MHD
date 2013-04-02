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
*     convection.f: c2pack��Ȥäƥ٥ʡ�����ή��׻�����ץ����
*                                                  2000/10/16 K.Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

* �ѥ�᥿�������� 

      PARAMETER(LM=42,KM=42)    !�����ȿ�������
      PARAMETER(N=LM*(2*KM+1)*2)  !���ڥ��ȥ��ѿ���������礭��
      DIMENSION Z(N)            !���٤Υ��ڥ��ȥ뷸��������
      DIMENSION W(N,3)          !Runge-Kuttaˡ�Τ���κ���ΰ�
      EXTERNAL SBGDZL,SBGDZN    !Runge-Kuttaˡ���ƤӽФ����֥롼����

      NSTEP=200                 !����ȯŸ���륹�ƥå׿�
      H=10D0                     !����ե��å���ɽ���Τ���λ��ֳִ�
      M=2                       !Runge-Kutta�ǤΥ��ƥå�ʬ���      

      DELTAT=H/M                !Runge-Kutta�����λ��ֹ��

* �����

      CALL SBINIT(DELTAT)
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
      SUBROUTINE SBINIT(DELTAT)

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CTIME*6
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(JM=64,IM=128)    !�ʻ�����������(JM: y����, IM: x����)
      PARAMETER(LM=42,KM=42)     !�����ȿ�������
      PARAMETER(ISPLIT=0)        !�⡼��ʬ�򤷤ư������ɤ����Υե饰
                                 !1�ʤ�ʬΥ����. ����ʳ��ʤ�ʬΥ���ʤ�. 
      DIMENSION Z(-KM:KM,LM,2)   !���٤ΤȲ��٥��ڥ��ȥ뷸��������
      DIMENSION DZ(-KM:KM,LM,2)  !��������Υ��ڥ��ȥ뷸��������
      DIMENSION WS(-KM:KM,0:LM)  !����ΰ�
      DIMENSION WG(0:JM,0:IM-1,4) !����ΰ�
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2) !C2PACK�ǻȤ�������
      DIMENSION DL(-KM:KM,LM,2,2) !��ͭ�⡼�ɤ�ȯŸ�˻Ȥ�������
      DIMENSION DIFF(-KM:KM,LM,2) !Ǵ�����̤Τ���˻Ȥ�������      
      REAL RG(0:IM,0:JM)        !����ե��å����˻Ȥ�������
      REAL DLEV,TLEV1,TLEV2     !����ե��å����˻Ȥ����ѿ�
      SAVE

*--------------------------------------------------
* �¸��ѥ�᥿��������. ʪ���ͤϤȤꤢ������Τ�Τ�CGSñ�̤�Ϳ���Ƥ���.
*      
      ALPHA=2.1D-4              !������ĥΨ
      GRAV=980D0                !���ϲ�®��
      DKAPPA=1.4D-3             !Ǯ�Ȼ�����
      DNU=1D-2                  !Ǵ������
      DEPTH=1D0                 !�忼
      DTEMP=0.1D0               !��ü�Ȳ�ü�β��ٺ�
*      DTEMP=0.05D0               !��ü�Ȳ�ü�β��ٺ�      
*      DTEMP=0.0448D0             !��ü�Ȳ�ü�β��ٺ�(����ƥ��������)
      DLENG=2*SQRT(2D0)*DEPTH   !��ʿ�����μ���
*--------------------------------------------------

      RAT=2*DEPTH/DLENG

      DKAPPB=DKAPPA*(PI/DEPTH)*(PI/DEPTH)
      DNUD=DNU*(PI/DEPTH)*(PI/DEPTH)      

      COEF1=ALPHA*GRAV*2*PI/DLENG
      COEF2=DTEMP/PI*RAT

      RAYLGH=ALPHA*GRAV*DTEMP*DEPTH**3/(DNU*DKAPPA)
      RC=PI**4*27/4

      WRITE(6,*) 'Rayleigh Number=',RAYLGH
      WRITE(6,*) 'Critical Rayleigh Number=',RC

* Ϳ����줿��������, SBGDZN�ǻȤ�������DL(Ǵ���ˤ�븺����̤�Ϳ����)
* �����ꤹ��. �ʤ�, SBGDZN��Runge-Kutta�������ǸƤӽФ���뤬, ���κ�,
* ���DT=DELTAT/2�ǸƤӽФ���뤫��, ������θ��������, DL�ϰʲ��Τ�
* ��������Ǥ���.

      IF(ISPLIT.EQ.1) THEN
        DO K=-KM,KM
          DO L=1,LM
            RK=RAT*K
            A=-DNUD*(RK*RK+L*L)
            B=-ALPHA*GRAV*2*PI/DLENG*K
            C=-DTEMP/PI*RK/(RK*RK+L*L)
            D=-DKAPPB*(RK*RK+L*L)
            SIGMA1=(A+D-SQRT((A-D)*(A-D)+4*B*C))/2
            SIGMA2=(A+D+SQRT((A-D)*(A-D)+4*B*C))/2
            ESIG1=EXP(SIGMA1*DELTAT/2)
            ESIG2=EXP(SIGMA2*DELTAT/2)
            SIG21=SIGMA2-SIGMA1
*          WRITE(6,*) K,SIGMA2
            DL(K,L,1,1)=((SIGMA2-A)*ESIG1+(A-SIGMA1)*ESIG2)/SIG21
            DL(K,L,1,2)=B/SIG21*(-ESIG1+ESIG2)
            DL(K,L,2,1)=C/SIG21*(-ESIG1+ESIG2)          
            DL(K,L,2,2)=((SIGMA2-D)*ESIG1+(D-SIGMA1)*ESIG2)/SIG21
          END DO
        END DO
      ELSE
        DO K=-KM,KM
          DO L=1,LM
            DIFF(K,L,1)=EXP(-DNUD*DELTAT/2*((RAT*K)*(RAT*K)+L*L))
            DIFF(K,L,2)=EXP(-DKAPPB*DELTAT/2*((RAT*K)*(RAT*K)+L*L))          
          END DO
        END DO
      END IF

* C2PACK�ν����

      CALL C2INIT(JM,IM,ITJ,TJ,ITI,TI)

* ����ե��å����ν����

      CALL SWISET('IWIDTH',  400)
      CALL SWISET('IHEIGHT', 400)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL SLDIV('T',1,2)
      CALL SLRAT(1.0,0.5)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)

      IPAT0=25
      DLEV=0.005
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

* �����Ǥ�, �԰���ʱ��ؤ����������ä������Ϳ��,
* �����C2G2SA��Ȥäƥ��ڥ��ȥ뷸�����Ѵ�����, �����Z�ν���ͤ�
* ���Ƥ���.

      CALL BSSET0((2*KM+1)*LM*2,Z)
      CALL BSSET0(IM*(JM+1),WG)
      
      WG(JM/2,IM/2,1)=1D-6
      CALL C2G2SA(LM,KM,JM,IM,WG,Z(-KM,1,2),WG(0,0,3),ITJ,TJ,ITI,TI,1)

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӱ��٤Υ��ڥ��ȥ뷸��(Z)�����ϤȤ���, ��������Υ���
*     ���ȥ뷸��������(DZ)����륵�֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZN(T,Z,DZ)

      CALL C2AJB2(LM,KM,JM,IM,RAT,Z,Z(-KM,1,2),DZ,DZ(-KM,1,2),
     &  WS,WG,ITJ,TJ,ITI,TI)

      IF(ISPLIT.NE.1) THEN
        DO L=1,LM
          DO K=-KM,KM
            DZ(K,L,1)=DZ(K,L,1)-COEF1*K*Z(-K,L,2)
            DZ(K,L,2)=DZ(K,L,2)-COEF2*K*
     &        (-Z(-K,L,1)/((RAT*K)*(RAT*K)+L*L))
          END DO
        END DO
      END IF

      RETURN
*----------------------------------------------------------------------
*     ����(T)����ӻ��ֹ��(DT)�����ϤȤ���, ����(Ǵ��)��ˤ��Z��
*     T��T+DT�ˤ�����ȯŸ��򤯥��֥롼����(Runge-Kutta�ǻȤ���)
*----------------------------------------------------------------------
      ENTRY SBGDZL(T,DT,Z)

      IF(ISPLIT.EQ.1) THEN
        DO L=1,LM
          DO K=-KM,KM
            TMPZ1=Z(-K,L,1)
            Z(-K,L,1)=DL(K,L,1,1)*TMPZ1-DL(K,L,1,2)*Z(K,L,2)
            Z(K,L,2)=-DL(K,L,2,1)*TMPZ1+DL(K,L,2,2)*Z(K,L,2)
          END DO
        END DO
      ELSE
        DO L=1,LM
          DO K=-KM,KM
            Z(K,L,1)=DIFF(K,L,1)*Z(K,L,1)
            Z(K,L,2)=DIFF(K,L,2)*Z(K,L,2)
          END DO
        END DO
      END IF

      RETURN
*----------------------------------------------------------------------
*     Z���б������򥰥�ե��å���ɽ������.
*----------------------------------------------------------------------
      ENTRY SBGRPH(T,Z)

      WRITE(CTIME,'(F6.1)') T

* ���پ��ɽ��      

      CALL C2S2GA(LM,KM,JM,IM,Z(-KM,1,2),WG,WG(0,0,3),ITJ,TJ,ITI,TI,1)      

      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)+DTEMP/DEPTH*(JM-J)/JM
        END DO
      END DO

      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.08,0.08+1/(2*SQRT(2.0))*0.8)
      CALL SGSWND(0.0,2*SQRT(2.0),0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UETONE(RG,IM+1,IM+1,JM+1)
      CALL USDAXS
      CALL UXSTTL('T','Temperature',0.0)
      CALL UXSTTL('T','T='//CTIME,1.0)
      CALL UDCNTR(RG,IM+1,IM+1,JM+1)

* ή���ؿ����ɽ��        

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-Z(K,L,1)/((RAT*K)*(RAT*K)+L*L)          
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG,WG(0,0,3),ITJ,TJ,ITI,TI,1)

      DO I=0,IM-1
        DO J=0,JM
          RG(I,J)=WG(J,I,1)
        END DO
      END DO

      DO J=0,JM
        RG(IM,J)=RG(0,J)
      END DO

      CALL GRFRM
      CALL SGSVPT(0.13,0.93,0.15,0.15+1/(2*SQRT(2.0))*0.8)
      CALL SGSWND(0.0,2*SQRT(2.0),0.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL USDAXS      
      CALL UDCNTR(RG,IM+1,IM+1,JM+1)
      CALL UXSTTL('T','Stream-Function',0.0)      

      RETURN
*----------------------------------------------------------------------
*     ��λ����
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS

      END
