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
*     ������������sppack�ǲ򤯤���Υ���ץ�ץ����       2002/05/19
*                                (MPI�ǰ�ľ���󲽤������) by K.Ishioka
***********************************************************************
      PROGRAM MODEL

      IMPLICIT REAL*8(A-H,O-Z)

      CALL SAMAIN(11,'output.dat')

      END
************************************************************************
*     TEST MODEL FOR NEW ISPACK
************************************************************************
      SUBROUTINE SAMAIN(IU,CF)

*     IU: ���ϥե�����������ֹ�
*     CF: ���ϥե�����̾
 
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CF*(*)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(MM=21)
      PARAMETER(LM=(MM+1)*(MM+1))
      DIMENSION VAR(LM,3),W(LM*3*3)
      EXTERNAL SBDVAR,SBDISS

*---- OPEN SUBROUTINE PACKAGE AND INITIALIZE VARIABLES -----------------

      LEV=10                    !�ⳬǴ����Υ�ץ饷����γ���
      ITM=10                    !����ȯŸ���륹�ƥå׿�
      NDV=10                     !Runge-Kutta�ǤΥ��ƥå�ʬ���
      DT=1D0                  !�ե�������Ϥλ��ֳִ�

      DNU=10D0/(1D0*MM*(MM+1))**LEV  !�ⳬǴ���η���
*      DNU=0                     

*/ ���������󷿤ξ���Υѥ�᥿��(�����ݥƥ󥷥���Ϳ����) /*

      DA=1D-1     !����ο���
      DB=10D0    !��������εտ��Υ롼�Ȥ���������ѥ�᥿��
      X0=PI        !������濴���֤Φ˺�ɸ
*      Y0=PI/4     !������濴���֤Φպ�ɸ
      Y0=0     

      CALL SBOPEN(IU,CF,LEV,DNU,NDV,DT)
      CALL SBINIT(VAR,DA,DB,X0,Y0)

*---- TIME EVOLUTION BY RUNGE-KUTTA METHOD -----------------------------

      I=0
      TIM=0
      CALL SBPUTS(VAR)
      CALL SBCHCK(I,VAR)

      DO I=1,ITM
        CALL TDRKNU(LM*3,NDV,DT,TIM,VAR,W,SBDISS,SBDVAR)
        CALL SBPUTS(VAR)
        CALL SBCHCK(I,VAR)
      END DO

*---- CLOSE SUBROUTINE PACKAGE -----------------------------------------

      CALL SBCLOS
      
      END
************************************************************************
*     OPEN SUBROUTINE PACKAGE
************************************************************************
      SUBROUTINE SBOPEN(IUD,CF,LEV,DNU,NDV,DT)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'
      PARAMETER(MM=21,JM=32,IM=64,LM=(MM+1)*(MM+1))
      PARAMETER(ID=IM+1,JD=JM+1)
      PARAMETER(IW=IM+2,JW=JD)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      PARAMETER(NB=23476)
      CHARACTER CF*(*)      

*/ OMEGA=��, BARPHI=���δ���� /*
      
      PARAMETER(OMEGA=2*PI,BARPHI=1D0)

*/ �����Ȥ�ʬΥ���Ʋ򤯤��ɤ����Υե饰.
*/      ISWTCH=1�ʤ�ʬΥ���Ʋ�. �����Ǥʤ����ʬΥ���ʤ�.

      PARAMETER(ISWTCH=1)

      DIMENSION VAR(LM,3),DVAR(LM,3)
      DIMENSION RN(LM,2),DRN(LM),DOM(LM)
      DIMENSION CD(LM),SD(LM)
      DIMENSION CP(LM),SP(LM)
      DIMENSION XG(IM),YG(JM)
      DIMENSION G(ID,JD)
      DIMENSION IT(5),T(IM*2),Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2),P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP4(4*((MM+1)/2+MM+1)*2),P4(4*((MM+1)/2+MM+1)*JM)
      DIMENSION R4(4*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP5(5*((MM+1)/2+MM+1)*2),P5(5*((MM+1)/2+MM+1)*JM)
      DIMENSION R5(5*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(5*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(5*JW*IW),WS(5*JW*IW)
      DIMENSION W(LM,3)
      SAVE

      IU=IUD ! ���������ֹ����¸

      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)      

      CALL SNMINI(MM,IM,JM,JC,IT,T,Y,IP,P,R,IA,A)

      JDD=JC+1 ! VP�Τ���˴���ˤȤäƤ���

      IF(JDD.GT.JD) THEN
        WRITE(6,*) 'JD MUST .GE. JDD'
        STOP
      END IF

      IF(JC.GT.0) THEN      
        CALL SNKINI(MM,JC,4,IP,P,R,IP4,P4,R4)
        CALL SNKINI(MM,JC,5,IP,P,R,IP5,P5,R5)
      END IF
      CALL SPNINI(MM,RN)

      DO J=1,JC/2
        YG(JC/2+J  )= ASIN(Y(J))
        YG(JC/2-J+1)=-ASIN(Y(J))
      END DO

      DO J=1,IM
        XG(J)=2*PI*(J-1)/IM
      END DO

      DTDNU=DNU*DT/(2*NDV)

      DO L=1,LM
        DRN(L)=EXP(-DTDNU*ABS(RN(L,1))**LEV)
      END DO

*/ ��������ʬ���ۤ�ʬΥ���Ʋ򤯺ݤ�ɬ�פ�����ν���      

      DO L=1,LM
        DOM(L)=SQRT(-RN(L,1)*BARPHI)
      END DO

      DTINT=DT/(2*NDV)

      CD(1)=1
      SD(1)=0
      CP(1)=1
      SP(1)=0
      DO L=2,LM
        CD(L)=COS(DOM(L)*DTINT)
        SD(L)=SIN(DOM(L)*DTINT)*(-RN(L,1))/DOM(L)
        CP(L)=COS(DOM(L)*DTINT)
        SP(L)=-SIN(DOM(L)*DTINT)*BARPHI/DOM(L)
      ENDDO

* OPEN FHPACK
      
      IF(IPROC.EQ.0) THEN
        CALL FHUOPN(IU,CF,'W',NB)
      END IF

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF d(VAR)/dt
*-----------------------------------------------------------------------
      ENTRY SBDVAR(TIM,VAR,DVAR)

      CALL SPMWNL(MM,IM,ID,JC,JDD,OMEGA,
     &  VAR(1,1), VAR(1,2), VAR(1,3),
     &  DVAR(1,1),DVAR(1,2),DVAR(1,3),
     &  RN,IT,T,Y,IP4,P4,R4,IP5,P5,R5,IA,A,Q,WS,WW,W)

      IF(ISWTCH.EQ.1) THEN
        DO L=1,LM
          DVAR(L,2)=DVAR(L,2)+RN(L,1)*VAR(L,3)
          DVAR(L,3)=DVAR(L,3)+BARPHI*VAR(L,2)
        END DO
      END IF

      RETURN
*-----------------------------------------------------------------------
*     CALCULATION OF THE EFFECT OF VISCOSITY
*-----------------------------------------------------------------------
      ENTRY SBDISS(TIM,DTIM,VAR)

      IF(ISWTCH.EQ.1) THEN
        DO L=1,LM
          VAR(L,1)=DRN(L)*VAR(L,1)
          TMPPHI=VAR(L,2)
          VAR(L,2)=DRN(L)*(CD(L)*VAR(L,2)+SD(L)*VAR(L,3))
          VAR(L,3)=DRN(L)*(CP(L)*VAR(L,3)+SP(L)*TMPPHI)
        END DO
      ELSE
        DO L=1,LM
          VAR(L,1)=DRN(L)*VAR(L,1)
          VAR(L,2)=DRN(L)*VAR(L,2)
          VAR(L,3)=DRN(L)*VAR(L,3)
        END DO
      END IF
      
      RETURN
*-----------------------------------------------------------------------
*     CHECK ENERGY AND ENSTROPHY CONSERVATION
*-----------------------------------------------------------------------
      ENTRY SBCHCK(I,VAR)

      CALL SPMWCK(MM,IM,ID,JC,JDD,OMEGA,
     &  VAR(1,1), VAR(1,2), VAR(1,3),
     &  AMOM,AENE,AENS,
     &  RN,IT,T,Y,IP4,P4,R4,IA,A,Q,WS,WW)

      IF(IPROC.EQ.0) THEN
        print *,'amom= ',AMOM,' aene=',AENE,' aens=',AENS
      END IF

      RETURN
*-----------------------------------------------------------------------
*     INITIALIZATION OF VAR
*-----------------------------------------------------------------------
      ENTRY SBINIT(VAR,DA,DB,X0,Y0)

      CALL BSSET0(LM*3,VAR)

*/ ���б��پ������(�ץ�ͥ��꡼���٤Τ�)
      VAR(3,1)=2*OMEGA/SQRT3

*/ �ݥƥ󥷥��������

      IF(JC.NE.0) THEN
        CALL SCINID(JC,IM,JDD,ID,G,XG,YG,DA,DB,X0,Y0)        
      END IF

      CALL SNTGMS(MM,IM,ID,JC,JDD,1,
     &  G,VAR(1,3),IT,T,Y,IP,P,R,IA,A,Q,WS,WW,0,0,W)

      VAR(1,3)=BARPHI

      RETURN
*-----------------------------------------------------------------------
*     WRITE DATA
*-----------------------------------------------------------------------
      ENTRY SBPUTS(VAR)

      IF(IPROC.EQ.0) THEN
        CALL FEPUTS(IU,LM*3,VAR)
      END IF

      RETURN
*-----------------------------------------------------------------------
*     CLOSE SUBROUTINE PACKAGE
*-----------------------------------------------------------------------
      ENTRY SBCLOS

* CLOSE FHPACK
      
      IF(IPROC.EQ.0) THEN
        CALL FHUCLS(IU)
      END IF
      
      CALL MPI_FINALIZE(IERR)      
      
      END
************************************************************************
*     CALCULATE DISTURBANCE FIELD
************************************************************************
      SUBROUTINE SCINID(JC,IM,JDD,ID,G,XG,YG,DA,DB,X0,Y0)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION G(ID,JDD)
      DIMENSION YG(JC),XG(IM)

      CB=COS(PI/2-Y0)
      SB=SIN(PI/2-Y0)

      DO I=1,IM
        CA=COS(XG(I)-X0)
        DO J=1,JC
          CC=COS(PI/2-YG(J))
          SC=SIN(PI/2-YG(J))
          CR=CB*CC+SB*SC*CA
          G(I,J)=DA*(EXP(DB*(CR-1))-(1-EXP(-2*DB))/(2*DB))
        END DO
      END DO

      END
