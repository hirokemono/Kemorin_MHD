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
*     �����ΰ����Ǵ�������������λ���ȯŸ(��������ʬ��ʬΥ���Ʋ�)     
*                                              2003/04/10 By K. Ishioka
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=10)
      PARAMETER(IU=11,NB=16384) ! �ǡ������������ֹ�ȥХåե�Ĺ�λ���      
      PARAMETER(MMS=(MM+1)*(MM+2)/2*3-1)
      DIMENSION VAR(MMS),W(MMS*3),WS(MMS)
      EXTERNAL SBDVAN,SBDVAL      

      ITM=1000              !����ȯŸ���륹�ƥå׿�      
      NDV=10                !Runge-Kutta�ǤΥ��ƥå�ʬ���
      DT=1D0                !�ե�������Ϥλ��ֳִ�
      TIM=0

* ���֥롼����ѥå�����������ѿ��ν����
* ��������ʬ��ʬΥ���Ʋ򤯤���, Ϳ�������ܾ줬��̩�����ˤʤ�ʤ�����
* ������ʬ�ˤ����������򤫤��Ƥ��뤳�Ȥ����.

      CALL SBOPEN
      CALL FHUOPN(IU,'data.dat','W',NB) ! ���ϥե�����Υ����ץ�      
      CALL SBINIZ(VAR)
      CALL BSCOPY(MMS,VAR,WS)
      CALL TDRKNU(MMS,1,DT/NDV,TIM,VAR,W,SBDVAL,SBDVAN)
      DO K=1,MMS
        WS(K)=VAR(K)-WS(K)
      END DO
      CALL SBINIT(VAR)
      CALL FEPUTD(IU,MMS,VAR) ! �ǡ����ν���      

* ����ȯŸ�������¸�̤Υ����å��ȥ����ɽ��

      DO I=1,ITM
        DO J=1,NDV        
          CALL TDRKNU(MMS,1,DT/NDV,TIM,VAR,W,SBDVAL,SBDVAN)
          DO K=1,MMS
            VAR(K)=VAR(K)-WS(K)
          END DO
        END DO
        CALL FEPUTD(IU,MMS,VAR) ! �ǡ����ν���        
        TIM=I*DT
        CALL SBCCNS(VAR,AMOM,AENE)
        print *,TIM,VAR(1),AMOM,AENE ! ��¸�̤Υ����å�
      END DO

* ��λ����
      
      CALL FHUCLS(IU)      ! ���ϥե�����Υ�����

      END
************************************************************************
*     OPEN SUBROUTINE PACKAGE
************************************************************************
      SUBROUTINE SBOPEN

      IMPLICIT REAL*8(A-H,O-Z)
*-----------------------------------------------------------------------
*     �ѥ�᥿��������(MM���ͤϥᥤ��ץ������ͤ˹�碌�뤳��)
*-----------------------------------------------------------------------
      PARAMETER(F=1D0)        ! ���ꥪ��ѥ�᥿������
      PARAMETER(MM=10,JM=9,IM=32)
        ! ���Ǽ��� MM ����� ʬ���� JM, IM ������
        ! IM �� 3MM+1, JM��(MM+MM/2+2)/2 �Ǥ��뤳��
*-----------------------------------------------------------------------
*     �ʲ�, ���Ѥ������������ʤ�(�ä��ѹ�����ɬ�פϤʤ�)
*-----------------------------------------------------------------------
      PARAMETER(MMP=(MM+8)*MM/4+1)
      PARAMETER(MMS=(MM+1)*(MM+2)/2*3-1)
      DIMENSION P(JM,2*MMP)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)      
      DIMENSION T(IM*2),IT(5)
      DIMENSION S(MMS),DS(MMS)
      DIMENSION G(JM,0:IM-1,10),WORK(JM*IM)
      DIMENSION HB(JM,0:IM-1,3)

      DIMENSION WORKD(JM,13)      
      DIMENSION WL((MM/2+1)*3,6)
      DIMENSION WV((MM/2+1)*(MM/2+1)*9,3)
      DIMENSION WRM(MM*(MM+4)/2)
      DIMENSION VRM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      DIMENSION VLM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      SAVE

      CALL DKAINI(MM,JM,IM,IT,T,P,A)  ! DKPACK�ν����
      HBNDRY=1 ! �����Ǥο忼����
      CALL DKAEGA(MM,JM,F,HBNDRY,WORKD,P,A,WL,WV,WRM,VRM,VLM)
       ! ��������ʬ��ʬΥ���Ʋ򤯤���ι���ν���

      RETURN
*-----------------------------------------------------------------------
*     ��°�ѿ��ν����(���ܾ���ʬ�Τ�)
*-----------------------------------------------------------------------
      ENTRY SBINIZ(S)

      CALL BSSET0(JM*IM*3,HB)
      CALL BSSET0(JM*IM*3,G)

      DO I=0,IM-1
        DO J=1,JM
          Y=P(J,1)
          X=Y*Y
          G(J,I,1)=(-(1-X)**5/5-F*(1-X)**3/3)/2+1
          G(J,I,2)=Y*(1-X)**2
        END DO
      END DO

      CALL DKAG2S(MM,JM,IM,G,S,G(1,0,10),IT,T,P,A)      

      RETURN
*-----------------------------------------------------------------------
*     ��°�ѿ��ν����. �����Ǥ�,��Ȥ���, �԰���ʼ��оξ�����������
*     �ä�����Τ�Ϳ���Ƥ���(����Ȥ����ȿ�1��Ϳ���Ƥ���)
*-----------------------------------------------------------------------
      ENTRY SBINIT(S)

      CALL BSSET0(JM*IM*3,G)

      DO I=0,IM-1
        DO J=1,JM
          Y=P(J,1)
          X=Y*Y
          G(J,I,1)=(-(1-X)**5/5-F*(1-X)**3/3)/2+1
          G(J,I,2)=Y*(1-X)**2
        END DO
      END DO

      PI=4*ATAN(1D0)
      DO I=0,IM-1
        DO J=1,JM
          Y=P(J,1)
          X=Y*Y
          G(J,I,3)=1D-6*COS(2*PI*I/IM)*(1-X)
        END DO
      END DO

      CALL DKAG2S(MM,JM,IM,G,S,G(1,0,10),IT,T,P,A)

      RETURN
*-----------------------------------------------------------------------
*     ������ʬ��η׻�
*-----------------------------------------------------------------------
      ENTRY SBDVAR(TIM,S,DS)

      CALL DKATDV(MM,JM,IM,F,HB(1,0,2),S,DS,G,IT,T,P,A)

      RETURN
*-----------------------------------------------------------------------
*     ��������ʬ�ʳ��λ�����ʬ��η׻�
*-----------------------------------------------------------------------
      ENTRY SBDVAN(TIM,S,DS)

      CALL DKATDL(MM,JM,IM,F,HBNDRY,HB(1,0,2),S,DS,G,IT,T,P,A)

      RETURN
*-----------------------------------------------------------------------
*     ��������ʬ�λ���ȯŸ
*-----------------------------------------------------------------------
      ENTRY SBDVAL(TIM,DTIM,S)

      CALL DKATDG(MM,S,DTIM,WORK,WRM,VRM,VLM)

      RETURN
*-----------------------------------------------------------------------
*     ��¸��(���ѱ�ư�̤���������ͥ륮���η׻�)
*-----------------------------------------------------------------------
      ENTRY SBCCNS(S,AMOM,AENE)

      CALL DKACNS(MM,JM,IM,F,HB,S,AMOM,AENE,G,IT,T,P)

      END
