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
************************************************************************
*     2������������η׻�(�ʥ����ǲ��̥롼����)               1999/03/21
************************************************************************
      SUBROUTINE SOTNLT(MM,IM,ID,JM,JD,S,IT,T,Y,IP2,P2,R2,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(MM*MM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP2(2*((MM+1)/2+MM+1)*2)
      DIMENSION P2(2*((MM+1)/2+MM+1)*JM)
      DIMENSION R2(2*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(2*((MM+1)/2+MM+1)*JM)
      DIMENSION WS(ID*JD,2),WW(*)

*/ ���ڥ��ȥ�εͤ��ؤ�
      CALL SOTNLP(MM,S,WS,IA,A,WW)

*/ �른���ɥ��Ѵ�
      CALL SNLS2G(MM,JM,2,WS,WW,Y,P2,R2,Q)

*/ �ѥ�ƥ��Ѵ�
      CALL SNPS2G(MM,JM,JD,2,WW,WS,IP2,Y,0)

*/ �ա��ꥨ�Ѵ�
      CALL SNFS2G(MM,IM,JD,2,WS,WW,IT,T)

*/ ź�����¤��ؤ�
      CALL SNGS2G(IM,ID,JD,2,WW,WS)

*/ ��������η׻�

      DO IJ=1,ID*JD
        UV=WS(IJ,1)*WS(IJ,2)
        WS(IJ,2)=WS(IJ,2)*WS(IJ,2)-WS(IJ,1)*WS(IJ,1)
        WS(IJ,1)=UV
      END DO

*/ ź�����¤��ؤ�
      CALL SNGG2S(IM,ID,JD,2,WS,WW)

*/ �ա��ꥨ�Ѵ�
      CALL SNFG2S(MM,IM,JD,2,WW,WS,IT,T)

*/ �ѥ�ƥ��Ѵ�
      CALL SNPG2S(MM,JM,JD,2,WS,WW,IP2,Y,4)

*/ �른���ɥ��Ѵ�
      CALL SNLG2S(MM,JM,2,WW,WS,Y,P2,R2,Q)

*/ ���ڥ��ȥ�εͤ��ؤ�
      CALL SOTNLA(MM,WS,WW,IA,A)

      END
************************************************************************
      SUBROUTINE SOTNLP(MM,S,WS,IA,A,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(MM*MM)
      DIMENSION WS(2,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(((MM+1)/2*2+3)*(MM/2+2)*2)

      LM=(MM+1)*(MM+1)
      LMD=MM*MM
      CALL BSSET0(2*((MM+1)/2*2+3)*(MM/2+2)*2,WS)
      CALL BSSET0(((MM+1)/2*2+3)*(MM/2+2)*2,WW)

*/ S�η�����ʬ����뤿�ν���

      DO L=1,LMD
        WS(2,IA(L,2))=A(L,2)*S(L)
      END DO

*/ S�ΰ�����ʬ(�������Ѥ������)����뤿�ν���

      DO L=1,LMD
        WW(IA(L,3))=-A(L,3)*S(L)
      END DO
      DO L=1,LMD
        WS(1,IA(L,4))=-A(L,4)*S(L)
      END DO
      DO L=1,((MM+1)/2*2+3)*(MM/2+2)*2
        WS(1,L)=WS(1,L)+WW(L)
      END DO

      END
************************************************************************
      SUBROUTINE SOTNLA(MM,WS,WW,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WS(2,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION WW((MM+1)*(MM+1),2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        WW(L,1)=A(L,2)*WS(1,IA(L,2))
     &    +A(L,3)*WS(2,IA(L,3))+A(L,4)*WS(2,IA(L,4))
     &    +A(L,5)*WS(2,IA(L,3))+A(L,6)*WS(2,IA(L,4))
        WW(L,2)=-(A(L,3)*WS(1,IA(L,3))+A(L,4)*WS(1,IA(L,4)))
     &    -2*(A(L,5)*WS(1,IA(L,3))+A(L,6)*WS(1,IA(L,4)))
      END DO

      END
