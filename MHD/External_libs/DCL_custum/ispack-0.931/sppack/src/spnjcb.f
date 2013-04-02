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
*     2�����䥳�ӥ���η׻�                                   1999/03/21
************************************************************************
      SUBROUTINE SPNJCB(MM,IM,ID,JM,JD,
     &  SA,SB,SC,IT,T,Y,IP2,P2,R2,IP3,P3,R3,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA((MM+1)*(MM+1)),SB((MM+1)*(MM+1))
      DIMENSION SC((MM+1)*(MM+1))
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP2(2*((MM+1)/2+MM+1)*2)
      DIMENSION P2(2*((MM+1)/2+MM+1)*JM)
      DIMENSION R2(2*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP3(3*((MM+1)/2+MM+1)*2)
      DIMENSION P3(3*((MM+1)/2+MM+1)*JM)
      DIMENSION R3(3*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(3*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(ID*JD,3)

*/ ���ڥ��ȥ�εͤ��ؤ�
      CALL SPNS2G(MM,SA,SB,WS,IA,A,WW)
      
*/ �른���ɥ��Ѵ�
      CALL SNLS2G(MM,JM,3,WS,WW,Y,P3,R3,Q)

*/ �ѥ�ƥ��Ѵ�
      CALL SNPS2G(MM,JM,JD,3,WW,WS,IP3,Y,0)

*/ �ա��ꥨ�Ѵ�
      CALL SNFS2G(MM,IM,JD,3,WS,WW,IT,T)

*/ ź�����¤��ؤ�
      CALL SNGS2G(IM,ID,JD,3,WW,WS)

*/ ��������η׻�

      DO IJ=1,ID*JD
        WS(IJ,1)=WS(IJ,1)*WS(IJ,3)
        WS(IJ,2)=WS(IJ,2)*WS(IJ,3)
      END DO

*/ ź�����¤��ؤ�
      CALL SNGG2S(IM,ID,JD,2,WS,WW)

*/ �ա��ꥨ�Ѵ�
      CALL SNFG2S(MM,IM,JD,2,WW,WS,IT,T)

*/ �ѥ�ƥ��Ѵ�
      CALL SNPG2S(MM,JM,JD,2,WS,WW,IP2,Y,2)

*/ �른���ɥ��Ѵ�
      CALL SNLG2S(MM,JM,2,WW,WS,Y,P2,R2,Q)

*/ ���ڥ��ȥ�εͤ��ؤ�
      CALL SPNG2S(MM,WS,SC,IA,A)

      END
************************************************************************
      SUBROUTINE SPNS2G(MM,SA,SB,WS,IA,A,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA((MM+1)*(MM+1)),SB((MM+1)*(MM+1))
      DIMENSION WS(3,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(((MM+1)/2*2+3)*(MM/2+2)*2)

      LM=(MM+1)*(MM+1)
      CALL BSSET0(3*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/ SB���ͤ���뤿��ν���

      DO L=1,LM
        WS(3,IA(L,1))=A(L,1)*SB(L)
      END DO

*/ SA�η�����ʬ����뤿�ν���

      DO L=1,LM
        WS(2,IA(L,2))=A(L,2)*SA(L)
      END DO

*/ SA�ΰ�����ʬ(�������Ѥ������)����뤿�ν���

      CALL BSSET0(((MM+1)/2*2+3)*(MM/2+2)*2,WW)
      DO L=1,LM
        WW(IA(L,3))=-A(L,3)*SA(L)
      END DO
      DO L=1,LM
        WS(1,IA(L,4))=-A(L,4)*SA(L)
      END DO
      DO L=1,((MM+1)/2*2+3)*(MM/2+2)*2
        WS(1,L)=WS(1,L)+WW(L)
      END DO

      END
************************************************************************
      SUBROUTINE SPNG2S(MM,WS,SC,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WS(2,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION SC((MM+1)*(MM+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        SC(L)=-A(L,2)*WS(1,IA(L,2))
     &    -A(L,3)*WS(2,IA(L,3))-A(L,4)*WS(2,IA(L,4))
      END DO

      END
