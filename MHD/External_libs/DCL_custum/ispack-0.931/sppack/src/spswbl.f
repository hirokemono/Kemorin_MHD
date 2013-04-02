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
*     �����������ν���Ͳ�                                    2000/05/06
************************************************************************
      SUBROUTINE SPSWBL(MM,IM,ID,JM,JD,OMEGA,BARPHI,
     &    AVT,PHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ �ºݤ���� */
      CALL SPSWBB(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,
     &  AVT,PHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWBB(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,
     &    AVT,PHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(MAXDIM,3)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2

*/ �פȦ֤η׻�(���Ū��, �פȤ���WS(*,3)��Ȥ�)
*/ �Ż߷Ϥ��鸫�뤳�Ȥˤ���.
      
      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(L,3)=RN(L,2)*AVT(L)
      END DO
      WS(3,3)=WS(3,3)+OMEGA/SQRT3
      
*/ ���η׻�(WS(*,1))

      CALL BSSET0(LMD,WS(1,2))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,3),1)=-A(L,3)*WS(L,3)
        WS(IA(L,4),2)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,2)
      END DO

*/ ���η׻�(WS(*,2))

      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WS(L,3)
      END DO

*/ ��(WS(*,3))�η׻��ν���

      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(IA(L,1),3)=A(L,1)*AVT(L)
      END DO

*/ ���ڥ��ȥ뢪����å�
      DO IV=1,3
*/      �른���ɥ��Ѵ�
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      �ѥ�ƥ��Ѵ�
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      �ա��ꥨ�Ѵ�
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      ź�����¤��ؤ�
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ ��������η׻�

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        WS(IJ,1)=WS(IJ,3)*U
        WS(IJ,2)=WS(IJ,3)*V
        WS(IJ,3)=(U*U+V*V)*0.5D0
      END DO
      
      DO IV=1,3
*/      ź�����¤��ؤ�
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      �ա��ꥨ�Ѵ�
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      �ѥ�ƥ��Ѵ�
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,2)
*/      �른���ɥ��Ѵ�
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO
 
      DO L=1,LM
        PHI(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),3)
      END DO

      DO L=2,LM
        PHI(L)=PHI(L)*RN(L,2)
      END DO
      PHI(1)=BARPHI

      END
