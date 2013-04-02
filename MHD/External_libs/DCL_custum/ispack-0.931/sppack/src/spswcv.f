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
*     ��������������¸�̤Υ����å�(����ΰ�︺��)            2000/04/03
************************************************************************
      SUBROUTINE SPSWCV(MM,IM,ID,JM,JD,OMEGA,
     &    AVT,DIV,PHI,AMOM,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)      
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
      CALL SPSWCL(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AMOM,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWCL(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &    AVT,DIV,PHI,AMOM,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)            
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(MAXDIM,4)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2

*/ �פȦ֤η׻�(���Ū��, �פȤ���WW(*)��, �֤Ȥ���WS(*,4)��Ȥ�)

      CALL BSSET0(LMD,WW)
      DO L=1,LM
        WW(L)=RN(L,2)*AVT(L)
      END DO
      WW(3)=WW(3)+OMEGA/SQRT3

*/ ���η׻�(WS(*,1))

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*RN(L,2)*DIV(L)
        WS(IA(L,3),3)=-A(L,3)*WW(L)
        WS(IA(L,4),4)=-A(L,4)*WW(L)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,3)+WS(L,4)
      END DO

*/ ���η׻�(WS(*,2))

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WW(L)
        WS(IA(L,3),3)=A(L,3)*RN(L,2)*DIV(L)
        WS(IA(L,4),4)=A(L,4)*RN(L,2)*DIV(L)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,3)+WS(L,4)
      END DO

*/ ��(WS(*,3))�Ȧ�(WS(*,4))�η׻��ν���

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,1),3)=A(L,1)*AVT(L)
        WS(IA(L,1),4)=A(L,1)*PHI(L)
      END DO

*/ ���ڥ��ȥ뢪����å�
      DO IV=1,4
*/      �른���ɥ��Ѵ�
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      �ѥ�ƥ��Ѵ�
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      �ա��ꥨ�Ѵ�
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      ź�����¤��ؤ�
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ ��¸�̤η׻�

      AMOM=0
      AENE=0
      AENS=0
      DO J=1,JM/2
        Y2=Y(J,2)
        Y3=Y(J,3)
        Y4=Y(J,4)
        J1=JM/2+J
        J2=JM/2-J+1
        DO I=1,IM
          IJ1=ID*(J1-1)+I
          IJ2=ID*(J2-1)+I
          U1=WS(IJ1,1)
          U2=WS(IJ2,1)
          V1=WS(IJ1,2)
          V2=WS(IJ2,2)
          Q1=WS(IJ1,3)
          Q2=WS(IJ2,3)
          H1=WS(IJ1,4)
          H2=WS(IJ2,4)
          AMOM=AMOM+Y2*( H1*(U1+OMEGA*Y3*Y3)
     &                  +H2*(U2+OMEGA*Y3*Y3) )
          AENE=AENE+0.5D0*Y2*( H1*((U1*U1+V1*V1)*Y4*Y4+H1)
     &                        +H2*((U2*U2+V2*V2)*Y4*Y4+H2) )
          AENS=AENS+0.5D0*Y2*(Q1*Q1/H1+Q2*Q2/H2)
          IF(H1.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J1,H1
          END IF
          IF(H2.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J2,H2
          END IF
        END DO
      END DO
      AMOM=AMOM/IM
      AENE=AENE/IM
      AENS=AENS/IM

      END
