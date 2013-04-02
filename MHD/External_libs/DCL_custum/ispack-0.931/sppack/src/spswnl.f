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
*     浅水方程式の非線形項の計算                              1999/03/29
************************************************************************
      SUBROUTINE SPSWNL(MM,IM,ID,JM,JD,OMEGA,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,
     &    RN,IT,T,Y,IP4,P4,R4,IP5,P5,R5,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP4(4*((MM+1)/2+MM+1)*2)
      DIMENSION P4(4*((MM+1)/2+MM+1)*JM)
      DIMENSION R4(4*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP5(5*((MM+1)/2+MM+1)*2)
      DIMENSION P5(5*((MM+1)/2+MM+1)*JM)
      DIMENSION R5(5*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(5*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(ID*JD,5)

*/ スペクトルの詰め替え
      CALL SPSWSG(MM,OMEGA,AVT,DIV,PHI,WS,RN,IA,A,WW)

*/ ルジャンドル変換
      CALL SNLS2G(MM,JM,4,WS,WW,Y,P4,R4,Q)

*/ パリティ変換
      CALL SNPS2G(MM,JM,JD,4,WW,WS,IP4,Y,0)

*/ フーリエ変換
      CALL SNFS2G(MM,IM,JD,4,WS,WW,IT,T)

*/ 添字の並べ替え
      CALL SNGS2G(IM,ID,JD,4,WW,WS)

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        WS(IJ,1)=U*WS(IJ,3)
        WS(IJ,2)=U*WS(IJ,4)
        WS(IJ,3)=V*WS(IJ,3)
        WS(IJ,4)=V*WS(IJ,4)
        WS(IJ,5)=(U*U+V*V)*0.5D0
      END DO

*/ 添字の並べ替え
      CALL SNGG2S(IM,ID,JD,5,WS,WW)

*/ フーリエ変換
      CALL SNFG2S(MM,IM,JD,5,WW,WS,IT,T)

*/ パリティ変換
      CALL SNPG2S(MM,JM,JD,5,WS,WW,IP5,Y,2)

*/ ルジャンドル変換
      CALL SNLG2S(MM,JM,5,WW,WS,Y,P5,R5,Q)

*/ スペクトルの詰め替え
      CALL SPSWGS(MM,PHI,WS,DAVT,DDIV,DPHI,RN,IA,A)

      END
************************************************************************
      SUBROUTINE SPSWSG(MM,OMEGA,AVT,DIV,PHI,WS,RN,IA,A,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION WS(4,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(((MM+1)/2*2+3)*(MM/2+2)*2,4)

      LM=(MM+1)*(MM+1)
      CALL BSSET0(4*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/ PSIとCHIの計算(一時的に, PSIとしてW(L,1)を, CHIとしてW(L,2)を使う)

      DO L=1,LM
        WW(L,1)=RN(L,2)*AVT(L)
        WW(L,2)=RN(L,2)*DIV(L)
      END DO
      WW(3,1)=WW(3,1)+OMEGA/SQRT3

*/ ｑとΦの計算の準備

      DO L=1,LM
        WS(3,IA(L,1))=A(L,1)*AVT(L)
        WS(4,IA(L,1))=A(L,1)*PHI(L)
      END DO

*/ ｕの計算

      CALL BSSET0(2*((MM+1)/2*2+3)*(MM/2+2)*2,WW(1,3))
      DO L=1,LM
        WS(1,IA(L,2))=A(L,2)*WW(L,2)
        WW(IA(L,3),3)=-A(L,3)*WW(L,1)
        WW(IA(L,4),4)=-A(L,4)*WW(L,1)
      END DO
      DO L=1,((MM+1)/2*2+3)*(MM/2+2)*2
        WS(1,L)=WS(1,L)+WW(L,3)+WW(L,4)
      END DO

*/ ｖの計算

      CALL BSSET0(2*((MM+1)/2*2+3)*(MM/2+2)*2,WW(1,3))
      DO L=1,LM
        WS(2,IA(L,2))=A(L,2)*WW(L,1)
        WW(IA(L,3),3)=A(L,3)*WW(L,2)
        WW(IA(L,4),4)=A(L,4)*WW(L,2)
      END DO
      DO L=1,((MM+1)/2*2+3)*(MM/2+2)*2
        WS(2,L)=WS(2,L)+WW(L,3)+WW(L,4)
      END DO

      END
************************************************************************
      SUBROUTINE SPSWGS(MM,PHI,WS,DAVT,DDIV,DPHI,RN,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION WS(5,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        DAVT(L)=A(L,2)*WS(1,IA(L,2))
     &    +A(L,3)*WS(3,IA(L,3))+A(L,4)*WS(3,IA(L,4))
        DDIV(L)=-A(L,2)*WS(3,IA(L,2))
     &    +A(L,3)*WS(1,IA(L,3))+A(L,4)*WS(1,IA(L,4))
     &    -RN(L,1)*(A(L,1)*WS(5,IA(L,1))+PHI(L))
        DPHI(L)=A(L,2)*WS(2,IA(L,2))
     &    +A(L,3)*WS(4,IA(L,3))+A(L,4)*WS(4,IA(L,4))
      END DO

      END
