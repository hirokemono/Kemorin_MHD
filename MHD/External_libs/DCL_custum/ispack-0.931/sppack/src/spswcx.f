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
*     浅水方程式の保存量のチェック(作業領域削減版)            2000/09/07
*     (角運動量を3成分計算する. ただし回転系では地軸まわり以外の
*      角運動量は保存量にならないことに注意)
************************************************************************
      SUBROUTINE SPSWCX(MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AM1,AM2,AM3,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

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

*/ 実際の操作 */
      CALL SPSWCT(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AM1,AM2,AM3,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWCT(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AM1,AM2,AM3,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      PARAMETER(PI=3.1415926535897932385D0)
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

*/ ψとχの計算(一時的に, ψとしてWW(*)を, χとしてWS(*,4)を使う)

      CALL BSSET0(LMD,WW)
      DO L=1,LM
        WW(L)=RN(L,2)*AVT(L)
      END DO
      WW(3)=WW(3)+OMEGA/SQRT3

*/ ｕの計算(WS(*,1))

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

*/ ｖの計算(WS(*,2))

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

*/ ｑ(WS(*,3))とΦ(WS(*,4))の計算の準備

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,1),3)=A(L,1)*AVT(L)
        WS(IA(L,1),4)=A(L,1)*PHI(L)
      END DO

*/ スペクトル→グリッド
      DO IV=1,4
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ 保存量の計算

      AM1=0
      AM2=0
      AM3=0            
      AENE=0
      AENS=0
      DO I=1,IM
        C=COS(2*PI*(I-1)/IM)
        S=SIN(2*PI*(I-1)/IM)
        DO J=1,JM/2
          Y1=Y(J,1)
          Y2=Y(J,2)
          Y3=Y(J,3)
          Y4=Y(J,4)
          J1=JM/2+J
          J2=JM/2-J+1
          IJ1=ID*(J1-1)+I
          IJ2=ID*(J2-1)+I
          U1=WS(IJ1,1)*Y4
          U2=WS(IJ2,1)*Y4
          UA1=U1+OMEGA*Y3
          UA2=U2+OMEGA*Y3
          V1=WS(IJ1,2)*Y4
          V2=WS(IJ2,2)*Y4
          Q1=WS(IJ1,3)
          Q2=WS(IJ2,3)
          H1=WS(IJ1,4)
          H2=WS(IJ2,4)
          AM1=AM1+Y2*(H1*UA1+H2*UA2)*Y3
          AM2=AM2+Y2*(H1*(-UA1*C*Y1+V1*S)+H2*(UA2*C*Y1+V2*S))
          AM3=AM3+Y2*(H1*(-UA1*S*Y1-V1*C)+H2*(UA2*S*Y1-V2*C))
          AENE=AENE+Y2/2*(H1*((U1*U1+V1*V1)+H1)+H2*((U2*U2+V2*V2)+H2))
          AENS=AENS+0.5D0*Y2*(Q1*Q1/H1+Q2*Q2/H2)
          IF(H1.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J1,H1
          END IF
          IF(H2.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J2,H2
          END IF
        END DO
      END DO
      AM1=AM1/IM
      AM2=AM2/IM
      AM3=AM3/IM            
      AENE=AENE/IM
      AENS=AENS/IM

      END
