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
*     浅水方程式の非線形項の計算
*     (角運動量を保存する散逸も含む)
*     (MPIによる安直並列化)                                   2002/05/20
************************************************************************
      SUBROUTINE SPMWNV(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     & AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,W)

      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
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
      DIMENSION W((MM+1)*(MM+1),3)      

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

      LM=(MM+1)*(MM+1)      

*/ 実際の操作 */

      IF(JM.EQ.0) THEN
        CALL BSSET0((MM+1)*(MM+1)*3,W)
      ELSE
        CALL SPMWSV(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &    AVT,DIV,PHI,W(1,1),W(1,2),W(1,3),
     &    RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)
      END IF
      
      CALL MPI_ALLREDUCE(W(1,1),DAVT,LM,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(W(1,2),DDIV,LM,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
      CALL MPI_ALLREDUCE(W(1,3),DPHI,LM,
     &  MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

*/ 回転系に戻るために以下の変換が必要
      
      DO L=1,LM
        DAVT(L)=DAVT(L)-OMEGA*IRM(L,2)*AVT(IRM(L,1))
        DDIV(L)=DDIV(L)-OMEGA*IRM(L,2)*DIV(IRM(L,1))
        DPHI(L)=DPHI(L)-OMEGA*IRM(L,2)*PHI(IRM(L,1))
      END DO
      DDIV(7)=DDIV(7)-OMEGA*OMEGA*2/SQRT(5D0)

      END
************************************************************************
      SUBROUTINE SPMWSV(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
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
      DIMENSION WS(MAXDIM,8)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      DNUD=DNU/BARPHI

*/ ψとχの計算(一時的に, ψとしてWS(*,3)を, χとしてWS(*,4)を使う)
*/ 静止系から見ることにする.
      
      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))      
      DO L=1,LM
        WS(L,3)=RN(L,2)*AVT(L)
        WS(L,4)=RN(L,2)*DIV(L)
      END DO

*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*WS(L,4)
        WS(IA(L,3),7)=-A(L,3)*WS(L,3)
        WS(IA(L,4),8)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,7)+WS(L,8)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WS(L,3)
        WS(IA(L,3),7)=A(L,3)*WS(L,4)
        WS(IA(L,4),8)=A(L,4)*WS(L,4)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,7)+WS(L,8)
      END DO

*/ ∂Φ/∂λ(WS(*,3)), cosφ∂Φ/∂φ(WS(*,4))の計算

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,3),3)=A(L,3)*PHI(L)
        WS(IA(L,4),4)=A(L,4)*PHI(L)
      END DO
      DO L=1,LMD
        WS(L,4)=WS(L,4)+WS(L,3)
      END DO

      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(IA(L,2),3)=A(L,2)*PHI(L)
      END DO

*/ ｑ(WS(*,5))とＤ(WS(*,6))とΦ(WS(*,7))と△Φ(WS(*,8))の計算の準備

      CALL BSSET0(LMD,WS(1,5))
      CALL BSSET0(LMD,WS(1,6))
      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))      
      DO L=1,LM
        WS(IA(L,1),5)=A(L,1)*AVT(L)
        WS(IA(L,1),6)=A(L,1)*DIV(L)        
        WS(IA(L,1),7)=A(L,1)*PHI(L)
        WS(IA(L,1),8)=A(L,1)*RN(L,1)*PHI(L)
      END DO

*/ スペクトル→グリッド
      DO IV=1,4
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,1)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

      DO IV=5,8
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        PHID=WS(IJ,7)-BARPHI
        WS(IJ,1)=WS(IJ,5)*U
     &   +DNUD*( 2*WS(IJ,8)*V+WS(IJ,5)*WS(IJ,3)+ALPHA*WS(IJ,6)*WS(IJ,4))
        WS(IJ,2)=WS(IJ,5)*V
     &   +DNUD*(-2*WS(IJ,8)*U+WS(IJ,5)*WS(IJ,4)-ALPHA*WS(IJ,6)*WS(IJ,3))
        WS(IJ,3)=PHID*U
        WS(IJ,4)=PHID*V
        WS(IJ,5)=PHID*WS(IJ,5)        
        WS(IJ,6)=(U*U+V*V)*0.5D0+DNUD*ALPHA*PHID*WS(IJ,6)
      END DO
      
*/ グリッド→スペクトル(IV=5,6の成分だけは SNPG2Sの変換が異なるので注意)
      
      DO IV=1,4
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,1)
*/      ルジャンドル変換
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO
 
      DO IV=5,6
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,0)
*/      ルジャンドル変換
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO

      DO L=1,LM
        DAVT(L)=A(L,2)*WS(IA(L,2),1)
     &    +A(L,3)*WS(IA(L,3),2)+A(L,4)*WS(IA(L,4),2)
     &    -DNUD*RN(L,1)*A(L,1)*WS(IA(L,1),5)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),4)
     &    +A(L,3)*WS(IA(L,3),3)+A(L,4)*WS(IA(L,4),3))
        DDIV(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),6)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),3)
     &    -A(L,3)*WS(IA(L,3),4)-A(L,4)*WS(IA(L,4),4))
        DPHI(L)=A(L,2)*WS(IA(L,2),3)
     &    +A(L,3)*WS(IA(L,3),4)+A(L,4)*WS(IA(L,4),4)
      END DO

      END
