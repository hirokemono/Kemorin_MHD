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
*     (角運動量を保存する高階粘性項も含む)
*     (若干高速化を図り, さらに ALPHA=0の場合に限定した版)
*     (MPIによる安直並列化)                                   2002/05/20
************************************************************************
      SUBROUTINE SPMWHX(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,LEV,
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
        CALL SPMWHT(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,
     &    LEV,AVT,DIV,PHI,W(1,1),W(1,2),W(1,3),
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
      SUBROUTINE SPMWHT(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,
     & LEV,AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

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
      DIMENSION WW(MAXDIM,10)
      DIMENSION WS(MAXDIM,10)

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

*/ u^, v^ の計算の準備

      DO I=1,LEV
        DO L=1,LM
          WS(L,3)=-(RN(L,1)+2)*WS(L,3)
          WS(L,4)=-(RN(L,1)+2)*WS(L,4)
        END DO
      END DO

*/ u^の計算(WS(*,5))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,5))
      DO L=1,LM
        WS(IA(L,2),5)=A(L,2)*WS(L,4)
        WS(IA(L,3),7)=-A(L,3)*WS(L,3)
        WS(IA(L,4),8)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,5)=WS(L,5)+WS(L,7)+WS(L,8)
      END DO

*/ v^の計算(WS(*,6))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,6))
      DO L=1,LM
        WS(IA(L,2),6)=A(L,2)*WS(L,3)
        WS(IA(L,3),7)=A(L,3)*WS(L,4)
        WS(IA(L,4),8)=A(L,4)*WS(L,4)
      END DO
      DO L=1,LMD
        WS(L,6)=WS(L,6)+WS(L,7)+WS(L,8)
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

*/ ｑ(WS(*,7))とΦ(WS(*,8))と△Φ(WS(*,9))とq^(WS(*,10))
*/          の計算の準備

      DO L=1,LM
        WS(L,7)=AVT(L)
      END DO
      
      DO I=1,LEV
        DO L=1,LM
          WS(L,7)=-(RN(L,1)+2)*WS(L,7)
        END DO
      END DO

      CALL BSSET0(LMD,WS(1,10))
      DO L=1,LM
        WS(IA(L,1),10)=A(L,1)*WS(L,7)
      END DO
      
      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,9))      
      DO L=1,LM
        WS(IA(L,1),7)=A(L,1)*AVT(L)
        WS(IA(L,1),8)=A(L,1)*PHI(L)
        WS(IA(L,1),9)=A(L,1)*RN(L,1)*PHI(L)
      END DO

*/  ルジャンドル変換
      
      CALL SPLSGA(MM,JM,1,
     &  WS(1,1),WS(1,2),WS(1,3),WS(1,4),WS(1,5),WS(1,6),WS(1,7),WS(1,8),
     &  WS(1,9),WS(1,10),
     &  WW(1,1),WW(1,2),WW(1,3),WW(1,4),WW(1,5),WW(1,6),WW(1,7),WW(1,8),
     &  WW(1,9),WW(1,10),
     &  Y,P,R,Q)

*/ スペクトル→グリッド
      DO IV=1,6
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW(1,IV),WS(1,IV),IP,Y,1)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW(1,IV),IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW(1,IV),WS(1,IV))
      END DO

      DO IV=7,10
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW(1,IV),WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW(1,IV),IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW(1,IV),WS(1,IV))
      END DO

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        PHID=WS(IJ,8)-BARPHI
        WS(IJ,1)=WS(IJ,7)*U
     &    +DNUD*( 2*WS(IJ,9)*WS(IJ,6)+WS(IJ,10)*WS(IJ,3) )
        WS(IJ,2)=WS(IJ,7)*V
     &    +DNUD*(-2*WS(IJ,9)*WS(IJ,5)+WS(IJ,10)*WS(IJ,4) )
        WS(IJ,3)=PHID*U
        WS(IJ,4)=PHID*V
        WS(IJ,5)=PHID*WS(IJ,5)
        WS(IJ,6)=PHID*WS(IJ,6)
        WS(IJ,7)=PHID*WS(IJ,10)        
        WS(IJ,8)=(U*U+V*V)*0.5D0
      END DO

*/ グリッド→スペクトル(IV=5,6の成分だけは SNPG2Sの変換が異なるので注意)
      
      DO IV=1,6
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW(1,IV))
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW(1,IV),WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW(1,IV),IP,Y,1)
      END DO
 
      DO IV=7,8
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW(1,IV))
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW(1,IV),WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW(1,IV),IP,Y,0)
      END DO

*/      ルジャンドル変換      

      CALL SPLGS8(MM,JM,1,
     &  WW(1,1),WW(1,2),WW(1,3),WW(1,4),WW(1,5),WW(1,6),WW(1,7),WW(1,8),
     &  WS(1,1),WS(1,2),WS(1,3),WS(1,4),WS(1,5),WS(1,6),WS(1,7),WS(1,8),
     &  Y,P,R,Q)
 
      DO L=1,LM
        DAVT(L)=A(L,2)*WS(IA(L,2),1)
     &    +A(L,3)*WS(IA(L,3),2)+A(L,4)*WS(IA(L,4),2)
     &    -DNUD*RN(L,1)*A(L,1)*WS(IA(L,1),7)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),6)
     &    +A(L,3)*WS(IA(L,3),5)+A(L,4)*WS(IA(L,4),5))
        DDIV(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),8)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),5)
     &    -A(L,3)*WS(IA(L,3),6)-A(L,4)*WS(IA(L,4),6))
        DPHI(L)=A(L,2)*WS(IA(L,2),3)
     &    +A(L,3)*WS(IA(L,3),4)+A(L,4)*WS(IA(L,4),4)
      END DO

      END
