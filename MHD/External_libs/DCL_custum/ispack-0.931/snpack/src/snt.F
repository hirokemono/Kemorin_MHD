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
*     SPECTRAL TRANSFORM                                      1999/03/04
************************************************************************
*     TRANSFORM SPECTRA TO GRID
*-----------------------------------------------------------------------
*     WS, WW はあちこちで作業領域として使用されるため,
*
*        KM*MAX(((MM+1)/2*2+3)*(MM/2+2)*2,JD*((MM+1)/2+MM+1)*2,JD*IM)
*
*     を満す大きさであること. または, さらに単純には, やや余裕をもって,
*
*        KM*MAX((MM+4)*(MM+3),JD*3*(MM+1),JD*IM)
*
*     としておいてもよい. ただし, Fortran90などで動的に領域を確保するの
*     でない限り, PARAMETER 文中でこのような値を MM,JD,IM から自動的に
*     設定できるようにするのは難しい( MAX のような関数が使えないため).
*     
*     しかし, 実際的な場合を考えると, 以下のように簡単に設定できる場合が
*     多い筈である. (安全のため, MM≧3 としておく).
*
*     1. 逆変換→正変換 で元のスペクトルデータへの復元が保証される条件
*        ( JD≧JM≧MM+1, IM≧2*MM+2 (IMは偶数より) )が満されている場合.
*
*        IW=IM+MM+1 とし, 作業領域を IW*JD*KM なる大きさにとればよい.
*
*     2. 2次の非線形項からのエリアジングを除く条件
*        ( JD≧JM≧3*MM/2D0, IM≧3*MM+1 )が満されている場合.
*
*        IW=IM+2 とし, 作業領域を IW*JD*KM なる大きさにとればよい.
*
*     3. 1,2のどちらの場合でもないが, メモリが潤沢にあり, 多少余分な領域
*        を使っても構わない場合.
*
*        IW=IM+3*(MM+1)とし, 作業領域を IW*JD*KM なる大きさにとればよい.
************************************************************************
      SUBROUTINE SNTS2G(MM,IM,ID,JM,JD,KM,
     &  S,G,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION G(ID*JD*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*),WW(*)

*/ スペクトルの詰め替え
      CALL SNCS2G(MM,KM,S,WS,IA,A,WW,IFLAG)
      
*/ ルジャンドル変換
      CALL SNLS2G(MM,JM,KM,WS,WW,Y,P,R,Q)

*/ パリティ変換
      CALL SNPS2G(MM,JM,JD,KM,WW,WS,IP,Y,IPOW)

*/ フーリエ変換
      CALL SNFS2G(MM,IM,JD,KM,WS,WW,IT,T)

*/ 添字の並べ替え
      CALL SNGS2G(IM,ID,JD,KM,WW,G)

      END
************************************************************************
      SUBROUTINE SNCS2G(MM,KM,S,WS,IA,A,WW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(KM,((MM+1)/2*2+3)*(MM/2+2)*2)

      LM=(MM+1)*(MM+1)
      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,1))=A(L,1)*S(L,K)
          END DO
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,2))=A(L,2)*S(L,K)
          END DO
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
          DO L=1,LM
            WW(K,IA(L,3))=A(L,3)*S(L,K)
          END DO
        END DO
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,4))=A(L,4)*S(L,K)
          END DO
        END DO
        CALL SNBADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      ELSE IF(IFLAG.EQ.2) THEN
        CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
          DO L=1,LM
            WW(K,IA(L,3))=A(L,5)*S(L,K)
          END DO
        END DO
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,4))=A(L,6)*S(L,K)
          END DO
        END DO
        CALL SNBADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      END IF

      END
************************************************************************
      SUBROUTINE SNBADD(N,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N)

      DO I=1,N
        A(I)=A(I)+B(I)
      END DO

      END
************************************************************************
      SUBROUTINE SNGS2G(IM,ID,JD,KM,WW,G)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WW(JD*KM,IM/2,2)
      DIMENSION G(ID,JD*KM)

      DO I=1,IM/2
        DO J=1,JD*KM
          G(2*I-1,J)=WW(J,I,1)
          G(2*I,J)=WW(J,I,2)
        END DO
      END DO

      DO I=IM+1,ID
        DO J=1,JD*KM
          G(I,J)=WW(J,1,1)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNFS2G(MM,IM,JD,KM,S,W,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT05=0.7071067811865475244D0)
      DIMENSION S(JD*KM,-(MM+1)/2:MM,2)
      DIMENSION W(JD*KM,0:IM/2-1,2)
      DIMENSION IT(5),T(0:IM/2-1,4)

      MMP=(MM+1)/2
      IH=IM/2
      A=SQRT05

*/ MM がもし IH-1 以上であっても, 波数 IH-1 成分までしか使わない
      
      MD=MIN(MM,IH-1)
      
*/ ベクトル化のために退避していた成分の合成

      DO M=0,MMP-1
        DO J=1,JD*KM
          S(J,M,1)=S(J,M,1)+S(J,-M-1,1)
          S(J,M,2)=S(J,M,2)+S(J,-M-1,2)
        END DO
      END DO
      
*/ 波数零成分

      DO J=1,JD*KM
        W(J,0,1)=S(J,0,1)
        W(J,0,2)=S(J,0,1)
      END DO

*/ 低波数側のみの寄与

      DO M=1,IH-MD-1
        DO J=1,JD*KM
          W(J,M,1)=A*(S(J,M,1)-T(M,3)*S(J,M,2)-T(M,4)*S(J,M,1))
          W(J,M,2)=A*(S(J,M,2)+T(M,3)*S(J,M,1)-T(M,4)*S(J,M,2))
        END DO
      END DO

*/ 両側が重なってる場合

      DO M=IH-MD,MD
        DO J=1,JD*KM
          W(J,M,1)=A*(        (S(J,IH-M,1)+S(J,M,1))
     &                -T(M,3)*(S(J,IH-M,2)+S(J,M,2))
     &                +T(M,4)*(S(J,IH-M,1)-S(J,M,1)))
          W(J,M,2)=A*(       -(S(J,IH-M,2)-S(J,M,2))
     &                -T(M,3)*(S(J,IH-M,1)-S(J,M,1))
     &                -T(M,4)*(S(J,IH-M,2)+S(J,M,2)))
        END DO
      END DO

*/ 高波数側のみの寄与

      DO M=MAX(IH-MD,MD+1),IH-1
        DO J=1,JD*KM
          W(J,M,1)=A*( S(J,IH-M,1)-T(M,3)*S(J,IH-M,2)
     &                            +T(M,4)*S(J,IH-M,1))
          W(J,M,2)=A*(-S(J,IH-M,2)-T(M,3)*S(J,IH-M,1)
     &                            -T(M,4)*S(J,IH-M,2))
        END DO
      END DO

*/ 両方から寄与が無い場合

      DO M=MD+1,IH-MD-1
        DO J=1,JD*KM
          W(J,M,1)=0
          W(J,M,2)=0
        END DO
      END DO

      CALL FTTZLM(JD*KM,IH,W,S,IT,T)

      END
************************************************************************
      SUBROUTINE SNPS2G(MM,JM,JD,KM,W,S,IP,Y,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y(JM/2,4)

      JH=JM/2
      MMP=(MM+1)/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(JH+J,K)=        (W(K,J,1)+W(K,J,2))*Y(J,4)**IPOW
          S(JH-J+1,K)=IP(K)*(W(K,J,1)-W(K,J,2))*Y(J,4)**IPOW
        END DO
      END DO

      DO J=JM+1,JD
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(J,K)=S(JM,K)
          S(J,K)=S(JM,K)
        END DO
      END DO

      END
***********************************************************************
      SUBROUTINE SNLS2G(MM,JM,KM,S,W,Y,P,R,Q)
*-----------------------------------------------------------------------
* 以下にある ifdef によるループの分解は, 今一つ賢くない SX4 のコンパイラ
* でも外側ループのアンローリングができるようにするためのもので, VPでは,
* 不要(むしろ有害)である.
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION W(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION Y(JM/2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3),0:MM/2)
      DIMENSION Q(KM*((MM+1)/2+MM+1),2,JM/2)

      MMP=(MM+1)/2
      MMD=MM/2
      JH=JM/2

      CALL BSCOPY(KM*((MM+1)/2+MM+1)*JM,P,Q)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W)

      L=0
      DO J=1,JH
        DO K=1,KM*(2*MMP+2)
          M=K+L*KM
          W(M,1,J,1)=W(M,1,J,1)+S(K+KM,L,1)*Q(M,1,J)
          W(M,2,J,1)=W(M,2,J,1)+S(K+KM,L,2)*Q(M,1,J)
          W(M,1,J,2)=W(M,1,J,2)+S(K,L+1,1)*Q(M,2,J)
          W(M,2,J,2)=W(M,2,J,2)+S(K,L+1,2)*Q(M,2,J)
        END DO
      END DO
      DO L=2,MMD-1,2
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W(M,1,J,1)=W(M,1,J,1)+S(K+KM,L,1)*Q(M,1,J)
            W(M,2,J,1)=W(M,2,J,1)+S(K+KM,L,2)*Q(M,1,J)
#ifdef SPLIT
          END DO
        END DO
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
#endif
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W(M,1,J,2)=W(M,1,J,2)+S(K,L+1,1)*Q(M,2,J)
            W(M,2,J,2)=W(M,2,J,2)+S(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      END DO
      IF(MOD(MMD,2).EQ.0) THEN
        L=MMD
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W(M,1,J,1)=W(M,1,J,1)+S(K+KM,L,1)*Q(M,1,J)
            W(M,2,J,1)=W(M,2,J,1)+S(K+KM,L,2)*Q(M,1,J)
#ifdef SPLIT
          END DO
        END DO
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
#endif
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W(M,1,J,2)=W(M,1,J,2)+S(K,L+1,1)*Q(M,2,J)
            W(M,2,J,2)=W(M,2,J,2)+S(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      ELSE
        L=MMD+1
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM-KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K,L-1)*Q(M,2,J)
            W(M,1,J,1)=W(M,1,J,1)+S(K,L,1)*Q(M,1,J)
            W(M,2,J,1)=W(M,2,J,1)+S(K,L,2)*Q(M,1,J)
          END DO
        END DO
      END IF

      END
************************************************************************
*     TRANSFORM SPECTRA TO GRID
*-----------------------------------------------------------------------
*     作業領域 WS,WW の大きさの設定については SNTS2Gを参照のこと.
************************************************************************
      SUBROUTINE SNTG2S(MM,IM,ID,JM,JD,KM,
     &  G,S,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID*JD*KM)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1))
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*),WW(*)

      LM=(MM+1)*(MM+1)

*/ 添字の並べ替え
      CALL SNGG2S(IM,ID,JD,KM,G,WW)

*/ フーリエ変換
      CALL SNFG2S(MM,IM,JD,KM,WW,WS,IT,T)

*/ パリティ変換
      CALL SNPG2S(MM,JM,JD,KM,WS,WW,IP,Y,IPOW)

*/ ルジャンドル変換
      CALL SNLG2S(MM,JM,KM,WW,WS,Y,P,R,Q)

*/ スペクトルの詰め替え
      CALL SNCG2S(MM,KM,WS,S,IA,A,IFLAG)

      END
************************************************************************
      SUBROUTINE SNCG2S(MM,KM,WS,S,IA,A,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=A(L,1)*WS(K,IA(L,1))
          END DO
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=-A(L,2)*WS(K,IA(L,2))
          END DO
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=-A(L,3)*WS(K,IA(L,3))-A(L,4)*WS(K,IA(L,4))
          END DO
        END DO
      ELSE IF(IFLAG.EQ.2) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=A(L,5)*WS(K,IA(L,3))+A(L,6)*WS(K,IA(L,4))
          END DO
        END DO
      END IF

      END
************************************************************************
      SUBROUTINE SNGG2S(IM,ID,JD,KM,G,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID,JD*KM)
      DIMENSION WW(JD*KM,IM/2,2)

      DO I=1,IM/2
        DO J=1,JD*KM
          WW(J,I,1)=G(2*I-1,J)
          WW(J,I,2)=G(2*I,J)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNFG2S(MM,IM,JD,KM,W,S,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT2=1.4142135623730950488D0)
      DIMENSION W(JD*KM,0:IM/2-1,2)
      DIMENSION S(JD*KM,-(MM+1)/2:MM,2)
      DIMENSION IT(5),T(0:IM/2-1,4)

      MMP=(MM+1)/2
      IH=IM/2
      
*/ MM がもし IH-1 以上である場合には, 波数 IH-1 成分までしか格納しない.
*/ それ以上の波数成分には零を格納する.      
      
      MD=MIN(MM,IH-1)

      CALL FTTZLM(JD*KM,IM/2,W,S,IT,T)

      A1=1D0/IM
*/ 以下のSQRT2は正規化のため.
      A2=A1/2*SQRT2
      
      DO J=1,JD*KM
        S(J,0,1)=A1*(W(J,0,1)+W(J,0,2))
        S(J,0,2)=0
      END DO

      DO I=1,MD
        DO J=1,JD*KM
          S(J,I,1)=A2*(       (W(J,IH-I,1)+W(J,I,1))
     &                +T(I,3)*(W(J,IH-I,2)+W(J,I,2))
     &                -T(I,4)*(W(J,IH-I,1)-W(J,I,1)))
          S(J,I,2)=A2*(       (W(J,IH-I,2)-W(J,I,2))
     &                -T(I,3)*(W(J,IH-I,1)-W(J,I,1))
     &                -T(I,4)*(W(J,IH-I,2)+W(J,I,2)))
        END DO
      END DO
      
      DO I=MD+1,MM
        DO J=1,JD*KM
          S(J,I,1)=0
          S(J,I,2)=0
        END DO
      END DO

      DO M=0,MMP-1
        DO J=1,JD*KM
          S(J,-M-1,1)=S(J,M,1)
          S(J,-M-1,2)=S(J,M,2)
        END DO
      END DO

      END
************************************************************************      
      SUBROUTINE SNPG2S(MM,JM,JD,KM,S,W,IP,Y,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y(JM/2,4)

      JH=JM/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          W(K,J,1)=(S(JH+J,K)+IP(K)*S(JH-J+1,K))*(Y(J,2)*Y(J,4)**IPOW)
          W(K,J,2)=(S(JH+J,K)-IP(K)*S(JH-J+1,K))*(Y(J,2)*Y(J,4)**IPOW)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNLG2S(MM,JM,KM,W,S,Y,P,R,Q)
*-----------------------------------------------------------------------
* 以下にある ifdef によるループの分解は, 今一つ賢くない SX4 のコンパイラ
* でも外側ループのアンローリングができるようにするためのもので, VPでは,
* 不要(むしろ有害)である.
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION S(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION Y(JM/2)
      DIMENSION P(KM*((MM+1)/2+MM+1),2,JM/2)
      DIMENSION R(KM*((MM+1)/2*2+3),0:MM/2)
      DIMENSION Q(KM*((MM+1)/2+MM+1),2,JM/2)

      MMP=(MM+1)/2
      MMD=MM/2
      JH=JM/2

      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S)
      CALL BSCOPY(KM*((MM+1)/2+MM+1)*JM,P,Q)

      L=0
      DO J=1,JH
        DO K=1,KM*(2*MMP+2)
          M=K+L*KM
          S(K+KM,L,1)=S(K+KM,L,1)+W(M,1,J,1)*Q(M,1,J)
          S(K+KM,L,2)=S(K+KM,L,2)+W(M,2,J,1)*Q(M,1,J)
          S(K,L+1,1)=S(K,L+1,1)+W(M,1,J,2)*Q(M,2,J)
          S(K,L+1,2)=S(K,L+1,2)+W(M,2,J,2)*Q(M,2,J)
        END DO
      END DO
      DO L=2,MMD-1,2
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            S(K+KM,L,1)=S(K+KM,L,1)+W(M,1,J,1)*Q(M,1,J)
            S(K+KM,L,2)=S(K+KM,L,2)+W(M,2,J,1)*Q(M,1,J)
#ifdef SPLIT
          END DO
        END DO
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
#endif
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            S(K,L+1,1)=S(K,L+1,1)+W(M,1,J,2)*Q(M,2,J)
            S(K,L+1,2)=S(K,L+1,2)+W(M,2,J,2)*Q(M,2,J)
          END DO
        END DO
      END DO
      IF(MOD(MMD,2).EQ.0) THEN
        L=MMD
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            S(K+KM,L,1)=S(K+KM,L,1)+W(M,1,J,1)*Q(M,1,J)
            S(K+KM,L,2)=S(K+KM,L,2)+W(M,2,J,1)*Q(M,1,J)
#ifdef SPLIT
          END DO
        END DO
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
#endif
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            S(K,L+1,1)=S(K,L+1,1)+W(M,1,J,2)*Q(M,2,J)
            S(K,L+1,2)=S(K,L+1,2)+W(M,2,J,2)*Q(M,2,J)
          END DO
        END DO
      ELSE
        L=MMD+1
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM-KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K,L-1)*Q(M,2,J)
            S(K,L,1)=S(K,L,1)+W(M,1,J,1)*Q(M,1,J)
            S(K,L,2)=S(K,L,2)+W(M,2,J,1)*Q(M,1,J)
          END DO
        END DO
      END IF

      END
