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
*     INITIALIZATION OF SNPACK                                  99/03/20
************************************************************************
      SUBROUTINE SNINIT(MM,IM,JM,IT,T,Y,IP,P,R,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(-(MM+1)/2:MM,2)
      DIMENSION P(-(MM+1)/2:MM,2,JM/2)
      DIMENSION R(-(MM+1)/2:(MM+1)/2+2,0:MM/2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

*/ FFTに使う配列の初期化 /*

      CALL SNINI1(IM,IT,T)

*/ ガウス緯度およびウェイトの初期化 /*

      CALL SNINI2(JM,Y)

*/ Legendre陪関数計算のための係数および初期値の設定 /*

      CALL SNINI3(MM,JM,Y,IP,P,R,IA,A)

      END
************************************************************************
*     FFTに使う配列の初期化
************************************************************************
      SUBROUTINE SNINI1(IM,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(IM*2)

      CALL FTTRUI(IM,IT,T)

      END
************************************************************************
*     ガウス緯度およびウェイトの初期化
************************************************************************
      SUBROUTINE SNINI2(JM,Y)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM/2,4)

      IF(MOD(JM,2).NE.0) THEN
        CALL BSDMSG('E','SNINI2','JM MUST BE EVEN.')
      END IF

      JH=JM/2

      CALL SNGAUS(JM,Y(1,1),Y(1,2))

      DO J=1,JH
        Y(J,3)=SQRT(1-Y(J,1)*Y(J,1))
        Y(J,4)=1/Y(J,3)
      END DO

      END
************************************************************************
*     Legendre陪関数計算のための係数および初期値の設定
************************************************************************
      SUBROUTINE SNINI3(MM,JM,Y,IP,P,R,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(-(MM+1)/2:MM,2),IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION P(-(MM+1)/2:MM,2,JM/2)
      DIMENSION R(-(MM+1)/2:(MM+1)/2+2,0:MM/2)

      EPSL(N,M)=SQRT((1D0*N*N-M*M)/(4D0*N*N-1))

*/ 便宜上用いる定数の設定

      MMD=MM/2
      MMP=(MM+1)/2
      JH=JM/2

*/    漸化式に使う係数(準備)  /*

      CALL BSSET0((2*MMP+3)*(MMD+1),R)

      DO N=0,MM
        CALL SNNMKL(MM,N,N,K,L)
        R(K,L)=1
      END DO

      DO N=1,MM
        M=N-1
        CALL SNNMKL(MM,N,M,K,L)
        R(K,L)=1/EPSL(N,M)
      END DO

      DO M=0,MM-2
        DO N=M+2,MM
          CALL SNNMKL(MM,N,M,K,L)
          CALL SNNMKL(MM,N-2,M,KD,LD)
          R(K,L)=-R(KD,LD)*EPSL(N-1,M)/EPSL(N,M)
        END DO
      END DO

*/    スペクトル変数格納位置変換のためのリストベクトル  /*

      DO N=0,MM
        DO M=-N,N
          CALL SNNM2L(N,M,L)
          MD=ABS(M)
          CALL SNNMKL(MM,N,MD,K,LD)
          A(L,1)=R(K,LD)
          IF(M.GE.0) THEN
            CALL SNKLIA(MM,K,LD,1,IA(L,1))
          ELSE
            CALL SNKLIA(MM,K,LD,2,IA(L,1))
          END IF
        END DO
      END DO

*/    リストベクトルおよび係数(経度微分用) /*

      DO N=0,MM
        DO M=-N,N
          CALL SNNM2L(N,M,L)
          MD=ABS(M)
          CALL SNNMKL(MM,N,MD,K,LD)
          A(L,2)=M*A(L,1)
          IF(M.GE.0) THEN
            CALL SNKLIA(MM,K,LD,2,IA(L,2))
          ELSE
            CALL SNKLIA(MM,K,LD,1,IA(L,2))
          END IF
        END DO
      END DO

*/    リストベクトル(緯度微分用) /*

      DO N=0,MM
        DO M=-N,N
          CALL SNNM2L(N,M,L)
          MD=ABS(M)
          CALL SNNMKL(MM,N,MD,K,LD)
          CALL SNNMKL(MM,N-1,MD,K1,LD1)
          CALL SNNMKL(MM,N+1,MD,K2,LD2)
          IF(M.GE.0) THEN
            CALL SNKLIA(MM,K1,LD1,1,IA(L,3))
            CALL SNKLIA(MM,K2,LD2,1,IA(L,4))
          ELSE
            CALL SNKLIA(MM,K1,LD1,2,IA(L,3))
            CALL SNKLIA(MM,K2,LD2,2,IA(L,4))
          END IF
          A(L,6)=(-1)**(N-MD)/R(K,LD)
          A(L,4)=-N*A(L,6)
          IF(MD.EQ.N) THEN
            A(L,3)=0
            A(L,5)=0
          ELSE
            A(L,5)=-(-1)**(N-MD)/R(K,LD)
            A(L,3)=(N+1)*A(L,5)
          END IF
        END DO
      END DO

*/    漸化式に使う係数  /*

      DO N=0,MM
        DO M=0,N
          CALL SNNMKL(MM,N,M,K,L)
          R(K,L)=(-1)**(N-M)*R(K,L)*R(K,L)
        END DO
      END DO

*/    Legendre陪関数の初期値(マイナス側)  /*
*/    (マイナス側はn=MM/2までの P^m_m成分, ただし, M=m-1)
*/    3次元目の添字は 1: ベースの初期値, 2: ベースからnを一つ増した初期値

      DO J=1,JH
        P(-1,1,J)=1
      END DO

      DO M=1,MMP-1
        ALPHA=SQRT(1D0*(2*M+1)/(2*M))
        DO J=1,JH
          P(-M-1,1,J)=ALPHA*P(-M,1,J)*Y(J,3)
        END DO
      END DO

      DO M=0,MMP-1
        DO J=1,JH
          P(-M-1,2,J)=Y(J,1)*P(-M-1,1,J)
        END DO
      END DO
      
*/    Legendre陪関数の初期値のプラス側の計算の準備  /*

      DO J=1,JH
        P(0,1,J)=1
      END DO

      DO M=1,MM
        ALPHA=SQRT(1D0*(2*M+1)/(2*M))
        DO J=1,JH
          P(M,1,J)=ALPHA*P(M-1,1,J)*Y(J,3)
        END DO
      END DO

*/    Legendre陪関数の初期値のプラス側の半分の計算  /*

      DO J=1,JH
        DO M=0,MMP
          P0=0
          P1=P(M,1,J)
          DO N=M+1,MMP,2
            CALL SNNMKL(MM,N-1,M,K,L)
            P0=P0+R(K,L)*P1*Y(J,1)
            CALL SNNMKL(MM,N,M,K,L)
            P1=P1+R(K,L)*P0*Y(J,1)
          END DO
          IF(MOD(MMP-M,2).EQ.1) THEN
            P(M,1,J)=P0
            P(M,2,J)=P1
          ELSE
            N=MMP
            CALL SNNMKL(MM,N,M,K,L)
            P0=P0+R(K,L)*P1*Y(J,1)
            P(M,1,J)=P1
            P(M,2,J)=P0
          END IF
        END DO
      END DO

*/    Legendre陪関数の初期値のプラス側の残りの半分の計算  /*
*/    (漸化式のために交互に0を入れておく /*

      DO J=1,JH
        DO M=MMP+1,MM-1,2
          P(M,2,J)=P(M,1,J)
          P(M,1,J)=0
          P(M+1,2,J)=0
        END DO
        IF(MOD(MM-MMP,2).NE.0) THEN
          P(MM,2,J)=P(M,1,J)
          P(MM,1,J)=0
        END IF
      END DO

*/    パリティ変数  /*

      DO M=-MMP,MM
        IP(M,1)=1
      END DO
      
      IF(MOD(MMP,2).EQ.0) THEN
        DO M=1,MM,2
          IP(M,1)=-1
        END DO
      ELSE
        DO M=0,MM,2
          IP(M,1)=-1
        END DO
      END IF

      DO M=-MMP,MM
        IP(M,2)=IP(M,1)
      END DO

      END
************************************************************************
      SUBROUTINE SNKLNM(MM,K,L,N,M)

      MMP=(MM+1)/2

      IF(K.GE.-L) THEN
        N=L+MMP
        M=K+L
      ELSE
        N=-K-1
        M=-K-L-1
      END IF

      END
************************************************************************
      SUBROUTINE SNNMKL(MM,N,M,K,L)

      MMP=(MM+1)/2
      MMD=MM/2

      IF(M.GT.N) THEN
        IF(N+1.LE.MMD) THEN
          K=-MMP-1
          L=N+1
        ELSE
          K=MMP+1
          L=N-MMD
        END IF
      ELSE
        IF(N.GE.MMP) THEN
          L=N-MMP
          K=M-L
        ELSE
          L=N-M
          K=-N-1
        END IF
      END IF

      END
************************************************************************
      SUBROUTINE SNKLIA(MM,K,L,I,IA)

      MMP=(MM+1)/2
      MMD=MM/2

      ND1=MMP*2+3
      ND2=MMD+2

      IA=1+((I-1)*ND2+L)*ND1+K+MMP+1

      END
