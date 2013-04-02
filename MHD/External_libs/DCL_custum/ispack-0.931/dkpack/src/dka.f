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
*     DKPACK: ヤコビ多項式を用いた円盤領域のスペクトル法
*                                                  2003/04/11 K. Ishioka
*      履歴:  2003/02/03  DKALN0 のバグを修正
************************************************************************
*     DKA*** の初期化 (DKA*** は非粘性浅水方程式のためのルーチン群)
*-----------------------------------------------------------------------      
      SUBROUTINE DKAINI(MM,JM,IM,IT,T,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,(MM+8)*MM/4+1)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)
      DIMENSION T(IM*2),IT(5)
      
      CALL FTTRUI(IM,IT,T)
      CALL DKAIN0(MM,JM,P,A)

      END
************************************************************************
*     DKA*** の初期化(配列 P,A のみについて)
*-----------------------------------------------------------------------      
*     MM.GE.2 であること.
*     P(J,1,1) にはガウス分点の座標, P(J,1,2) にはウェイトを格納する.
*-----------------------------------------------------------------------      
      SUBROUTINE DKAIN0(MM,JM,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,(MM+8)*MM/4+1)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)

      IP=1
      M=0
      NM=MM/2
      
      CALL DKGAUS(JM,P(1,1,NM+2),P(1,2,NM+2))
      CALL DKJACB(JM,NM,M,0,P(1,1,NM+2),P)
      CALL BSCOPY(JM*2,P(1,1,NM+2),P)
      
      DO N=0,NM
        A(N+1)=1D0/2
      END DO
      DO N=1,NM
        A(NM+N+1)=N/(2*SQRT(4D0*N*N-1))
      END DO
      DO N=1,NM
        A(2*NM+N+1)=1D0/2*(N+1)/N
      END DO
      DO N=2,NM
        A(3*NM+N)=-(N+1)/(2*SQRT(4D0*N*N-1))
      END DO
      
      IP=IP+NM+2
      IA=1+4*NM
      
      DO M=1,MM
        NM=(MM-M)/2
        CALL DKJACB(JM,NM,M,2,P,P(1,1,IP))
        CALL BSCOPY(JM*2,P(1,1,IP+NM),P(1,1,IP-1))
        DO J=1,JM
          P(J,1,IP+NM)=P(J,1,1)**(M/2D0)
          P(J,2,IP+NM)=(M/2D0)*P(J,1,1)**(M/2D0-1)
        END DO
        DO N=0,NM-1
          R=(2*N+M+3)/(1D0*(N+1)*(N+2)*(N+M+1)*(N+M+2))
          R=SQRT(R)
          DO J=1,JM
            P(J,1,IP+NM)=P(J,1,IP+NM)-R*P(J,1,IP+N)
            P(J,2,IP+NM)=P(J,2,IP+NM)-R*P(J,2,IP+N)
          END DO
        END DO
        S=SQRT(1D0*(NM+1)*(NM+M+1))
        DO J=1,JM
          P(J,1,IP+NM)=S*P(J,1,IP+NM)
          P(J,2,IP+NM)=S*P(J,2,IP+NM)
        END DO
        DO N=0,NM-1
          A(IA+N)=(1+(M*M-4D0)/((2*N+M+2)*(2*N+M+4)))/2
        END DO
        A(IA+NM)=1-1D0/(2*NM+M+2)
        A(IA+NM+1)=2D0*(2*NM+M+3)/M
        A(IA+2*NM+2)=SQRT((2*NM+M+3)/(1D0*(NM+2)*(NM+M+2)))        
        DO N=1,NM-1
          A(IA+NM+N+1)=1D0/(2*N+M+2)
     &      *SQRT((1D0*N*(N+M)*(N+2)*(N+M+2))/((2*N+M+1)*(2*N+M+3)))
        END DO
        IF(NM.GE.1) THEN
          A(IA+2*NM+1)=SQRT(1D0*NM*(NM+M)/(2*NM+M+1))/(2*NM+M+2)
          N=NM
          A(IA+2*NM+3+NM-1)=A(IA+NM+1)
     &      *SQRT(1D0*(2*N+M+1)*N*(N+2)/((2*N+M+3)*(N+M)*(N+M+2)))
          DO N=NM-1,1,-1
            A(IA+2*NM+3+N-1)=-A(IA+2*NM+3+N)
     &        *SQRT(1D0*(2*N+M+1)*N*(N+2)/((2*N+M+3)*(N+M)*(N+M+2)))          
          END DO
        END IF
        IP=IP+NM+2
        IA=IA+3*NM+3
      END DO

      DO J=1,JM
        P(J,1,1)=SQRT(P(J,1,1))
      END DO

* AのLU分解

      M=0
      IA=1
      NM=MM/2
      DO N=1,NM
        A(IA+N)=A(IA+N)-A(IA+NM+N)**2/A(IA+N-1)
      END DO
      DO N=0,NM
        A(IA+N)=1/A(IA+N)
      END DO
      DO N=1,NM
        A(IA+NM+N)=-A(IA+NM+N)*A(IA+N-1)
      END DO
      DO N=2,NM
        A(IA+2*NM+N)=A(IA+2*NM+N)-A(IA+3*NM+N-1)**2/A(IA+2*NM+N-1)
      END DO
      DO N=1,NM
        A(IA+2*NM+N)=1/A(IA+2*NM+N)
      END DO
      DO N=2,NM
        A(IA+3*NM+N-1)=-A(IA+3*NM+N-1)*A(IA+2*NM+N-1)
      END DO
      IA=IA+4*NM
      DO M=1,MM
        NM=(MM-M)/2
        DO N=1,NM+1
          A(IA+N)=A(IA+N)-A(IA+NM+N+1)**2/A(IA+N-1)
        END DO
        DO N=0,NM+1
          A(IA+N)=1/A(IA+N)
        END DO
        DO N=1,NM+1
          A(IA+NM+N+1)=-A(IA+NM+N+1)*A(IA+N-1)
        END DO
        IA=IA+3*NM+3
      END DO
        
      END
************************************************************************
*     傾度風バランスした水深場を求める
*-----------------------------------------------------------------------
      SUBROUTINE DKABLC(MM,JM,F,U,SH,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,(MM+8)*MM/4+1)
      DIMENSION U(JM),SH(MM/2)

      NM=MM/2
      CALL BSSET0(NM,SH)

      DO N=1,NM
        DO J=1,JM
          Y=P(J,1,1)
          SH(N)=SH(N)
     &      +U(J)*(F*Y+U(J))*P(J,2,1)*(1-Y*Y)*P(J,2,N+1)
        END DO
      END DO

      DO N=1,NM
        SH(N)=SH(N)/(2*N*(N+1))
      END DO

      END
************************************************************************
*     線形安定性解析のための行列の計算
*-----------------------------------------------------------------------
*     F: コリオリパラメター
*-----------------------------------------------------------------------
      SUBROUTINE DKALNR(MM,JM,M,F,H,U,B,WORK,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,(MM+8)*MM/4+1)
      DIMENSION WORK(JM,9)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)
      DIMENSION U(JM,2),H(JM,2)
*      DIMENSION B(((MM-M)/2+1)*3,((MM-M)/2+1)*3)
      DIMENSION B(*)

      IF(M.EQ.0) THEN
        CALL DKALN0(MM,JM,F,H,U,B,WORK,WORK(1,7),P,P(1,2,1),P,A)
      ELSE
        NM=MM/2
        IP=NM+2
        IA=4*NM+1
        DO L=1,M-1
          NM=(MM-L)/2
          NM1=NM+1
          IP=IP+NM+2
          IA=IA+3*NM1
        END DO
        CALL DKALNS(MM,JM,M,F,H,U,B,WORK,WORK(1,7),
     &    P,P(1,2,1),P(1,1,IP),A(IA))
      END IF

      END
************************************************************************
*     線形安定性解析のための行列の計算(帯状成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKALN0(MM,JM,F,H,U,B,G,WORK,Y,W,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),W(JM)
      DIMENSION P(JM,2,0:MM/2)
      DIMENSION B(MM/2*3+1,MM/2*3+1)
      DIMENSION G(JM,2,3)
      DIMENSION WORK(JM,3)
      DIMENSION U(JM,2),H(JM,2)
      DIMENSION A(0:MM/2*3+2)      

      NM=MM/2
      M=0

      CALL BSSET0((NM*3+1)**2,B)

      DO L=1,NM*3+1
        CALL BSSET0(JM*6,G)
        IF(L.LE.NM) THEN
          N=L
          DO J=1,JM
            G(J,1,1)=P(J,1,N)
            G(J,2,1)=P(J,2,N)
          END DO
        ELSE IF(L.EQ.NM+1) THEN
          N=0
          DO J=1,JM
            G(J,1,2)=Y(J)
            G(J,2,2)=1
          END DO
        ELSE IF(L.LE.2*NM+1) THEN
          N=L-NM-1
          DO J=1,JM
            G(J,1,2)=P(J,1,N)*Y(J)
            G(J,2,2)=P(J,2,N)*Y(J)*Y(J)+P(J,1,N)
          END DO
        ELSE
          N=L-2*NM-1
          DO J=1,JM
            G(J,1,3)=(1-Y(J)*Y(J))*Y(J)*P(J,2,N)/N
            G(J,2,3)=-(N+1)*P(J,1,N)
          END DO
        END IF
        DO J=1,JM
          WORK(J,1)=H(J,1)*(-2*G(J,2,3))-2*Y(J)*G(J,1,3)*H(J,2)
          WORK(J,2)=-G(J,1,3)*(F+2*U(J,2))
          WORK(J,3)=2*Y(J)*G(J,2,1)-G(J,1,2)*(F+2*U(J,1)/Y(J))
        END DO

        DO J=1,JM
          WORK(J,1)=WORK(J,1)*W(J)
          WORK(J,2)=WORK(J,2)*W(J)*Y(J)
          WORK(J,3)=WORK(J,3)*W(J)*Y(J)*(1-Y(J)*Y(J))
        END DO
      
        DO N=1,NM
          DO J=1,JM
            B(N,L)=B(N,L)+WORK(J,1)*P(J,1,N)
          END DO
        END DO

        N=0
        DO J=1,JM
          B(NM+N+1,L)=B(NM+N+1,L)+WORK(J,2)
        END DO
        DO N=1,NM
          DO J=1,JM
            B(NM+N+1,L)=B(NM+N+1,L)+WORK(J,2)*P(J,1,N)
          END DO
        END DO
        
        DO N=1,NM
          DO J=1,JM
            B(2*NM+N+1,L)=B(2*NM+N+1,L)+WORK(J,3)*P(J,2,N)
          END DO
        END DO

        DO N=1,NM
*          B(2*NM+N+1,L)=B(2*NM+N+1,L)/(N+1)
          B(2*NM+N+1,L)=B(2*NM+N+1,L)/N
        END DO

        DO N=1,NM
          B(NM+N+1,L)=B(NM+N+1,L)+A(NM+N)*B(NM+N,L)
        END DO
        B(NM+NM+1,L)=B(NM+NM+1,L)*A(NM)
        DO N=NM,1,-1
          B(NM+N,L)=A(N-1)*B(NM+N,L)+A(NM+N)*B(NM+N+1,L)
        END DO

        DO N=1,NM-1
          B(2*NM+N+2,L)=B(2*NM+N+2,L)+A(3*NM+N)*B(2*NM+N+1,L)
        END DO
*        B(2*NM+NM+2,L)=B(2*NM+NM+2,L)*A(3*NM)
        B(2*NM+NM+1,L)=B(2*NM+NM+1,L)*A(3*NM)        
        DO N=NM-1,1,-1
          B(2*NM+N+1,L)=A(2*NM+N)*B(2*NM+N+1,L)+A(3*NM+N)*B(2*NM+N+2,L)
        END DO

      END DO

      END
************************************************************************
*     線形安定性解析のための行列の計算(波数 M 成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKALNS(MM,JM,M,F,H,U,B,G,WORK,Y,W,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),W(JM)
      DIMENSION P(JM,2,-1:(MM-M)/2)
      DIMENSION B(((MM-M)/2+1)*3,((MM-M)/2+1)*3)
      DIMENSION G(JM,2,3)
      DIMENSION WORK(JM,3)
      DIMENSION U(JM,2),H(JM,2)
      DIMENSION A(0:(MM-M)/2*3+2)      

      NM=(MM-M)/2
      NM1=NM+1

      CALL BSSET0(((MM-M)/2+1)**2*9,B)

      DO L=1,NM1*3
        CALL BSSET0(JM*6,G)
        IF(L.LE.NM1) THEN
          N=L-1
          DO J=1,JM
            G(J,1,1)=P(J,1,N)
            G(J,2,1)=P(J,2,N)
          END DO
        ELSE IF(L.LE.NM1*2) THEN
          N=L-NM1-1
          DO J=1,JM
            G(J,1,2)=P(J,1,N)
            G(J,2,2)=P(J,2,N)
          END DO
        ELSE
          N=L-2*NM1-2
          DO J=1,JM
            G(J,1,3)=P(J,1,N)
            G(J,2,3)=P(J,2,N)
          END DO
        END IF
        DO J=1,JM
          G(J,2,2)=G(J,2,2)*Y(J)*Y(J)+G(J,1,2)+G(J,2,3)
          G(J,1,3)=G(J,1,3)/Y(J)
          G(J,1,2)=G(J,1,2)*Y(J)+G(J,1,3)          
        END DO
        DO J=1,JM
          WORK(J,1)=H(J,1)*(M*G(J,1,2)/Y(J)-2*G(J,2,3))
     &      +U(J,1)/Y(J)*M*G(J,1,1)-2*Y(J)*G(J,1,3)*H(J,2)
          WORK(J,2)=M/Y(J)*G(J,1,1)-G(J,1,3)*(F+2*U(J,2))
     &      +U(J,1)/Y(J)*M*G(J,1,2)
          WORK(J,3)=2*Y(J)*G(J,2,1)-G(J,1,2)*(F+2*U(J,1)/Y(J))
     &      +U(J,1)/Y(J)*M*G(J,1,3)
        END DO

        DO J=1,JM
          B(NM+2+NM+1,L)=B(NM+2+NM+1,L)
     &      +(WORK(J,2)+WORK(J,3))*P(J,1,-1)*W(J)/Y(J)
          B(NM+2+NM,L)=B(NM+2+NM,L)+WORK(J,2)*P(J,1,NM)*W(J)*Y(J)
        END DO

        DO J=1,JM
          WORK(J,1)=WORK(J,1)*W(J)
          WORK(J,3)=WORK(J,3)*W(J)*Y(J)
          WORK(J,2)=WORK(J,2)*W(J)*Y(J)-WORK(J,3)
        END DO
      
        DO N=0,NM
          DO J=1,JM
            B(N+1,L)=B(N+1,L)+WORK(J,1)*P(J,1,N)
          END DO
        END DO
        
        DO N=0,NM-1
          DO J=1,JM
            B(NM+2+NM+2+N,L)=B(NM+2+NM+2+N,L)+WORK(J,3)*P(J,1,N)
          END DO
        END DO

        DO N=0,NM-1
          B(NM+2+NM+1,L)=B(NM+2+NM+1,L)+A(2*NM+3+N)*B(NM+2+NM+2+N,L)
        END DO

        DO N=0,NM-1
          DO J=1,JM
            B(NM+2+N,L)=B(NM+2+N,L)+WORK(J,2)*P(J,1,N)
          END DO
        END DO

        DO N=1,NM+1
          B(NM+2+N,L)=B(NM+2+N,L)+A(NM+1+N)*B(NM+2+N-1,L)
        END DO
        B(NM+2+NM+1,L)=B(NM+2+NM+1,L)*A(NM+1)
        DO N=NM+1,1,-1
          B(NM+2+N-1,L)=A(N-1)*B(NM+2+N-1,L)+A(NM+1+N)*B(NM+2+N,L)
        END DO

      END DO

      END
************************************************************************
*     時間微分項の計算
*-----------------------------------------------------------------------
*     F: コリオリパラメター,
*     HB: 水底地形(HB(J,I,1): S微分, HB(J,I,2): θ微分)
*-----------------------------------------------------------------------
      SUBROUTINE DKATDV(MM,JM,IM,F,HB,S,DS,G,IT,T,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2*((MM+8)*MM/4+1))
      DIMENSION T(IM*2),IT(5)
      DIMENSION HB(JM,0:IM-1,2)
      DIMENSION S((MM+1)*(MM+2)/2*3-1)
      DIMENSION DS((MM+1)*(MM+2)/2*3-1)            
      DIMENSION G(JM,0:IM-1,10)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)

      CALL DKAS2V(MM,JM,IM,S,G,G(1,0,10),IT,T,P)

      DO I=0,IM-1
        DO J=1,JM
          G(J,I,1)=-G(J,I,4)*G(J,I,2)/P(J,1)
     &      -G(J,I,1)*(G(J,I,5)/P(J,1)+2*G(J,I,9))
     &      -2*P(J,1)*G(J,I,7)*G(J,I,3)
          G(J,I,2)=-G(J,I,4)*G(J,I,5)/P(J,1)
     &      -(G(J,I,2)+HB(J,I,1))/P(J,1)-G(J,I,7)*(F+2*G(J,I,6))
          G(J,I,3)=-G(J,I,4)*G(J,I,8)/P(J,1)
     &      +G(J,I,7)*G(J,I,7)/P(J,1)+G(J,I,4)*(F+G(J,I,4)/P(J,1))
     &      -2*G(J,I,7)*G(J,I,9)-2*(G(J,I,3)+HB(J,I,2))*P(J,1)
        END DO
      END DO

      CALL DKAG2S(MM,JM,IM,G,DS,G(1,0,10),IT,T,P,A)

      END
************************************************************************
*     保存量(AMOM: 全角運動量, AENE: 全エネルギー)の計算
*-----------------------------------------------------------------------      
*     F: コリオリパラメター, HB: 水底地形
*-----------------------------------------------------------------------
      SUBROUTINE DKACNS(MM,JM,IM,F,HB,S,AMOM,AENE,G,IT,T,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2*((MM+8)*MM/4+1))
      DIMENSION T(IM*2),IT(5)
      DIMENSION HB(JM,0:IM-1)
      DIMENSION S((MM+1)*(MM+2)/2*3-1)
      DIMENSION G(JM,0:IM-1,4)

      CALL DKAS2G(MM,JM,IM,S,G,G(1,0,4),IT,T,P)

      AENE=0
      AMOM=0
      DO I=0,IM-1
        DO J=1,JM
          AMOM=AMOM+P(J,2)*G(J,I,1)*(G(J,I,2)+F/2*P(J,1))*P(J,1)
          AENE=AENE+P(J,2)*G(J,I,1)
     &      *(G(J,I,1)+2*HB(J,I)+G(J,I,2)**2+G(J,I,3)**2)
        END DO
      END DO
      AMOM=AMOM/IM
      AENE=AENE/(2*IM)

      END
************************************************************************
*     スペクトル→グリッド の変換(勾配も同時に計算)
*-----------------------------------------------------------------------
      SUBROUTINE DKAS2V(MM,JM,IM,S,G,WORK,IT,T,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,(MM+8)*MM/4+1)
      DIMENSION T(IM*2),IT(5)
      DIMENSION S((MM+1)*(MM+2)/2*3-1)      
      DIMENSION G(JM,0:IM-1,3,3),WORK(JM*IM)

      MMH=(MM+1)*(MM+2)/2
      
      CALL BSSET0(JM*IM*9,G)

      M=0
      NM=MM/2
      CALL DKAS0V(MM,JM,S,S(NM+2),S(2*NM+3),
     &  G,G(1,0,1,2),G(1,0,1,3),
     &  G(1,0,3,1),G(1,0,3,2),G(1,0,3,3),P,P)
      IS=3*NM+3
      IP=NM+2
      DO M=1,MM
        NM=(MM-M)/2
        NM1=NM+1
        CALL DKASMV(MM,JM,M,S(IS),S(IS+NM1),S(IS+3*NM1),S(IS+4*NM1),
     &    G(1,2*M,1,1),G(1,2*M,1,2),G(1,2*M,1,3),
     &    G(1,2*M,3,1),G(1,2*M,3,2),G(1,2*M,3,3),
     &    P,P(1,1,IP))
        IS=IS+6*NM1
        IP=IP+NM+2
      END DO

      DO M=1,MM
        DO J=1,JM
          G(J,2*M+1,2,1)= M*G(J,2*M  ,1,1)
          G(J,2*M  ,2,1)=-M*G(J,2*M+1,1,1)          
          G(J,2*M+1,2,2)= M*G(J,2*M  ,1,2)
          G(J,2*M  ,2,2)=-M*G(J,2*M+1,1,2)          
          G(J,2*M+1,2,3)= M*G(J,2*M  ,1,3)
          G(J,2*M  ,2,3)=-M*G(J,2*M+1,1,3)
        END DO
      END DO

      DO IV=1,3
        DO ID=1,3
          CALL FTTRUB(JM,IM,G(1,0,ID,IV),WORK,IT,T)
        END DO
      END DO

      END
************************************************************************
*     スペクトル→グリッド の変換(勾配も同時に計算, 帯状成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKAS0V(MM,JM,SH,SU,SV,GH,GU,GV,GHD,GUD,GVD,Y,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),P(JM,2,0:MM/2)
      DIMENSION SH(0:MM/2),SU(0:MM/2),SV(MM/2)
      DIMENSION GH(JM),GU(JM),GV(JM)
      DIMENSION GHD(JM),GUD(JM),GVD(JM)      

      NM=MM/2

      DO J=1,JM
        GH(J)=SH(0)
        GU(J)=SU(0)
      END DO
      
      DO N=1,NM
        DO J=1,JM
          GH(J)=GH(J)+SH(N)*P(J,1,N)
          GU(J)=GU(J)+SU(N)*P(J,1,N)          
        END DO
      END DO

      DO N=1,NM
        DO J=1,JM
          GV(J)=GV(J)+(SV(N)/N)*P(J,2,N)
        END DO
      END DO

      DO J=1,JM
        GV(J)=GV(J)*(1-Y(J)*Y(J))*Y(J)
      END DO

      DO N=1,NM
        DO J=1,JM
          GHD(J)=GHD(J)+SH(N)*P(J,2,N)
          GUD(J)=GUD(J)+SU(N)*P(J,2,N)          
        END DO
      END DO

      DO N=1,NM
        DO J=1,JM
          GVD(J)=GVD(J)-((N+1)*SV(N))*P(J,1,N)
        END DO
      END DO

      DO J=1,JM
        GUD(J)=GUD(J)*Y(J)*Y(J)+GU(J)
        GU(J)=GU(J)*Y(J)        
      END DO

      END
************************************************************************
*     スペクトル→グリッド の変換(勾配も同時に計算, 波数 M 成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKASMV(MM,JM,M,SH1,SW1,SH2,SW2,GH,GU,GV,GHD,GUD,GVD,
     &  Y,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),P(JM,2,-1:(MM-M)/2)
      DIMENSION SH1(0:(MM-M)/2),SW1(0:(MM-M)/2*2+1)
      DIMENSION SH2(0:(MM-M)/2),SW2(0:(MM-M)/2*2+1)      
      DIMENSION GH(JM,2),GU(JM,2),GV(JM,2)
      DIMENSION GHD(JM,2),GUD(JM,2),GVD(JM,2)      

      NM=(MM-M)/2
      
      DO N=0,NM
        DO J=1,JM
          GH(J,1)=GH(J,1)+SH1(N)*P(J,1,N)
          GH(J,2)=GH(J,2)+SH2(N)*P(J,1,N)
          GU(J,1)=GU(J,1)+SW1(N)*P(J,1,N)
          GU(J,2)=GU(J,2)+SW2(N)*P(J,1,N)            
        END DO
      END DO

      DO N=-1,NM-1
        DO J=1,JM
          GV(J,2)=GV(J,2)-SW1(NM+2+N)*P(J,1,N)          
          GV(J,1)=GV(J,1)+SW2(NM+2+N)*P(J,1,N)
        END DO
      END DO

      DO N=0,NM
        DO J=1,JM
          GHD(J,1)=GHD(J,1)+SH1(N)*P(J,2,N)
          GHD(J,2)=GHD(J,2)+SH2(N)*P(J,2,N)
          GUD(J,1)=GUD(J,1)+SW1(N)*P(J,2,N)
          GUD(J,2)=GUD(J,2)+SW2(N)*P(J,2,N)            
        END DO
      END DO

      DO N=-1,NM-1
        DO J=1,JM
          GVD(J,2)=GVD(J,2)-SW1(NM+2+N)*P(J,2,N)
          GVD(J,1)=GVD(J,1)+SW2(NM+2+N)*P(J,2,N)
        END DO
      END DO

      DO J=1,JM
        GUD(J,1)=GUD(J,1)*Y(J)*Y(J)+GU(J,1)-GVD(J,2)
        GUD(J,2)=GUD(J,2)*Y(J)*Y(J)+GU(J,2)+GVD(J,1)
      END DO

      DO J=1,JM
        GV(J,1)=GV(J,1)/Y(J)
        GV(J,2)=GV(J,2)/Y(J)
        GU(J,1)=GU(J,1)*Y(J)-GV(J,2)
        GU(J,2)=GU(J,2)*Y(J)+GV(J,1)
      END DO

      END
************************************************************************
*     スペクトル→グリッド の変換
*-----------------------------------------------------------------------      
      SUBROUTINE DKAS2G(MM,JM,IM,S,G,WORK,IT,T,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,(MM+8)*MM/4+1)
      DIMENSION T(IM*2),IT(5)
      DIMENSION S((MM+1)*(MM+2)/2*3-1)      
      DIMENSION G(JM,0:IM-1,3),WORK(JM*IM)

      CALL BSSET0(JM*IM*3,G)

      M=0
      NM=MM/2
      CALL DKAS0G(MM,JM,S,S(NM+2),S(2*NM+3),G,G(1,0,2),G(1,0,3),P,P)
      IS=3*NM+3
      IP=NM+2
      DO M=1,MM
        NM=(MM-M)/2
        NM1=NM+1
        CALL DKASMG(MM,JM,M,S(IS),S(IS+NM1),S(IS+3*NM1),S(IS+4*NM1),
     &    G(1,2*M,1),G(1,2*M,2),G(1,2*M,3),P,P(1,1,IP))
        IS=IS+6*NM1
        IP=IP+NM+2
      END DO

      CALL FTTRUB(JM,IM,G,WORK,IT,T)
      CALL FTTRUB(JM,IM,G(1,0,2),WORK,IT,T)
      CALL FTTRUB(JM,IM,G(1,0,3),WORK,IT,T)

      END
************************************************************************
*     スペクトル→グリッド の変換(帯状成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKAS0G(MM,JM,SH,SU,SV,GH,GU,GV,Y,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),P(JM,2,0:MM/2)
      DIMENSION SH(0:MM/2),SU(0:MM/2),SV(MM/2)
      DIMENSION GH(JM),GU(JM),GV(JM)

      NM=MM/2

      DO J=1,JM
        GH(J)=SH(0)
        GU(J)=SU(0)
      END DO
      
      DO N=1,NM
        DO J=1,JM
          GH(J)=GH(J)+SH(N)*P(J,1,N)
          GU(J)=GU(J)+SU(N)*P(J,1,N)          
        END DO
      END DO

      DO N=1,NM
        DO J=1,JM
          GV(J)=GV(J)+(SV(N)/N)*P(J,2,N)
        END DO
      END DO

      DO J=1,JM
        GU(J)=GU(J)*Y(J)
        GV(J)=GV(J)*(1-Y(J)*Y(J))*Y(J)
      END DO

      END
************************************************************************
*     スペクトル→グリッド の変換(波数 M 成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKASMG(MM,JM,M,SH1,SW1,SH2,SW2,GH,GU,GV,Y,P)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),P(JM,2,-1:(MM-M)/2)
      DIMENSION SH1(0:(MM-M)/2),SW1(0:(MM-M)/2*2+1)
      DIMENSION SH2(0:(MM-M)/2),SW2(0:(MM-M)/2*2+1)      
      DIMENSION GH(JM,2),GU(JM,2),GV(JM,2)

      NM=(MM-M)/2
      
      DO N=0,NM
        DO J=1,JM
          GH(J,1)=GH(J,1)+SH1(N)*P(J,1,N)
          GH(J,2)=GH(J,2)+SH2(N)*P(J,1,N)
          GU(J,1)=GU(J,1)+SW1(N)*P(J,1,N)
          GU(J,2)=GU(J,2)+SW2(N)*P(J,1,N)            
        END DO
      END DO

      DO N=-1,NM-1
        DO J=1,JM
          GV(J,2)=GV(J,2)-SW1(NM+2+N)*P(J,1,N)
          GV(J,1)=GV(J,1)+SW2(NM+2+N)*P(J,1,N)
        END DO
      END DO

      DO J=1,JM
        GV(J,1)=GV(J,1)/Y(J)
        GV(J,2)=GV(J,2)/Y(J)
        GU(J,1)=GU(J,1)*Y(J)-GV(J,2)
        GU(J,2)=GU(J,2)*Y(J)+GV(J,1)
      END DO

      END
************************************************************************
*     グリッド→スペクトルの変換
*-----------------------------------------------------------------------      
      SUBROUTINE DKAG2S(MM,JM,IM,G,S,WORK,IT,T,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,(MM+8)*MM/4+1)
      DIMENSION T(IM*2),IT(5)
      DIMENSION S((MM+1)*(MM+2)/2*3-1)     
      DIMENSION G(JM,0:IM-1,3),WORK(JM*IM)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)

      CALL BSSET0((MM+1)*(MM+2)/2*3-1,S)      

      MMH=(MM+1)*(MM+2)/2

      CALL FTTRUF(JM,IM,G,WORK,IT,T)
      CALL FTTRUF(JM,IM,G(1,0,2),WORK,IT,T)
      CALL FTTRUF(JM,IM,G(1,0,3),WORK,IT,T)

      M=0
      NM=MM/2
      CALL DKAG0S(MM,JM,G,G(1,0,2),G(1,0,3),S,S(NM+2),S(2*NM+3),
     &  P(1,1,1),P(1,2,1),P,A)
      IS=3*NM+3
      IP=NM+2
      IA=4*NM+1
      DO M=1,MM
        NM=(MM-M)/2
        NM1=NM+1
        CALL DKAGMS(MM,JM,M,G(1,2*M,1),G(1,2*M,2),G(1,2*M,3),
     &    S(IS),S(IS+NM1),S(IS+3*NM1),S(IS+4*NM1),
     &    P(1,1,1),P(1,2,1),P(1,1,IP),A(IA))
        IS=IS+6*NM1
        IP=IP+NM+2
        IA=IA+3*NM1
      END DO

      END
************************************************************************
*     グリッド→スペクトル の変換(帯状成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKAG0S(MM,JM,GH,GU,GV,SH,SU,SV,Y,W,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),W(JM),P(JM,2,0:MM/2)
      DIMENSION GH(JM),GU(JM),GV(JM)      
      DIMENSION SH(0:MM/2),SU(0:MM/2),SV(MM/2)
      DIMENSION A(0:MM/2*4-1)

      NM=MM/2

      DO J=1,JM
        GH(J)=GH(J)*W(J)
        GV(J)=GV(J)*W(J)*Y(J)*(1-Y(J)*Y(J))
        GU(J)=GU(J)*W(J)*Y(J)
      END DO

      N=0
      DO J=1,JM
        SH(N)=SH(N)+GH(J)
        SU(N)=SU(N)+GU(J)
      END DO
      
      DO N=1,NM
        DO J=1,JM
          SH(N)=SH(N)+GH(J)*P(J,1,N)
          SU(N)=SU(N)+GU(J)*P(J,1,N)          
        END DO
      END DO

      DO N=1,NM
        DO J=1,JM
          SV(N)=SV(N)+GV(J)*P(J,2,N)
        END DO
      END DO
      
      DO N=1,NM
        SV(N)=SV(N)/N
      END DO

      DO N=1,NM
        SU(N)=SU(N)+A(NM+N)*SU(N-1)
      END DO
      SU(NM)=SU(NM)*A(NM)
      DO N=NM,1,-1
        SU(N-1)=A(N-1)*SU(N-1)+A(NM+N)*SU(N)
      END DO

      DO N=2,NM
        SV(N)=SV(N)+A(3*NM+N-1)*SV(N-1)        
      END DO
      SV(NM)=SV(NM)*A(3*NM)
      DO N=NM,2,-1
        SV(N-1)=A(2*NM+N-1)*SV(N-1)+A(3*NM+N-1)*SV(N)
      END DO

      END
************************************************************************
*     グリッド→スペクトル の変換(波数 M 成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKAGMS(MM,JM,M,GH,GU,GV,SH1,SW1,SH2,SW2,Y,W,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM),W(JM),P(JM,2,-1:(MM-M)/2)
      DIMENSION SH1(0:(MM-M)/2),SW1(0:(MM-M)/2*2+1)
      DIMENSION SH2(0:(MM-M)/2),SW2(0:(MM-M)/2*2+1)      
      DIMENSION GH(JM,2),GU(JM,2),GV(JM,2)
      DIMENSION A(0:(MM-M)/2*3+2)

      NM=(MM-M)/2

      DO J=1,JM
        SW1(NM+1)=SW1(NM+1)+(GU(J,1)-GV(J,2))*P(J,1,-1)*W(J)/Y(J)
        SW2(NM+1)=SW2(NM+1)+(GU(J,2)+GV(J,1))*P(J,1,-1)*W(J)/Y(J)
        SW1(NM)=SW1(NM)+GU(J,1)*P(J,1,NM)*W(J)*Y(J)
        SW2(NM)=SW2(NM)+GU(J,2)*P(J,1,NM)*W(J)*Y(J)
      END DO

      DO J=1,JM
        GH(J,1)=GH(J,1)*W(J)
        GH(J,2)=GH(J,2)*W(J)
        GV(J,1)=GV(J,1)*W(J)*Y(J)
        GV(J,2)=GV(J,2)*W(J)*Y(J)
        GU(J,1)=GU(J,1)*W(J)*Y(J)+GV(J,2)
        GU(J,2)=GU(J,2)*W(J)*Y(J)-GV(J,1)
      END DO
      
      DO N=0,NM
        DO J=1,JM
          SH1(N)=SH1(N)+GH(J,1)*P(J,1,N)
          SH2(N)=SH2(N)+GH(J,2)*P(J,1,N)
        END DO
      END DO

      DO N=0,NM-1
        DO J=1,JM
          SW1(NM+2+N)=SW1(NM+2+N)-GV(J,2)*P(J,1,N)
          SW2(NM+2+N)=SW2(NM+2+N)+GV(J,1)*P(J,1,N)          
        END DO
      END DO

      DO N=0,NM-1
        SW1(NM+1)=SW1(NM+1)+A(2*NM+3+N)*SW1(NM+2+N)
        SW2(NM+1)=SW2(NM+1)+A(2*NM+3+N)*SW2(NM+2+N)
      END DO

      DO N=0,NM-1
        DO J=1,JM
          SW1(N)=SW1(N)+GU(J,1)*P(J,1,N)
          SW2(N)=SW2(N)+GU(J,2)*P(J,1,N)          
        END DO
      END DO

      DO N=1,NM+1
        SW1(N)=SW1(N)+A(NM+1+N)*SW1(N-1)
        SW2(N)=SW2(N)+A(NM+1+N)*SW2(N-1)        
      END DO
      SW1(NM+1)=SW1(NM+1)*A(NM+1)
      SW2(NM+1)=SW2(NM+1)*A(NM+1)      
      DO N=NM+1,1,-1
        SW1(N-1)=A(N-1)*SW1(N-1)+A(NM+1+N)*SW1(N)
        SW2(N-1)=A(N-1)*SW2(N-1)+A(NM+1+N)*SW2(N)
      END DO

      END
************************************************************************
*     Q分布を与えて, 対応する(バランスした) u, h 場を求める
*     (境界での h の値を固定する)
*-----------------------------------------------------------------------
      SUBROUTINE DKAQ2U(MM,JM,F,HBNDRY,Q,U,SU,SH,C,D,E,DD,ERR,P)
      
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NTR=20,EPS=1D-16)
      DIMENSION P(JM,2,0:(MM+8)*MM/4)
      DIMENSION C(0:MM/2,0:MM/2)      
      DIMENSION D(0:MM/2,0:MM/2)
      DIMENSION E(MM/2,0:MM/2)
      DIMENSION DD(-1:MM/2,-1:MM/2)
      DIMENSION ERR(-1:MM/2)            
      DIMENSION U(JM),Q(JM)
      DIMENSION SH(0:MM/2),SU(0:MM/2)

      NM=MM/2
      NM1=NM+1

      CALL BSSET0(NM1*NM1,D)

      DO L=1,NM
        DO N=1,NM
          DO J=1,JM
            D(N,L)=D(N,L)+P(J,2,0)*P(J,2,L)*P(J,1,N)*P(J,1,0)**2
          END DO
        END DO
      END DO

      DO L=1,NM
        DO J=1,JM
          D(0,L)=D(0,L)+P(J,2,0)*P(J,2,L)*P(J,1,0)**2
        END DO
      END DO
      
      DO N=0,NM
        D(N,N)=D(N,N)+1
      END DO

      DO L=0,NM
        DO N=0,NM
          D(N,L)=2*D(N,L)
        END DO
      END DO

      CALL BSSET0(NM1*NM1,C)

      DO L=1,NM
        DO N=1,NM
          DO J=1,JM
            C(N,L)=C(N,L)+Q(J)*P(J,2,0)*P(J,1,L)*P(J,1,N)
          END DO
        END DO
      END DO

      L=0
      DO N=1,NM
        DO J=1,JM
          C(N,L)=C(N,L)+Q(J)*P(J,2,0)*P(J,1,N)
        END DO
      END DO

      N=0
      DO L=1,NM
        DO J=1,JM
          C(N,L)=C(N,L)+Q(J)*P(J,2,0)*P(J,1,L)
        END DO
      END DO

      DO J=1,JM
        C(0,0)=C(0,0)+Q(J)*P(J,2,0)
      END DO
      
      CALL BSCOPY(NM1,C(0,0),SU)
      DO N=0,NM
        SU(N)=SU(N)*HBNDRY
      END DO
      SU(0)=SU(0)-F

      DO L=0,NM
        DO N=0,NM
          DD(N,L)=D(N,L)
        END DO
      END DO

      DO N=0,NM-1
        DO I=N+1,NM
          SU(I)=SU(I)-SU(N)*DD(I,N)/DD(N,N)
          DO K=N+1,NM
            DD(I,K)=DD(I,K)-DD(N,K)*DD(I,N)/DD(N,N)
          END DO
        END DO
      END DO

      DO N=NM,0,-1
        DO I=N+1,NM
          SU(N)=SU(N)-SU(I)*DD(N,I)
        END DO
        SU(N)=SU(N)/DD(N,N)
      END DO

      SH(0)=HBNDRY

      ITR=0

      IC=1
   10 CONTINUE

      ITR=ITR+1

        DO J=1,JM
          U(J)=SU(0)
        END DO
        DO N=1,NM
          DO J=1,JM
            U(J)=U(J)+SU(N)*P(J,1,N)
          END DO
        END DO
        DO J=1,JM
          U(J)=U(J)*P(J,1,0)
        END DO
        
        CALL DKABLC(MM,JM,F,U,SH(1),P)

        CALL BSSET0(NM1+1,ERR)
        ERR(0)=-F
        DO N=0,NM
          DO L=0,NM
            ERR(N)=ERR(N)+C(N,L)*SH(L)-D(N,L)*SU(L)
          END DO
        END DO
        ERR(-1)=-HBNDRY
        DO N=0,NM
          ERR(-1)=ERR(-1)+SQRT(2*N+1D0)*SH(N)
        END DO

        ERRSUM=0
        DO N=-1,NM
          ERRSUM=ERRSUM+ERR(N)**2
        END DO
        ERRSUM=ERRSUM/(NM+2)
        ERRSUM=SQRT(ERRSUM)

        CALL BSSET0(NM*NM1,E)

        DO L=1,NM
          DO N=1,NM
            DO J=1,JM
              Y=P(J,1,0)
              E(N,L)=E(N,L)+P(J,2,0)*(2*U(J)+F*Y)
     &            *Y*(1-Y*Y)*P(J,1,L)*P(J,2,N)
            END DO
          END DO
        END DO

        L=0
        DO N=1,NM
          DO J=1,JM
            Y=P(J,1,0)
            E(N,L)=E(N,L)+P(J,2,0)*(2*U(J)+F*Y)*Y*(1-Y*Y)*P(J,2,N)
          END DO
        END DO

        DO N=1,NM
          DO L=0,NM
            E(N,L)=E(N,L)/(2*N*(N+1))
          END DO
        END DO

        CALL BSSET0((NM1+1)*(NM1+1),DD)

        DO N=0,NM
          DO L=0,NM
            DD(N,L)=D(N,L)
          END DO
        END DO

        DO N=0,NM
          DO L=0,NM
            DO K=1,NM
              DD(N,L)=DD(N,L)-C(K,N)*E(K,L)
            END DO
          END DO
        END DO

        DD(-1,-1)=-1
        DO L=0,NM
          DO K=1,NM
            DD(-1,L)=DD(-1,L)-SQRT(2*K+1D0)*E(K,L)
          END DO
        END DO

        DO N=0,NM
          DD(N,-1)=-C(N,0)
        END DO
        
        DO N=-1,NM-1
          DO I=N+1,NM
            ERR(I)=ERR(I)-ERR(N)*DD(I,N)/DD(N,N)
            DO K=N+1,NM
              DD(I,K)=DD(I,K)-DD(N,K)*DD(I,N)/DD(N,N)
            END DO
          END DO
        END DO

        DO N=NM,-1,-1
          DO I=N+1,NM 
           ERR(N)=ERR(N)-ERR(I)*DD(N,I)
          END DO
          ERR(N)=ERR(N)/DD(N,N)
        END DO

        DO N=0,NM
          SU(N)=SU(N)+ERR(N)
        END DO
        SH(0)=SH(0)+ERR(-1)

      IF(IC.NE.0) THEN
        IF(ERRSUM.LE.EPS) THEN
          IC=0
          GOTO 10        
        ELSE IF(ITR.LT.NTR) THEN
          GOTO 10
        ELSE IF(ITR.EQ.NTR) THEN
          CALL BSDMSG('E','DKAQ2U','CANNOT CONVERGE')
        END IF
      END IF

      END
************************************************************************
*     重力波成分を分離して解くための行列の準備(上位ルーチン)
*-----------------------------------------------------------------------
      SUBROUTINE DKAEGA(MM,JM,F,HBAR,WORK,P,A,WL,WV,WRM,VRM,VLM)      

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2,0:(MM+8)*MM/4)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)
      DIMENSION WORK(JM,13)      
      DIMENSION WL((MM/2+1)*3,6)
      DIMENSION WV((MM/2+1)*(MM/2+1)*9,3)
      DIMENSION WRM(MM*(MM+4)/2)
      DIMENSION VRM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      DIMENSION VLM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      
      DO J=1,JM
        WORK(J,1)=HBAR
        WORK(J,2)=0        
        WORK(J,3)=0
        WORK(J,4)=0
      END DO

      M=0
      N=MM/2*3+1
      ND=MM/2*2
      IP=1
      IPV=1      
      CALL DKAEGM(MM,JM,M,N,ND,WORK,WORK(1,3),F,WV(1,3),WORK(1,5),
     &  P,A,WL,WL(1,2),WV,WV(1,2),WRM,VRM,VLM,WL(1,3))
      DO M=1,MM
        IP=IP+ND
        IPV=IPV+N*ND
        N=((MM-M)/2+1)*3
        ND=((MM-M)/2+1)*2
        CALL DKAEGM(MM,JM,M,N,ND,WORK,WORK(1,3),F,WV(1,3),WORK(1,5),
     &    P,A,WL,WL(1,2),WV,WV(1,2),WRM(IP),VRM(IPV),VLM(IPV),WL(1,3))
      END DO

      END
************************************************************************
*     重力波成分を分離して解くための行列の準備(下位ルーチン)
*-----------------------------------------------------------------------
      SUBROUTINE DKAEGM(MM,JM,M,N,ND,H,U,F,B,WORK,P,A,WR,WI,VR,VL,
     &  WRM,VRM,VLM,WORKL)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(EPS=1D-10)
      DIMENSION P(JM,2,0:(MM+8)*MM/4)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)
      DIMENSION H(JM,2),U(JM,2)
      DIMENSION B((MM/2+1)**2*9)
      DIMENSION WORK(JM*9)      
      DIMENSION WR(N),WI(N)
      DIMENSION VR(N,N),VL(N,N)
      DIMENSION VRM(N,ND),VLM(N,ND),WRM(ND)
      DIMENSION WORKL(4*(MM/2+1)*3)

      CALL DKALNR(MM,JM,M,F,H,U,B,WORK,P,A)
      LWORK=4*(MM/2+1)*3
      CALL DGEEV('V','V',N,B,N,WR,WI,VL,N,VR,N,WORKL,LWORK,INFO)

      JD=0
      DO J=1,N
        IF(ABS(WR(J)).GT.EPS) THEN
          IF(JD.LE.ND-1) THEN
            JD=JD+1
            S=0
            DO I=1,N
              S=S+VR(I,J)*VL(I,J)
            END DO
            WRM(JD)=WR(J)
            DO I=1,N
              VRM(I,JD)=VR(I,J)
              VLM(I,JD)=VL(I,J)/S
            END DO
          ELSE
            CALL BSDMSG('E','DKEGGM','CHECK')
          END IF
        END IF
      END DO

      END
************************************************************************
*     重力波成分の時間発展(上位ルーチン)
*-----------------------------------------------------------------------
      SUBROUTINE DKATDG(MM,S,DT,WS,WRM,VRM,VLM)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WRM(MM*(MM+4)/2)
      DIMENSION VRM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      DIMENSION VLM(6*(1+(MM+2)/2*((MM+5)/2*MM-3)/3)+MM/2*2)
      DIMENSION S((MM+1)*(MM+2)/2*3-1)
      DIMENSION WS((MM/2+1)*4)

      IS=2
      M=0
      N=MM/2*3+1
      ND=MM/2*2
      IP=1
      IPV=1
      CALL DKATD0(MM,N,ND,DT,S(IS),WS,WRM,VRM,VLM)
      IS=IS+N
      DO M=1,MM
        IP=IP+ND
        IPV=IPV+N*ND
        N=((MM-M)/2+1)*3
        ND=((MM-M)/2+1)*2
        CALL DKATDM(MM,M,N,ND,DT,S(IS),WS,WRM(IP),VRM(IPV),VLM(IPV))
        IS=IS+N*2
      END DO

      END
************************************************************************
*     重力波成分の時間発展(下位ルーチン: 波数M成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKATDM(MM,M,N,ND,DT,S,WS,WRM,VRM,VLM)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(N,2)
      DIMENSION WS(ND,2)
      DIMENSION VRM(N,ND),VLM(N,ND),WRM(ND)

      DO J=1,ND
        WS(J,1)=0
        WS(J,2)=0        
        DO I=1,N
          WS(J,1)=WS(J,1)+VLM(I,J)*S(I,1)
          WS(J,2)=WS(J,2)+VLM(I,J)*S(I,2)
        END DO
      END DO

      DO J=1,ND
        DO I=1,N
          S(I,1)=S(I,1)-WS(J,1)*VRM(I,J)
          S(I,2)=S(I,2)-WS(J,2)*VRM(I,J)
        END DO
      END DO

      DO J=1,ND
        DO I=1,N
          S(I,1)=S(I,1)+WS(J,1)*VRM(I,J)*COS(WRM(J)*DT)
     &                 +WS(J,2)*VRM(I,J)*SIN(WRM(J)*DT)
          S(I,2)=S(I,2)+WS(J,2)*VRM(I,J)*COS(WRM(J)*DT)
     &                 -WS(J,1)*VRM(I,J)*SIN(WRM(J)*DT)
        END DO
      END DO

      END
************************************************************************
*     重力波成分の時間発展(下位ルーチン: 波数0成分)
*-----------------------------------------------------------------------
      SUBROUTINE DKATD0(MM,N,ND,DT,S,WS,WRM,VRM,VLM)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(N)
      DIMENSION WS(ND,2)
      DIMENSION VRM(N,ND),VLM(N,ND),WRM(ND)

      NV=MM/2*2+1

      DO J=1,ND
        WS(J,1)=0
        DO I=1,NV
          WS(J,1)=WS(J,1)+VLM(I,J)*S(I)
        END DO
        WS(J,2)=0        
        DO I=NV+1,N
          WS(J,2)=WS(J,2)+VLM(I,J)*S(I)
        END DO
      END DO

      DO J=1,ND
        DO I=1,NV
          S(I)=S(I)-WS(J,1)*VRM(I,J)
        END DO
        DO I=NV+1,N
          S(I)=S(I)-WS(J,2)*VRM(I,J)
        END DO
      END DO

      DO J=1,ND
        DO I=1,NV
          S(I)=S(I)+WS(J,1)*VRM(I,J)*COS(WRM(J)*DT)
     &             +WS(J,2)*VRM(I,J)*SIN(WRM(J)*DT)
        END DO
        DO I=NV+1,N
          S(I)=S(I)+WS(J,2)*VRM(I,J)*COS(WRM(J)*DT)
     &             -WS(J,1)*VRM(I,J)*SIN(WRM(J)*DT)
        END DO
      END DO

      END
************************************************************************
*     重力波成分を除いた時間微分項の計算
*-----------------------------------------------------------------------
*     F: コリオリパラメター,
*     HB: 水底地形(HB(J,I,1): S微分, HB(J,I,2): θ微分)
*-----------------------------------------------------------------------
      SUBROUTINE DKATDL(MM,JM,IM,F,HBAR,HB,S,DS,G,IT,T,P,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(JM,2*((MM+8)*MM/4+1))
      DIMENSION T(IM*2),IT(5)
      DIMENSION HB(JM,0:IM-1,2)
      DIMENSION S((MM+1)*(MM+2)/2*3-1)
      DIMENSION DS((MM+1)*(MM+2)/2*3-1)            
      DIMENSION G(JM,0:IM-1,10)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)

      CALL DKAS2V(MM,JM,IM,S,G,G(1,0,10),IT,T,P)

      DO I=0,IM-1
        DO J=1,JM
          G(J,I,1)=-G(J,I,4)*G(J,I,2)/P(J,1)
     &      -(G(J,I,1)-HBAR)*(G(J,I,5)/P(J,1)+2*G(J,I,9))
     &      -2*P(J,1)*G(J,I,7)*G(J,I,3)
          G(J,I,2)=-G(J,I,4)*G(J,I,5)/P(J,1)
     &      -HB(J,I,1)/P(J,1)-G(J,I,7)*2*G(J,I,6)
          G(J,I,3)=-G(J,I,4)*G(J,I,8)/P(J,1)
     &      +G(J,I,7)*G(J,I,7)/P(J,1)+G(J,I,4)*G(J,I,4)/P(J,1)
     &      -2*G(J,I,7)*G(J,I,9)-2*HB(J,I,2)*P(J,1)
        END DO
      END DO

      CALL DKAG2S(MM,JM,IM,G,DS,G(1,0,10),IT,T,P,A)

      END
