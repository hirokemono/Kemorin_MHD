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
*     Q分布を与えて対応する U, H の場を求めるプログラム(DKAQ2U)を
*     用いて実際のプロファイルの形をチェックするプログラム      
*                                              2003/04/11 By  K. Ishioka
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
* コリオリパラメター F および 境界でのジオポテンシャル HBNDRY の設定
      PARAMETER(F=1D0,HBNDRY=1D0)
* 切断次数 MM および 分点数 JM の設定
* JM≧(MM+MM/2+2)/2 であること
      PARAMETER(MM=85,JM=65)
* 以下のパラメターや配列には触らないこと
      PARAMETER(MMP=(MM+8)*MM/4+1)
      PARAMETER(NM=MM/2)
      DIMENSION P(JM,2,0:MMP-1)
      DIMENSION A(3*((MM+1)/2)*(MM/2)+7*MM/2)      
      DIMENSION D(0:MM/2,0:MM/2)
      DIMENSION C(0:MM/2,0:MM/2)
      DIMENSION E(MM/2,0:MM/2)
      DIMENSION DD(-1:MM/2,-1:MM/2)
      DIMENSION ERR(-1:MM/2)            
      DIMENSION H(JM),U(JM),Q(JM),Z(JM)
      DIMENSION SH(0:MM/2),SU(0:MM/2)
      REAL RY(JM),RH(JM),RU(JM),RQ(JM)

      CALL DKAIN0(MM,JM,P,A)

*-----------------------------------------------------------------------
*     Q分布の設定
*     Y: r座標変数の値
*-----------------------------------------------------------------------      

      DO J=1,JM
        Y=P(J,1,0)
        X=Y*Y
        Q(J)=F+EXP(-16*X)
      END DO

*-----------------------------------------------------------------------      

      CALL DKAQ2U(MM,JM,F,HBNDRY,Q,U,SU,SH,C,D,E,DD,ERR,P)

      DO J=1,JM
        Y=P(J,1,0)        
        H(J)=SH(0)
        Z(J)=F+2*SU(0)
        U(J)=SU(0)*Y
      END DO
      DO N=1,NM
        DO J=1,JM
          Y=P(J,1,0)
          H(J)=H(J)+SH(N)*P(J,1,N)
          Z(J)=Z(J)+2*SU(N)*(P(J,1,N)+Y*Y*P(J,2,N))
          U(J)=U(J)+SU(N)*P(J,1,N)*Y
        END DO
      END DO

      DO J=1,JM
        RY(J)=P(J,1,0)
        RH(J)=H(J)
        RU(J)=U(J)
        RQ(J)=Z(J)/H(J)
      END DO

*-----------------------------------------------------------------------      
*     グラフィクス: 計算された Q, H, U の場をそれぞれ描く
*     (Qの場はスペクトル展開によって実現されたQの場なので,
*      与えた Q の場とは若干異なる(Gibbs現象のため)
*-----------------------------------------------------------------------

      CALL GROPN(1)
      CALL GRFRM
      CALL USSTTL('Q',' ','r',' ')
      CALL USGRPH(JM,RY,RQ)
      CALL GRFRM
      CALL USSTTL('H',' ','r',' ')      
      CALL USGRPH(JM,RY,RH)
      CALL GRFRM
      CALL USSTTL('U',' ','r',' ')
      CALL USGRPH(JM,RY,RU)
      CALL GRCLS

      END
