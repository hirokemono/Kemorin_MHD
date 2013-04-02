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
*     浅水方程式の保存量のチェック                            1999/03/31
************************************************************************
      SUBROUTINE SPSWCK(MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AMOM,AENE,AENS,
     &  RN,IT,T,Y,IP4,P4,R4,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP4(4*((MM+1)/2+MM+1)*2)
      DIMENSION P4(4*((MM+1)/2+MM+1)*JM)
      DIMENSION R4(4*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(4*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(ID,JD,4)

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

*/ 保存量の計算

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
          U1=WS(I,J1,1)
          U2=WS(I,J2,1)
          V1=WS(I,J1,2)
          V2=WS(I,J2,2)
          Q1=WS(I,J1,3)
          Q2=WS(I,J2,3)
          H1=WS(I,J1,4)
          H2=WS(I,J2,4)
          AMOM=AMOM+Y2*( H1*(U1+OMEGA*Y3*Y3)
     &                  +H2*(U2+OMEGA*Y3*Y3) )
          AENE=AENE+0.5D0*Y2*( H1*((U1*U1+V1*V1)*Y4*Y4+H1)
     &                        +H2*((U2*U2+V2*V2)*Y4*Y4+H2) )
          AENS=AENS+0.5D0*Y2*(Q1*Q1/H1+Q2*Q2/H2)
        END DO
      END DO
      AMOM=AMOM/IM
      AENE=AENE/IM
      AENS=AENS/IM

      END
