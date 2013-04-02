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
***********************************************************************
*     model.f のためのグラフィクスプログラム                 2010/02/28
***********************************************************************
      CALL SBOPEN

      DO I=0,100
        WRITE(6,*) I
        CALL SBGVRT(I)
        CALL SBPMAP(I)
      END DO

      CALL SBCLOS

      END
***********************************************************************
*     初期化
***********************************************************************
      SUBROUTINE SBOPEN

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CF*80
      PARAMETER(MM=85,JM=128,IM=256,NM=MM+2)
      PARAMETER(LM=(MM+1)*(MM+1))
      PARAMETER(NB=1024*8)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION PSI(LM),VRT(LM),US(MM+2),U(JM)

      DIMENSION G(0:IM-1,JM)
      DIMENSION P(JM/2,MM+4),Q(JM/2,11),R((MM+1)*(2*NM-MM-1)+1)
      DIMENSION IT(2,2),T(IM*3,2)            
      DIMENSION WS(2*(MM+3))
      DIMENSION WG((IM+2)*JM)
      DIMENSION W((JM+1)*IM)
      DIMENSION C((MM+1)*(MM+1))      
      DIMENSION D((MM+1)*(MM+1),2)      

      PARAMETER(NX=IM+1,NY=JM+2)
      PARAMETER(NBR=(((NX+2)*(NY+2)+15)/16+1)*3)
      DIMENSION IBR(NBR)
      REAL RG(0:IM,0:JM+1),RY(0:JM+1),RU(0:JM+1),RPHI(0:JM+1)
      REAL TLEV1,TLEV2,DAY
      
      SAVE

      IWS=1
      DT=0.1D0
      CF='data.dat'
      
      CALL FHUOPN(10,CF,'R',NB)
      CALL GPOPEN(IWS)

      CALL SJINIT(MM,NM,JM,IM,P,R,IT,T)
      CALL SJINIC(MM,C)      
      CALL SJINID(MM,D)

      DO J=1,JM/2
        RY(JM/2+J)=180/PI*ASIN(P(J,1))
        RY(JM/2-J+1)=-RY(JM/2+J)
        RPHI(JM/2+J)=P(J,1)
        RPHI(JM/2-J+1)=-P(J,1)        
      END DO
      RPHI(JM+1)=1
      RPHI(0)=-1
      RY(JM+1)=90
      RY(0)=-90

      RETURN
*----------------------------------------------------------------------
*     渦度場データの取得
*----------------------------------------------------------------------
      ENTRY SBGVRT(I)

      CALL FHUJMP(10,I*LM*8)
      CALL FEGETD(10,LM,VRT)

      RETURN
*----------------------------------------------------------------------
*     グラフィクス
*----------------------------------------------------------------------
      ENTRY SBPMAP(I)

      DAY=DT*I

      CALL SJTS2G(MM,NM,MM,IM,JM,VRT,G,IT,T,P,Q,R,WS,WG,W,0)
      CALL LJTSZP(MM,VRT,GNP,GSP)

      CALL SJCLAP(MM,VRT,PSI,D,2)
      CALL LJCSZY(MM,PSI,US,C)
      CALL LJTSZG(NM,MM+1,JM,US,U,P,Q,R,WS,0)

      DO J=1,JM
        DO K=0,IM-1
          RG(K,J)=G(K,J)
        END DO
        RG(IM,J)=G(0,J)        
      END DO
      DO K=0,IM
        RG(K,JM+1)=GNP
        RG(K,0)=GSP
      END DO

      RU(0)=0
      DO J=1,JM
        RU(J)=-U(J)
      END DO
      RU(JM+1)=0
      
      CALL GPGRPH(IM,JM,RG,RU,DAY,NBR,IBR,RY,RPHI)
      
      RETURN
*----------------------------------------------------------------------
*     終了処理
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS
      CALL FHUCLS(10)

      END
***********************************************************************
*     グラフィクスの下請けサブルーチン
***********************************************************************
      SUBROUTINE GPOPEN(IWS)

      CALL SGPSET('LCORNER',.FALSE.)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SWISET('IWIDTH',  500)
      CALL SWISET('IHEIGHT', 350)
      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(IWS)
      CALL SGPSET('LSOFTF',.FALSE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL SLRAT(1.0,0.7)
      CALL UZPSET('INNER',-1)

      IPAT0=25
      DLEV=3
      NLEV=20
      IDLEV=3
      TLEV0=-DLEV*NLEV/2

      DO I=1,NLEV
        IPAT=(IPAT0+I*IDLEV)*1000+999
        IF(I.EQ.1) THEN
          TLEV1=-999.0
        ELSE
          TLEV1=TLEV0+(I-1)*DLEV
        END IF
        IF(I.EQ.NLEV) THEN
          TLEV2=999.0
        ELSE
          TLEV2=TLEV0+I*DLEV
        END IF
        CALL UESTLV(TLEV1,TLEV2,IPAT)
      END DO

      END
***********************************************************************
      SUBROUTINE GPGRPH(IM,JM,RG,RU,DAY,NBR,IBR,RY,RPHI)

      CHARACTER CTTL*4,CSGI*1
      DIMENSION RG(0:IM,0:JM+1),RU(0:JM+1),RY(0:JM+1),RPHI(0:JM+1)
      DIMENSION IBR(NBR)

      WRITE(CTTL,'(F4.1)') DAY

      CALL GRFRM

      CALL SGTXZV(0.0,0.68,'T='//CTTL,0.04,0,-1,2)

      CALL GRSVPT(0.05,0.65,0.05,0.65)
      CALL GRSWND(0.0,360.0,-90.0,90.0)
      CALL GRSSIM(0.3,0.0,0.0)
      CALL GRSMPL(0.0,0.0,0.0)
      CALL GRSTXY( -180.0, 180.0, 0.0, 90.0 )
      CALL GRSTRN(30)
      CALL GRSTRF

      CALL UWSGYA(RY,JM+2)

      CALL UETONF(RG,IM+1,IM+1,JM+2)
*      CALL UDCNTZ(RG,IM+1,IM+1,JM,IBR,NBR)

      CALL UMPSET('DGRIDMJ',90.0)
      CALL UMPSET('DGRIDMN',30.0)
      CALL UMPGLB

      CALL GRSVPT(0.7,0.95,0.35,0.65)
      CALL GRSWND(-1.0,1.0,0.0,1.0)
      CALL GRSTRN(1)
      CALL GRSTRF
      CALL UXAXDV('B',0.1,1.0)

      CALL GRSVPT(0.7,0.95,0.05,0.65)
      CALL GRSWND(-1.0,1.0,-1.0,1.0)
      CALL GRSTRN(1)
      CALL GRSTRF

      CALL SGSLNI(3)
      CALL SGLNU(0.0,-1.0,0.0,1.0)
      CALL SGPLU(JM+2,RU,RPHI)
      
      END
