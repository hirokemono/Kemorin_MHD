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
*     GRAPHIC PROGRAM FOR MODEL                              1999/03/29
***********************************************************************
      PROGRAM GRAPHM
 
      CALL SBOPEN

      DO I=0,10
        CALL SCPMAP(I)
      END DO

      CALL SBCLOS

      END
***********************************************************************
      SUBROUTINE SCPMAP(I)

      WRITE(6,*) I
      CALL SBGAVT(I)
      CALL SBPMAP(I)

      END
***********************************************************************
*     LOCAL SUBROUTINE PACKAGE                
***********************************************************************
      SUBROUTINE SBOPEN

      CHARACTER CF*80,CDUMPF*80
      PARAMETER(MM=21)
      PARAMETER(JM=32,KM=64)
      PARAMETER(LM=(MM+1)*(MM+1))
      PARAMETER(NB=23476)
      PARAMETER(NBR=(((JM*2+3)*(KM*2+3)+15)/16+1)*3)
      PARAMETER(DT=0.1)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION AVT(LM),DIV(LM),PHI(LM),AVTD(LM,3)
      DIMENSION IPOINT(LM),RPOINT(LM)
      DIMENSION G(-KM:KM,-JM:JM),GD(0:KM*2,-JM:JM)
      DIMENSION W(-JM:JM,-MM:MM)
      DIMENSION WORK((JM+1)*(4*JM+5*MM+14)+(MM+1)*(MM+1)+MM+2+6*KM+15)
      DIMENSION IBR(NBR),SY(-JM:JM)
      SAVE

      SQRT05=SQRT(0.5D0)

      DO J=-JM,JM
        SY(J)=SIN(PI/2*J/JM)
      END DO

      IWS=-1
      WRITE(6,*) 'DATA FILE NAME?'
      READ(5,*) CF
*      CF='output.dat'
      CALL FHUOPN(10,CF,'R',NB)
      CALL SHTINT(MM,JM,KM,WORK)
      CALL GPOPEN(IWS,CF,CDUMPF)

      DO L=1,LM
        CALL SNL2NM(L,N,M)
        CALL STNM2L(MM,N,M,IPOINT(L))
        IF(M.EQ.0) THEN
          RPOINT(L)=1
        ELSE
          RPOINT(L)=SQRT05
        END IF
      END DO

      RETURN
*----------------------------------------------------------------------
*     GET AVT
*----------------------------------------------------------------------
      ENTRY SBGAVT(I)

      CALL FHUJMP(10,I*LM*3*4)
      CALL FEGETR(10,LM*3,AVTD)

      DO L=1,LM
        AVT(IPOINT(L))=RPOINT(L)*AVTD(L,1)
        DIV(IPOINT(L))=RPOINT(L)*AVTD(L,2)
        PHI(IPOINT(L))=RPOINT(L)*AVTD(L,3)
      END DO
      
      RETURN
*----------------------------------------------------------------------
*     POLAR MAP
*----------------------------------------------------------------------
      ENTRY SBPMAP(I)

      DAY=DT*I
      CALL SHTS2G(MM,JM,KM,0,AVT,W,G,WORK)

      DO J=-JM,JM
        DO K=0,KM
          GD(K,J)=G(K,J)
          GD(KM+K,J)=G(-KM+K,J)
        END DO
      END DO
      CALL GPCNT3(KM,JM,GD,I,NBR,IBR,SY)
      
      CALL SHTS2G(MM,JM,KM,0,DIV,W,G,WORK)

      DO J=-JM,JM
        DO K=0,KM
          GD(K,J)=G(K,J)
          GD(KM+K,J)=G(-KM+K,J)
        END DO
      END DO
      CALL GPCNT3(KM,JM,GD,I,NBR,IBR,SY)
      
      CALL SHTS2G(MM,JM,KM,0,PHI,W,G,WORK)

      DO J=-JM,JM
        DO K=0,KM
          GD(K,J)=G(K,J)
          GD(KM+K,J)=G(-KM+K,J)
        END DO
      END DO
      CALL GPCNT3(KM,JM,GD,I,NBR,IBR,SY)
      
      RETURN
*----------------------------------------------------------------------
*     CLOSE SUBROUTINE
*----------------------------------------------------------------------
      ENTRY SBCLOS

      CALL GRCLS
      CALL FHUCLS(10)

      END
***********************************************************************
*     GRAPHIC SUBROUTINE PACKAGE                
***********************************************************************
      SUBROUTINE GPOPEN(IWS,CF,CDUMPF)

      CHARACTER CF*(*),CDUMPF*(*)

      CALL SGPSET('LCORNER',.FALSE.)
      CALL GLPSET('LMISS',.TRUE.)
      CALL GLPSET('MSGLEV',2)

      CALL GROPN(IWS)
      CALL SGPSET('LSOFTF',.FALSE.)
      CALL SGPSET('LFULL',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)
      CALL UDPSET('LMSG',.FALSE.)
      CALL SLDIV('Y',3,4)
      CALL SLRAT(1.0,1.0)
      CALL UZPSET('INNER',-1)

      END
***********************************************************************
      SUBROUTINE GPCNT3(KM,JM,G,IDAY,NBR,IBR,SY)

      CHARACTER CTTL*5,CSGI*1
      DIMENSION G(0:2*KM,-JM:JM),SY(-JM:JM)
      DIMENSION IBR(NBR)

      CALL GRFRM
      CALL SGSVPT(0.2,0.9,0.2,0.9)
      CALL SGSWND(0.0,2.0,-1.0,1.0)
      CALL SGSTRN(1)
      CALL SGSTRF

      CALL UWSGYA(SY,2*JM+1)

      WRITE(CTTL,'(I3)') IDAY
      CALL UXAXDV('B',0.5,1.0)
      CALL UXAXDV('T',0.5,1.0)
      CALL UYAXDV('L',0.1,0.5)
      CALL UYAXDV('R',0.1,0.5)
      CALL UXMTTL('T','t='//CTTL,0.0)
      CALL UXMTTL('B',CSGI(162)//'('//CSGI(194)//CSGI(167)//')',0.0)
      CALL UYMTTL('L',CSGI(163),0.0)
*      CALL UDGCLA(-5.0,5.0,0.8)
      CALL UDCNTZ(G,2*KM+1,2*KM+1,2*JM+1,IBR,NBR)
      CALL UETONE(G,2*KM+1,2*KM+1,2*JM+1)

      END
