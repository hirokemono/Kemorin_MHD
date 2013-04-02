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
*    navier-stokes-graph.f:
*          navier-stokes.f �ν��Ϥ�ɽ���Τ���Υץ����      
*                                                  2002/03/16 K.Ishioka
***********************************************************************

* �����
      CALL SBINIT

      NSTEP=10
      DO ISTEP=0,NSTEP
        CALL SBGETD(ISTEP)
        CALL SBSPCT
*        CALL SBGRPH        
      END DO

      CALL SBCLS

      END
***********************************************************************
*     ��ν�����ޤȤ᤿���֥롼����ѥå�����
*----------------------------------------------------------------------
*     �ѥå������ν�����򤹤륵�֥롼����
*----------------------------------------------------------------------
      SUBROUTINE SBINIT

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER CK*3
      PARAMETER(NM=10,MM=10,LM=10) ! �����ȿ�
      PARAMETER(KM=32,JM=32,IM=32) ! �ʻ�����
      PARAMETER(IJKM2=(2*LM+1)*(2*MM+1)*(2*NM+1)*2)      
      PARAMETER(KMAX=17)  ! ���ͥ륮�����ڥ��ȥ���ȿ��ϰ�
      PARAMETER(IU=10,NB=1024)      
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)
      DIMENSION DZ(-NM:NM,-MM:MM,-LM:LM,2)
      DIMENSION DL(-NM:NM,-MM:MM,-LM:LM,2)
      DIMENSION WS(-NM:NM,-MM:MM,-LM:LM) !����ΰ�
      DIMENSION W(KM*JM*IM) !����ΰ�
      DIMENSION U(KM*JM*IM,3)   
      DIMENSION O(0:KM-1,0:JM-1,0:IM-1,3)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)
      DIMENSION ES(KMAX)
      REAL RG(0:IM,0:JM),RES(KMAX),RK(KMAX)
      REAL RX(2),RY(2)
      SAVE

      CALL FHUOPN(IU,'test.dat','R',NB)      

* P3PACK�ν����

      CALL P3INIT(KM,JM,IM,ITK,TK,ITJ,TJ,ITI,TI)

      CALL SWISET('IWIDTH',  400)
      CALL SWISET('IHEIGHT', 400)
*      CALL SWLSET('LWAIT',.FALSE.)
      CALL SWLSET('LALT',.TRUE.)
      CALL SGLSET('LCORNER',.FALSE.)

      CALL GROPN(1)
      CALL UZPSET('INNER',-1)
      CALL SLRAT(1.0,1.0)
      CALL GLPSET('LMISS',.TRUE.)
      CALL SGPSET('LCNTL',.TRUE.)

      RETURN
*----------------------------------------------------------------------
*     �ǡ������ɤ߹���
*----------------------------------------------------------------------      
      ENTRY SBGETD(ISTEP)

      CALL FHUJMP(IU,IJKM2*8*ISTEP)
      CALL FEGETD(IU,IJKM2,Z)
      
      RETURN
*----------------------------------------------------------------------
*     ���٤ζ����������̤����̤Υ���ե�����
*----------------------------------------------------------------------
      ENTRY SBGRPH

      DO ISW=1,3
        CALL P3GETO(NM,MM,LM,Z,WS,ISW)
        CALL P3S2GA(NM,MM,LM,KM,JM,IM,WS,O(0,0,0,ISW),
     &    W,ITK,TK,ITJ,TJ,ITI,TI)
      END DO

      DO K=0,KM-1
        DO I=0,IM-1
          DO J=0,JM-1
            RG(I,J)=SQRT(O(K,J,I,1)**2+O(K,J,I,2)**2+O(K,J,I,3)**2)
          END DO
          RG(I,JM)=RG(I,0)
        END DO
        DO J=0,JM
          RG(IM,J)=RG(0,J)
        END DO

        CALL GRFRM
        CALL SGSVPT(0.15,0.9,0.15,0.9)
        CALL SGSWND(0.0,1.0,0.0,1.0)
        CALL SGSTRN(1)
        CALL SGSTRF

        CALL USDAXS
        WRITE(CK(1:3),'(I3)') K
        CALL UXSTTL('T','K='//CK,1.0)
        CALL UDCNTR(RG,IM+1,IM+1,JM+1)
      END DO

      RETURN
*----------------------------------------------------------------------
*     ���ͥ륮�����ڥ��ȥ��ɽ��
*----------------------------------------------------------------------
      ENTRY SBSPCT

      CALL P3ESPT(NM,MM,LM,KMAX,Z,ES)
      DO K=1,KMAX
        RES(K)=ES(K)
        RK(K)=K
      END DO

      CALL GRFRM
      CALL SGSVPT(0.15,0.9,0.15,0.9)
      CALL SGSWND(RK(1),RK(KMAX),1E-10,1.0)
      CALL SGSTRN(4)
      CALL SGSTRF

      CALL USDAXS
      CALL SGPLU(KMAX,RK,RES)

      RX(1)=RK(1)
      RY(1)=1.0
      RX(2)=RK(KMAX)
      RY(2)=RK(KMAX)**(-5D0/3)

      CALL SGPLZU(2,RX,RY,3,1)

      RETURN
*----------------------------------------------------------------------
*     ��λ����
*----------------------------------------------------------------------
      ENTRY SBCLS

      CALL FHUCLS(IU)
      CALL GRCLS

      END
