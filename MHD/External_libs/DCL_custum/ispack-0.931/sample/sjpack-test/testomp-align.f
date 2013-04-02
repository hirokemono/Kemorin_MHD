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
*     testomp.f に 16バイトアラインメントを強制したもの)      2009/08/21
************************************************************************      
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MM=170,JM=256,IM=512,NT=16)
      PARAMETER(NALIGN=16)
      PARAMETER(NTT=IM*6)
      PARAMETER(NQ=JM/2*11*NT,NG=IM*JM,NWG=(IM+2)*JM,NW=(JM+1)*IM)
      CHARACTER*1 CT(0:NTT*8+NALIGN-2)      
      CHARACTER*1 CQ(0:NQ*8+NALIGN-2)
      CHARACTER*1 CG1(0:NG*8+NALIGN-2)
      CHARACTER*1 CG2(0:NG*8+NALIGN-2)      
      CHARACTER*1 CGO1(0:NG*8+NALIGN-2)
      CHARACTER*1 CGO2(0:NG*8+NALIGN-2)      
      CHARACTER*1 CWG(0:NWG*8+NALIGN-2)
      CHARACTER*1 CW1(0:NW*8+NALIGN-2)
      CHARACTER*1 CW2(0:NW*8+NALIGN-2)      

      CALL APALGN(CT,NALIGN,ITT)
      CALL APALGN(CQ,NALIGN,IQ)
      CALL APALGN(CG1,NALIGN,IG1)
      CALL APALGN(CG2,NALIGN,IG2)      
      CALL APALGN(CGO1,NALIGN,IGO1)
      CALL APALGN(CGO2,NALIGN,IGO2)      
      CALL APALGN(CWG,NALIGN,IWG)
      CALL APALGN(CW1,NALIGN,IW1)
      CALL APALGN(CW2,NALIGN,IW2)      

      CALL SUB(CT(ITT),CQ(IQ),CG1(IG1),CG2(IG2),CGO1(IGO1),CGO2(IGO2),
     &  CWG(IWG),CW1(IW1),CW2(IW2))

      END
