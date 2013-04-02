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
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NALIGN=16)
      PARAMETER(JM=2048,IM=1024,LM=1023,KM=511,NTR=1)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION TJ2(JM*2),TI2(IM*2)
      CHARACTER C(0:16*(2*JM*IM+JM*6+IM*8)-1+NALIGN-1)*1      
      DIMENSION IP(2)

      CALL FJGTAD(C(0),IP)

      IF(IP(1).LT.0) THEN
        ID=NALIGN+MOD(IP(1),NALIGN)
      ELSE
        ID=MOD(IP(1),NALIGN)
      END IF
      ID=MOD(NALIGN-ID,NALIGN)

      ID1=ID+JM*IM*16
      ID2=ID1+JM*IM*16
      ID3=ID2+JM*6*16

*     G,W,ITJ,ITI の先頭が 16バイトアラインされるようにする
      CALL SUB(JM,IM,LM,KM,C(ID),C(ID1),C(ID2),C(ID3),TJ2,TI2,S)

      END
******************************************************
      SUBROUTINE SUB(JM,IM,LM,KM,G,W,TJ,TI,TJ2,TI2,S)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NTR=1)
      DIMENSION S(-LM:LM,-KM:KM)
      DIMENSION G(JM*IM)
      DIMENSION W(JM*IM)
      DIMENSION ITJ(4),ITI(4)
      DIMENSION ITJ2(5),ITI2(5)

      CALL PZINIT(JM,IM,ITJ,TJ,ITI,TI)
      CALL P2INIT(JM,IM,ITJ2,TJ2,ITI2,TI2)      

      DO K=-KM,KM
        DO L=-LM,LM
          S(L,K)=K+1D-3*L
        END DO
      END DO

      CALL APTIME(TIM0)
      DO ITR=1,NTR
        CALL P2S2GA(LM,KM,JM,IM,S,G,W,ITJ2,TJ2,ITI2,TI2)
        CALL P2G2SA(LM,KM,JM,IM,G,S,W,ITJ2,TJ2,ITI2,TI2)
      END DO
      CALL APTIME(TIM1)
      print *,TIM1-TIM0            

      CALL APTIME(TIM0)
      DO ITR=1,NTR
        CALL PZS2GA(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI)
        CALL PZG2SA(LM,KM,JM,IM,G,S,W,ITJ,TJ,ITI,TI)
      END DO
      CALL APTIME(TIM1)
      print *,TIM1-TIM0            

      DO K=-KM,KM
        DO L=-LM,LM
          IF(ABS(S(L,K)-(K+1D-3*L)).GT.1D-10) THEN
            WRITE(6,*) L,K,S(L,K),(K+1D-3*L),S(L,K)-(K+1D-3*L)
          END IF
        END DO
      END DO



      END
