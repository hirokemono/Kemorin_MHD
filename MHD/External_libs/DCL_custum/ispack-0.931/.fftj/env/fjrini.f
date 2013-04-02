************************************************************************
* FTTJ:  An FFT library
* Copyright (C) 2008 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
      SUBROUTINE FJRINI(N,IO,IBF,IP,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      DIMENSION IP(2),ZT(*)
      EXTERNAL FJRIB1,FJRIF1,FJROB1,FJROF1
      EXTERNAL FJRIB2,FJRIF2,FJROB2,FJROF2
      EXTERNAL FJRIB3,FJRIF3,FJROB3,FJROF3
      EXTERNAL FJRIB4,FJRIF4,FJROB4,FJROF4
      EXTERNAL FJRIB5,FJRIF5,FJROB5,FJROF5
      EXTERNAL FJRIB6,FJRIF6,FJROB6,FJROF6
      EXTERNAL FJRIB7,FJRIF7,FJROB7,FJROF7
      EXTERNAL FJRIB8,FJRIF8,FJROB8,FJROF8
      EXTERNAL FJRIB9,FJRIF9,FJROB9,FJROF9
      EXTERNAL FJRIBA,FJRIFA,FJROBA,FJROFA
      EXTERNAL FJRIBB,FJRIFB,FJROBB,FJROFB

      IF((IO.NE.1).AND.(IO.NE.2)) THEN
        WRITE(6,*) 'IO must be 1 (in-place) or 2(out-of-place).'
        STOP
      END IF
      
      IF((IBF.NE.1).AND.(IBF.NE.2)) THEN
        WRITE(6,*) 'IBF must be 1 (backward) or 2(forward).'
        STOP
      END IF

      IF(N.EQ.2) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB1,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB1,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF1,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF1,IP)
          END IF
        END IF
      ELSE IF(N.EQ.4) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB2,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB2,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF2,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF2,IP)
          END IF
        END IF
      ELSE IF(N.EQ.8) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB3,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB3,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF3,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF3,IP)
          END IF
        END IF
      ELSE IF(N.EQ.16) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB4,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB4,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF4,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF4,IP)
          END IF
        END IF
      ELSE IF(N.EQ.32) THEN
        IF(IBF.EQ.1) THEN
          CALL FJRTB5(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB5,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB5,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJRTF5(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF5,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF5,IP)
          END IF
        END IF
      ELSE IF(N.EQ.64) THEN
        IF(IBF.EQ.1) THEN
          CALL FJRTB6(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB6,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB6,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJRTF6(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF6,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF6,IP)
          END IF
        END IF
      ELSE IF(N.EQ.128) THEN
        IF(IBF.EQ.1) THEN
          CALL FJRTB7(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB7,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB7,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJRTF7(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF7,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF7,IP)
          END IF
        END IF
      ELSE IF(N.EQ.256) THEN
        IF(IBF.EQ.1) THEN
          CALL FJRTB8(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB8,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB8,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJRTF8(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF8,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF8,IP)
          END IF
        END IF
      ELSE IF(N.EQ.512) THEN        
        IF(IBF.EQ.1) THEN
          CALL FJRTB9(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIB9,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROB9,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJRTF9(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIF9,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROF9,IP)
          END IF
        END IF
      ELSE IF(N.EQ.1024) THEN        
        IF(IBF.EQ.1) THEN
          CALL FJRTBA(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIBA,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROBA,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJRTFA(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIFA,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROFA,IP)
          END IF
        END IF
      ELSE IF(N.EQ.2048) THEN
        IF(IBF.EQ.1) THEN
          CALL FJRTBB(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJRIBB,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROBB,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJRTFB(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJRIFB,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJROFB,IP)
          END IF
        END IF
*------------------------------
      ELSE
        WRITE(6,*) 'N=',N,' is not suppoted by FFTJ.'
        STOP
      END IF

      END
      
