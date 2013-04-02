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
      SUBROUTINE FJCINI(N,IO,IBF,IP,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      DIMENSION IP(2),ZT(*)
      EXTERNAL FJCIB1,FJCIF1,FJCOB1,FJCOF1
      EXTERNAL FJCIB2,FJCIF2,FJCOB2,FJCOF2
      EXTERNAL FJCIB3,FJCIF3,FJCOB3,FJCOF3
      EXTERNAL FJCIB4,FJCIF4,FJCOB4,FJCOF4
      EXTERNAL FJCIB5,FJCIF5,FJCOB5,FJCOF5
      EXTERNAL FJCIB6,FJCIF6,FJCOB6,FJCOF6
      EXTERNAL FJCIB7,FJCIF7,FJCOB7,FJCOF7
      EXTERNAL FJCIB8,FJCIF8,FJCOB8,FJCOF8
      EXTERNAL FJCIB9,FJCIF9,FJCOB9,FJCOF9
      EXTERNAL FJCIBA,FJCIFA,FJCOBA,FJCOFA

      IF((IO.NE.1).AND.(IO.NE.2)) THEN
        WRITE(6,*) 'IO must be 1 (in-place) or 2(out-of-place).'
        STOP
      END IF
      
      IF((IBF.NE.1).AND.(IBF.NE.2)) THEN
        WRITE(6,*) 'IBF must be 1 (backward) or 2(forward).'
        STOP
      END IF

      IF(N.EQ.2) THEN
*---------------------------------                
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIB1,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB1,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN            
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF1,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF1,IP)
          END IF
        END IF
*---------------------------------
      ELSE IF(N.EQ.4) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIB2,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB2,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN            
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF2,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF2,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.8) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIB3,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB3,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN            
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF3,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF3,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.16) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIB4,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB4,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN            
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF4,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF4,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.32) THEN
        IF(IBF.EQ.1) THEN
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJCIB5,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB5,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN            
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF5,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF5,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.64) THEN
        IF(IBF.EQ.1) THEN
          CALL FJCTB6(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJCIB6,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB6,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJCTF6(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF6,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF6,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.128) THEN
        IF(IBF.EQ.1) THEN
          CALL FJCTB7(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJCIB7,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB7,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJCTF7(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF7,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF7,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.256) THEN
        IF(IBF.EQ.1) THEN
          CALL FJCTB8(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJCIB8,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB8,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJCTF8(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF8,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF8,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.512) THEN
        IF(IBF.EQ.1) THEN
          CALL FJCTB9(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJCIB9,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOB9,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJCTF9(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIF9,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOF9,IP)
          END IF
        END IF
*------------------------------
      ELSE IF(N.EQ.1024) THEN
        IF(IBF.EQ.1) THEN
          CALL FJCTBA(ZT)
          IF(IO.EQ.1) THEN
            CALL FJGTAD(FJCIBA,IP)          
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOBA,IP)
          END IF
        ELSE IF(IBF.EQ.2) THEN
          CALL FJCTFA(ZT)
          IF(IO.EQ.1) THEN          
            CALL FJGTAD(FJCIFA,IP)
          ELSE IF(IO.EQ.2) THEN
            CALL FJGTAD(FJCOFA,IP)
          END IF
        END IF
*------------------------------
      ELSE
        WRITE(6,*) 'N=',N,' is not suppoted by FFTJ.'
        STOP
      END IF

      END
      
