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
      SUBROUTINE CHKSUB(N,ZIN,ZOUT,ZWORK,ZT,ZINS,ZDRCT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(ZI=(0,1))
      PARAMETER(EPS=1D-15)      
      DIMENSION ZIN(0:N-1),ZOUT(0:N-1),ZWORK(0:N-1),ZT(N*2)
      DIMENSION ZINS(0:N-1),ZDRCT(0:N-1)
      DIMENSION IP(2)

      PI=4*ATAN(1D0)

      DO I=0,N-1
        THETA=2*PI*(I+0.25D0)/N
        ZINS(I)=EXP(ZI*THETA*(1+COS(THETA))*N/4)
      END DO
      
      ANOR=0
      DO I=0,N-1
        ZDRCT(I)=0
        DO J=0,N-1
          THETA=2*PI*I/N
          ZDRCT(I)=ZDRCT(I)+ZINS(J)*EXP(ZI*J*THETA)
        END DO
        ANOR=ANOR+ABS(ZDRCT(I))**2        
      END DO

      print *,'== checking fft',N,' =='

*------ checking in-place backward transform -----

      print *,'--- checking in-place backward transform ---'

      DO I=0,N-1
        ZIN(I)=ZINS(I)
        ZOUT(I)=0
        ZWORK(I)=0
        ZT(I+1)=0
        ZT(I+1+N)=0        
      END DO
      IP(1)=0
      IP(2)=0
      CALL FJCINI(N,1,1,IP,ZT)

      
      CALL FJCRUN(ZIN,ZOUT,ZWORK,ZT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(ZIN(I)-ZDRCT(I))**2
      END DO
      IF(SQRT(BNOR/ANOR)/N.LT.EPS) THEN
        print *,'output value: OK'
      ELSE
        print *,'output value: ERROR'
        STOP
      END IF
      
*------ checking in-place forward transform -----

      print *,'--- checking in-place forward transform ---'

      DO I=0,N-1
        ZIN(I)=CONJG(ZINS(I))
      END DO
      CALL FJCINI(N,1,2,IP,ZT)
      CALL FJCRUN(ZIN,ZOUT,ZWORK,ZT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(CONJG(ZIN(I))-ZDRCT(I))**2
      END DO
      IF(SQRT(BNOR/ANOR)/N.LT.EPS) THEN
        print *,'output value: OK'
      ELSE
        print *,'output value: ERROR'
        STOP 
      END IF
      
*------ checking out-of-place backward transform -----

      print *,'--- checking out-of-place backward transform ---'

      DO I=0,N-1
        ZIN(I)=ZINS(I)
      END DO
      CALL FJCINI(N,2,1,IP,ZT)
      CALL FJCRUN(ZIN,ZOUT,ZWORK,ZT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(ZOUT(I)-ZDRCT(I))**2
      END DO
      IF(SQRT(BNOR/ANOR)/N.LT.EPS) THEN
        print *,'output value: OK'
      ELSE
        print *,'output value: ERROR'
        STOP
      END IF

      CNOR=0
      DO I=0,N-1
        CNOR=CNOR+ABS(ZIN(I)-ZINS(I))
      END DO
      IF(CNOR.EQ.0) THEN
        print *,'input value conservation: OK'
      ELSE
        print *,'input value conservation: ERROR'
        STOP
      END IF
      
*------ checking out-of-place forward transform -----

      print *,'--- checking out-of-place forward transform ---'

      DO I=0,N-1
        ZIN(I)=CONJG(ZINS(I))
      END DO
      CALL FJCINI(N,2,2,IP,ZT)
      CALL FJCRUN(ZIN,ZOUT,ZWORK,ZT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(CONJG(ZOUT(I))-ZDRCT(I))**2
      END DO
      IF(SQRT(BNOR/ANOR)/N.LT.EPS) THEN
        print *,'output value: OK'
      ELSE
        print *,'output value: ERROR'
        STOP
      END IF

      CNOR=0
      DO I=0,N-1
        CNOR=CNOR+ABS(ZIN(I)-CONJG(ZINS(I)))
      END DO
      IF(CNOR.EQ.0) THEN
        print *,'input value conservation: OK'
      ELSE
        print *,'input value conservation: ERROR'
        STOP
      END IF

      END
