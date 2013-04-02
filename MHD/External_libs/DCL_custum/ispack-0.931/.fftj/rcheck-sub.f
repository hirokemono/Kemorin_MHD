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
      SUBROUTINE RCHKSB(N,XIN,XOUT,XWORK,XT,XINS,XDRCT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(ZI=(0,1))
      PARAMETER(EPS=1D-15)      
      DIMENSION XIN(0:N-1),XOUT(0:N-1),XWORK(0:N-1),XT(N*3)
      DIMENSION XINS(0:N-1),XDRCT(0:N-1)
      DIMENSION IP(2)

      PI=4*ATAN(1D0)

      DO I=0,N/2-1
        THETA=2*PI*(I+0.25D0)/(N/2)
        XINS(2*I)=COS(THETA*(1+COS(THETA))*(N/2)/4)
        XINS(2*I+1)=SIN(THETA*(1+COS(THETA))*(N/2)/4)        
      END DO
      
      ANOR=0
      DO I=0,N-1
        XDRCT(I)=XINS(0)+XINS(1)*(-1)**I
        DO J=1,N/2-1
          THETA=2*PI*I/N
          XDRCT(I)=XDRCT(I)
     &      +2*(XINS(2*J)+XINS(2*J+1)*ZI)*EXP(ZI*J*THETA)
        END DO
        ANOR=ANOR+XDRCT(I)**2        
      END DO

      print *,'== checking fft',N,' =='

*------ checking in-place backward transform -----

      print *,'--- checking in-place backward transform ---'

      DO I=0,N-1
        XIN(I)=XINS(I)
        XOUT(I)=0
        XWORK(I)=0
      END DO
      DO I=1,3*N
        XT(I)=0
      END DO
      
      IP(1)=0
      IP(2)=0
      CALL FJRINI(N,1,1,IP,XT)
      
      CALL FJRRUN(XIN,XOUT,XWORK,XT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(XIN(I)-XDRCT(I))**2
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
        XIN(I)=XDRCT(I)
      END DO
      CALL FJRINI(N,1,2,IP,XT)
      CALL FJRRUN(XIN,XOUT,XWORK,XT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(XIN(I)/N-XINS(I))**2
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
        XIN(I)=XINS(I)
        XOUT(I)=0
        XWORK(I)=0
      END DO
      DO I=1,3*N
        XT(I)=0
      END DO
      
      IP(1)=0
      IP(2)=0
      CALL FJRINI(N,2,1,IP,XT)
      
      CALL FJRRUN(XIN,XOUT,XWORK,XT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(XOUT(I)-XDRCT(I))**2
      END DO
      IF(SQRT(BNOR/ANOR)/N.LT.EPS) THEN
        print *,'output value: OK'
      ELSE
        print *,'output value: ERROR'
        STOP
      END IF
      
      CNOR=0
      DO I=0,N-1
        CNOR=CNOR+ABS(XIN(I)-XINS(I))
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
        XIN(I)=XDRCT(I)
      END DO
      CALL FJRINI(N,2,2,IP,XT)
      CALL FJRRUN(XIN,XOUT,XWORK,XT,IP)

      BNOR=0
      DO I=0,N-1
        BNOR=BNOR+ABS(XOUT(I)/N-XINS(I))**2
      END DO
      IF(SQRT(BNOR/ANOR)/N.LT.EPS) THEN
        print *,'output value: OK'
      ELSE
        print *,'output value: ERROR'
        STOP 
      END IF

      CNOR=0
      DO I=0,N-1
        CNOR=CNOR+ABS(XIN(I)-XDRCT(I))
      END DO
      IF(CNOR.EQ.0) THEN
        print *,'input value conservation: OK'
      ELSE
        print *,'input value conservation: ERROR'
        STOP
      END IF

      END
