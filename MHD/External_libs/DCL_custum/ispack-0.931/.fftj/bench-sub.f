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
      SUBROUTINE BNCSUB(N,ZIN,ZOUT,ZWORK,ZT)

      IMPLICIT REAL*8(A-H,O-Y),COMPLEX*16(Z)
      PARAMETER(NTRY=8,NTR0=1024*1024*128)
      DIMENSION ZIN(0:N-1),ZOUT(0:N-1),ZWORK(0:N-1),ZT(N*2)
      DIMENSION IP(2)
      REAL ETIME,T(2)
      CHARACTER CIO*13,CIBF*10

      NTR=NTR0/N

      DO I=0,N-1
        ZIN(I)=0
      END DO
      
      WRITE(6,'(A,I5,A)') '== benchmarking fft',N,' =='

      DO IO=1,2
        IF(IO.EQ.1) THEN
          CIO='    in-place'
        ELSE
          CIO='out-of-place'
        END IF
        DO IBF=1,2
          IF(IBF.EQ.1) THEN
            CIBF='backward:'
          ELSE
            CIBF=' forward:'
          END IF
          CALL FJCINI(N,IO,IBF,IP,ZT)
          DO ITRY=1,NTRY
            SEC=SEC1-SEC0
            SEC0=ETIME(T)            
            DO ITR=1,NTR
              CALL FJCRUN(ZIN,ZOUT,ZWORK,ZT,IP)
            END DO
            SEC1=ETIME(T)
            SEC=SEC1-SEC0
            IF(ITRY.EQ.1) THEN
              SECMIN=SEC
            ELSE IF(SEC.LT.SECMIN) THEN
              SECMIN=SEC
            END IF
          END DO
          GFLOPS=1D0*5*N*LOG(1D0*N)/LOG(2D0)*NTR/SECMIN/1D9
          WRITE(6,'(3A,F6.3,A)') '   ',CIO,CIBF,GFLOPS,' GFlops'
        END DO
      END DO
        
      END
