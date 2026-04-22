!***********************************************************************
! ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
! Copyright (C) 1998--2024 Keiichi Ishioka <ishioka@gfd-dennou.org>
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
! 02110-1301 USA.
!***********************************************************************
!-----------------------------------------------------------------------
! initialization of fxpack (complex)
!-----------------------------------------------------------------------
SUBROUTINE FXZINI(N,IT,T)

  IMPLICIT NONE  
  REAL(16) :: PI
  INTEGER(8),PARAMETER :: IMM=-9223372036854775808_8  
  INTEGER(8) :: N,IT(N),J,K,L,I,IS,IC,I4,ITSUM,IT1,ISJ
  INTEGER(8) :: IPN(4)=(/5,4,3,2/),IPC,IP,IPD
  INTEGER(8) :: ITC(2:5)
  REAL(8) :: T(2,0:N-1)    
  INTEGER(8),DIMENSION(:),ALLOCATABLE :: ITW,IW

  IF(N.EQ.1) THEN
     IT=0
     T=0
     RETURN
  END IF

  PI=4*ATAN(1Q0)  

  ALLOCATE(ITW(0:N-1))

  ITC=0
  
  J=N
  DO I=5,2,-1
     ITC(I)=0
     K=0
     DO WHILE(K.EQ.0)
        K=MOD(J,I)
        IF(K.EQ.0) THEN
           ITC(I)=ITC(I)+1
           J=J/I
        END IF
     END DO
  END DO
  IF(J.NE.1) THEN
     PRINT*, '*** error in FXZINI: N.ne.(2**P)*(3**Q)*(5**R)'
     STOP
  END IF

  ITW=0
  K=N
  L=1
  DO IPC=4,1,-1
     IP=IPN(IPC)
     DO IC=1,ITC(IP)
        DO I=0,L-1      
           DO J=0,K/IP-1
              DO IPD=1,IP-1
                 ITW(I+L*(IPD+J*IP))=ITW(I+L*(IPD+J*IP))+IPD*K/IP
              END DO
           END DO
        END DO
        K=K/IP
        L=L*IP
     END DO
  END DO

  T=0
  K=N
  L=1
  IS=0
  DO IPC=1,4
     IP=IPN(IPC)
     DO IC=1,ITC(IP)
        DO I=0,L-1
           J=ITW(K*I)
           DO IPD=1,IP-1
              T(1,IS+(IP-1)*I+IPD-1)=COS(2*PI*J*IPD/(IP*L))
              T(2,IS+(IP-1)*I+IPD-1)=SIN(2*PI*J*IPD/(IP*L))
           END DO
        END DO
        IS=IS+L*(IP-1)        
        K=K/IP
        L=L*IP
     END DO
  END DO

  ITW=0
  K=N
  L=1
  DO IPC=1,4
     IP=IPN(IPC)
     DO IC=1,ITC(IP)
        DO I=0,L-1      
           DO J=0,K/IP-1
              DO IPD=1,IP-1
                 ITW(I+L*(IPD+J*IP))=ITW(I+L*(IPD+J*IP))+IPD*K/IP
              END DO
           END DO
        END DO
        K=K/IP
        L=L*IP
     END DO
  END DO

  ITSUM=0
  DO I=2,5
     ITSUM=ITSUM+ITC(I)
  END DO

  I4=1
  IT1=0
  DO IPC=1,4
     IP=IPN(IPC)
     DO IC=1,ITC(IP)
        IT1=IT1+(IP-2)*I4
        I4=I4*4
     END DO
  END DO

  IT(N-1)=IT1
  IT(N)=ITSUM

  IF(N.GE.3) THEN
     ALLOCATE(IW(N-2))
     IW=0
     IC=0
     DO J=1,N-2
        IF(IW(J).EQ.1) THEN
           CYCLE
        END IF
        IC=IC+1
        IT(IC)=J
        IW(J)=1
        ISJ=ITW(J)
        DO
           IF(ISJ.EQ.J) THEN
              IT(IC)=IT(IC)+IMM
              EXIT
           END IF
           IC=IC+1
           IT(IC)=ISJ
           IW(ISJ)=1             
           ISJ=ITW(ISJ)
        END DO
     END DO
     DEALLOCATE(IW)
  END IF
     
  DEALLOCATE(ITW)

END SUBROUTINE FXZINI
!-----------------------------------------------------------------------
! complex backward FFT (in-place)
!-----------------------------------------------------------------------
SUBROUTINE FXZTBA(M,N,X,IT,T)

  IMPLICIT NONE  
  INTEGER(8) :: M,N,K,L,IT(N),ITSUM,IT1,IC,IS,J1,IP
  REAL(8) :: T(N*2),X(M*2,0:N-1)

  IF(N.EQ.1) THEN
     RETURN
  END IF

  ITSUM=IT(N)
  IT1=IT(N-1)
  
  K=1
  L=N
  IC=1
  IS=0
  J1=0

  IP=MOD(IT1,4)+2
  CALL FXZTB0(M,L,X,IP)       
  CALL FXZTAL(M,K,L,X,T,IC,ITSUM,IS,J1,IT1)
  CALL FXZTBP(M,N,X,IT)

END SUBROUTINE FXZTBA
!-----------------------------------------------------------------------
! complex forward FFT 
!-----------------------------------------------------------------------
SUBROUTINE FXZTFA(M,N,X,IT,T)

  IMPLICIT NONE  
  INTEGER(8) :: M,N,K,L,IT(N),ITSUM,IT1,IC,IS,J1,IP
  REAL(8) :: T(N*2),X(M*2,0:N-1)

  IF(N.EQ.1) THEN
     RETURN
  END IF

  ITSUM=IT(N)
  IT1=IT(N-1)
  
  K=1
  L=N
  IC=1
  IS=0
  J1=0

  IP=MOD(IT1,4)+2
  CALL FXZTF0(M,L,X,IP)
  CALL FXZTAL(M,K,L,X,T,IC,ITSUM,IS,J1,IT1)
  CALL FXZTFP(M,N,X,IT)

END SUBROUTINE FXZTFA
!-----------------------------------------------------------------------
! permutation for backward FFT
!-----------------------------------------------------------------------
SUBROUTINE FXZTBP(M,N,X,IT)

  IMPLICIT NONE  
  INTEGER(8) :: M,N,IT(*)
  REAL(8) :: X(*)

  IF(M.EQ.4) THEN
     CALL FXZQBP(N,X,IT)
  ELSE IF(M.EQ.8) THEN
     CALL FXZOBP(N,X,IT)
  ELSE     
     CALL FXZMBP(M,N,X,IT)
  END IF
  
END SUBROUTINE FXZTBP
!-----------------------------------------------------------------------
! permutation for forward FFT
!-----------------------------------------------------------------------
SUBROUTINE FXZTFP(M,N,X,IT)

  IMPLICIT NONE  
  INTEGER(8) :: M,N,IT(*)
  REAL(8) :: X(*)
  
  IF(M.EQ.4) THEN
     CALL FXZQFP(N,X,IT)
  ELSE IF(M.EQ.8) THEN
     CALL FXZOFP(N,X,IT)     
  ELSE
     CALL FXZMFP(M,N,X,IT)
  END IF

END SUBROUTINE FXZTFP
!-----------------------------------------------------------------------
!  lower routine for complex FFT
!-----------------------------------------------------------------------
RECURSIVE SUBROUTINE FXZTAL(M,K,L,X,T,IC,ITSUM,IS,J1,IT1)

  IMPLICIT NONE
  INTEGER(8) :: M,K,L,IC,ITSUM,IS,J1,IT1
  INTEGER(8) :: IP,J,ISD,KD,LD,K1,JD,JDD,ICD,ICDD,IT1D
  REAL(8) :: X(M,2,0:*),T(2,0:*)

  KD=K
  LD=L
  K1=1
  J=J1

  IT1D=IT1
  IP=MOD(IT1D,4)+2
  IF(IC.NE.1) THEN
     CALL FXZTAK(M,K1,LD,X(1,1,J*LD),T(1,IS+J*(IP-1)),IP)
  END IF
  K1=K1*IP
  LD=LD/IP
  ISD=IS+KD*(IP-1)
  KD=KD*IP  
  J=J*IP
  IT1D=IT1D/4
  
  ICD=IC+1  
  IF(M*IP*LD.GT.2048.AND.ICD.LE.ITSUM) THEN
     DO JD=0,IP-1
        JDD=J+JD
        CALL FXZTAL(M,KD,LD,X,T,ICD,ITSUM,ISD,JDD,IT1D)
     END DO
  ELSE
     DO ICDD=ICD,ITSUM
        IP=MOD(IT1D,4)+2
        CALL FXZTAK(M,K1,LD,X(1,1,J*LD),T(1,ISD+J*(IP-1)),IP)
        K1=K1*IP
        LD=LD/IP
        ISD=ISD+KD*(IP-1)        
        KD=KD*IP
        J=J*IP                             
        IT1D=IT1D/4
     END DO
  END IF
  
END SUBROUTINE FXZTAL
!-----------------------------------------------------------------------
! short DFT (j=0) (bwd)
!-----------------------------------------------------------------------
SUBROUTINE FXZTB0(M,L,X,IP)

  IMPLICIT NONE
  INTEGER(8) :: M,L,IP
  REAL(8) :: X(*)

  IF(M.EQ.4) THEN
     IF(IP.EQ.4) THEN
        CALL FXZQ4B(L,X)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZQ2B(L,X)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZQ3B(L,X)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZQ5B(L,X)
     END IF
  ELSE IF(M.EQ.8) THEN     
     IF(IP.EQ.4) THEN
        CALL FXZO4B(L,X)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZO2B(L,X)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZO3B(L,X)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZO5B(L,X)
     END IF
  ELSE
     IF(IP.EQ.4) THEN
        CALL FXZM4B(M,L,X)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZM2B(M,L,X)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZM3B(M,L,X)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZM5B(M,L,X)
     END IF
  END IF

END SUBROUTINE FXZTB0
!-----------------------------------------------------------------------
! short DFT (j=0) (fwd)
!-----------------------------------------------------------------------
SUBROUTINE FXZTF0(M,L,X,IP)

  IMPLICIT NONE
  INTEGER(8) :: M,L,IP
  REAL(8) :: X(*)

  IF(M.EQ.4) THEN  
     IF(IP.EQ.4) THEN
        CALL FXZQ4F(L,X)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZQ2F(L,X)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZQ3F(L,X)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZQ5F(L,X)
     END IF
  ELSE IF(M.EQ.8) THEN
     IF(IP.EQ.4) THEN
        CALL FXZO4F(L,X)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZO2F(L,X)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZO3F(L,X)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZO5F(L,X)
     END IF
  ELSE
     IF(IP.EQ.4) THEN
        CALL FXZM4F(M,L,X)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZM2F(M,L,X)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZM3F(M,L,X)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZM5F(M,L,X)
     END IF
  END IF

END SUBROUTINE FXZTF0
!-----------------------------------------------------------------------
! short DFT
!-----------------------------------------------------------------------
SUBROUTINE FXZTAK(M,K,L,X,T,IP)

  IMPLICIT NONE
  INTEGER(8) :: M,K,L,IP
  REAL(8) :: X(*),T(*)

  IF(M.EQ.4) THEN    
     IF(IP.EQ.4) THEN
        CALL FXZQ4A(K,L,X,T)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZQ2A(K,L,X,T)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZQ3A(K,L,X,T)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZQ5A(K,L,X,T)
     END IF
  ELSE IF(M.EQ.8) THEN    
     IF(IP.EQ.4) THEN
        CALL FXZO4A(K,L,X,T)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZO2A(K,L,X,T)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZO3A(K,L,X,T)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZO5A(K,L,X,T)
     END IF
  ELSE
     IF(IP.EQ.4) THEN
        CALL FXZM4A(M,K,L,X,T)
     ELSE IF(IP.EQ.2) THEN
        CALL FXZM2A(M,K,L,X,T)
     ELSE IF(IP.EQ.3) THEN
        CALL FXZM3A(M,K,L,X,T)
     ELSE IF(IP.EQ.5) THEN
        CALL FXZM5A(M,K,L,X,T)
     END IF
  END IF

END SUBROUTINE FXZTAK
