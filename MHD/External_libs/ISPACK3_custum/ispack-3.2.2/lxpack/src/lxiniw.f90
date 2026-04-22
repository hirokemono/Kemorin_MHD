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
SUBROUTINE LXINIW(NM,JM,M,P,PM,R,JC)

  IMPLICIT NONE
  INTEGER(8) :: JM,NM,N,M,IE,IA,IC,L,MD,LD,J,JH,IJ
  REAL(8) :: P(JM/2,5)
  REAL(16) :: QBIGL,QCHK1,QCHK2,QPME
  REAL(8) :: DBIG,DBIGR,DBIGL
  REAL(8) :: DCHK1,DCHK2,DEPS,DPME,DEPSL
  REAL(16),DIMENSION(:),ALLOCATABLE :: QWP
  REAL(8),DIMENSION(:),ALLOCATABLE :: DWP    
  REAL(8) :: PM(JM/2,2)
  REAL(8) :: R((NM-M)/2*3+NM-M+1)
  INTEGER(8) :: JC((NM-M)/8+1)

  ALLOCATE(QWP(JM/2))
  ALLOCATE(DWP(JM/2))  

  DEPS=1D-25
  DEPSL=LOG(DEPS)
  
  QPME=0
  DO MD=1,M
     QPME=QPME+0.5Q0*LOG(1Q0*(2*MD+1)/(2*MD))
  END DO
  DPME=QPME

  JC=0

  DBIG=(2D0)**900
  DBIGR=1/DBIG
  QBIGL=LOG(1Q0*DBIG)
  DBIGL=LOG(DBIG)  

  IE=0
  IA=IE+(NM-M)/2*2
  IC=IA+(NM-M+1)/2
  IJ=0

  JH=JM/2

  PM=0
  
  IF(NM.LE.M+1) THEN
     L=0
     LD=1
     DO J=1,JH
        PM(J,1)=1
        PM(J,2)=0
     END DO
     DO J=JH,1,-1
        DCHK1=DPME+LOG(ABS(PM(J,1)))+M*LOG(P(J,3))
        IF(DCHK1.GT.DEPSL) THEN
           JC(IJ+LD)=J
           EXIT
        END IF
     END DO
     DO J=1,JC(IJ+LD)
        QCHK1=QPME+LOG(ABS(1Q0*PM(J,1)))+M*LOG(1Q0*P(J,3))
        PM(J,1)=SIGN(EXP(QCHK1),1Q0*PM(J,1))
     END DO
  ELSE
     L=0
     LD=1
     DO J=1,JH
        PM(J,1)=1
        PM(J,2)=R(IC+2*L+1)*P(J,5)+R(IC+2*L+2)
     END DO
     DO J=JH,1,-1
        DCHK1=DPME+LOG(ABS(PM(J,1)))+M*LOG(P(J,3))
        DCHK2=DPME+LOG(ABS(PM(J,2)))+M*LOG(P(J,3))
        IF(DCHK2.GT.DEPSL.OR.DCHK1.GT.DEPSL) THEN
           JC(IJ+LD)=J
           EXIT
        END IF
     END DO
     DO J=1,JC(IJ+LD)
        QCHK1=QPME+LOG(ABS(1Q0*PM(J,1)))+M*LOG(1Q0*P(J,3))
        QCHK2=QPME+LOG(ABS(1Q0*PM(J,2)))+M*LOG(1Q0*P(J,3))
        PM(J,1)=SIGN(EXP(QCHK1),1Q0*PM(J,1))
        PM(J,2)=SIGN(EXP(QCHK2),1Q0*PM(J,2))
     END DO

     DO J=JC(IJ+LD)+1,JH
        QWP(J)=QPME+M*LOG(1Q0*P(J,3))
        DWP(J)=QWP(J)
     END DO

     DO N=M,NM-10,8
        L=(N-M)/2
        LD=L/4+1

        JC(IJ+LD+1)=JC(IJ+LD)
        DO J=JC(IJ+LD)+1,JH
           PM(J,1)=PM(J,1)+(R(IC+2*L+3)*P(J,5)+R(IC+2*L+4))*PM(J,2)
           PM(J,2)=PM(J,2)+(R(IC+2*L+5)*P(J,5)+R(IC+2*L+6))*PM(J,1)
           PM(J,1)=PM(J,1)+(R(IC+2*L+7)*P(J,5)+R(IC+2*L+8))*PM(J,2)
           PM(J,2)=PM(J,2)+(R(IC+2*L+9)*P(J,5)+R(IC+2*L+10))*PM(J,1)
           IF(ABS(PM(J,1)).GT.DBIG.OR.ABS(PM(J,2)).GT.DBIG) THEN
              PM(J,2)=PM(J,2)*DBIGR
              PM(J,1)=PM(J,1)*DBIGR
              QWP(J)=QWP(J)+QBIGL
              DWP(J)=DWP(J)+DBIGL              
           END IF
        END DO
        DO J=JH,JC(IJ+LD)+1,-1
           DCHK1=LOG(ABS(PM(J,1)))+DWP(J)
           DCHK2=LOG(ABS(PM(J,2)))+DWP(J)
           IF(DCHK2.GT.DEPSL.OR.DCHK1.GT.DEPSL) THEN
              JC(IJ+LD+1)=J
              EXIT
           END IF
        END DO
        DO J=JC(IJ+LD)+1,JC(IJ+LD+1)
           QCHK1=LOG(ABS(1Q0*PM(J,1)))+QWP(J)
           QCHK2=LOG(ABS(1Q0*PM(J,2)))+QWP(J)
           PM(J,1)=SIGN(EXP(QCHK1),1Q0*PM(J,1))
           PM(J,2)=SIGN(EXP(QCHK2),1Q0*PM(J,2))
        END DO
     END DO

  END IF

  DEALLOCATE(QWP)
  DEALLOCATE(DWP)  

END SUBROUTINE LXINIW

