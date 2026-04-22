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
SUBROUTINE LXSSZV(NM,NN,JM,JV,JR,S1,S2,G1,G2,P,R,Q,WS,IPOW,IFLAG)

  IMPLICIT NONE
  INTEGER(8) :: JM,NM,NN,N,M,J,JV,JR,L,IR,ID,IA,IE,IC,JD,JVD,JB,IL,IPOW,IFLAG
  REAL(8) :: P(JM/2,5)
  REAL(8) :: R(NM/2*3+NM+1) ! NM=0 でも問題ないように
  REAL(8) :: S1(0:NN),S2(0:NN),G1(*),G2(*),GQ1,GQ2
  REAL(8) :: WS(2,0:NN),Q(JV,7,JR)
  
  M=0

  IE=0
  IA=IE+(NM-M)/2*2
  IC=IA+(NM-M+1)/2

  DO L=1,(NN+1-M)/2
     N=2*L-1+M
     WS(1,N)=R(IA+L)*S1(N)
     WS(2,N)=R(IA+L)*S2(N)     
  END DO

  IF(NN-M.LE.1) THEN
     N=M
     WS(1,N)=S1(N)
     WS(2,N)=S2(N)     
  ELSE
     N=M
     WS(1,N)=S1(N)+R(IE+N-M+1)*S1(N+2)
     WS(2,N)=S2(N)+R(IE+N-M+1)*S2(N+2)     
     DO N=M+2,NN-2,2
        WS(1,N)=R(IE+N-M)*S1(N)+R(IE+N-M+1)*S1(N+2)
        WS(2,N)=R(IE+N-M)*S2(N)+R(IE+N-M+1)*S2(N+2)         
     END DO
     WS(1,N)=R(IE+N-M)*S1(N)
     WS(2,N)=R(IE+N-M)*S2(N)     
  END IF

  DO ID=1,JM/(2*JV*JR)

     IF(NN.EQ.M) THEN
        IF(IPOW.EQ.0) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,4,IR)=0
                 Q(J,5,IR)=WS(1,M)
                 Q(J,6,IR)=0
                 Q(J,7,IR)=WS(2,M)
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,4,IR)=0
                 Q(J,5,IR)=WS(1,M)*P(J+JV*(JD-1),4)
                 Q(J,6,IR)=0
                 Q(J,7,IR)=WS(2,M)*P(J+JV*(JD-1),4)
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,4,IR)=0
                 Q(J,5,IR)=WS(1,M)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 Q(J,6,IR)=0
                 Q(J,7,IR)=WS(2,M)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
              END DO
           END DO
        END IF
     ELSE IF(NN.EQ.M+1) THEN
        IF(IPOW.EQ.0) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,4,IR)=WS(1,M+1)
                 Q(J,5,IR)=WS(1,M)
                 Q(J,6,IR)=WS(2,M+1)
                 Q(J,7,IR)=WS(2,M)
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,4,IR)=WS(1,M+1)*P(J+JV*(JD-1),4)
                 Q(J,5,IR)=WS(1,M)*P(J+JV*(JD-1),4)
                 Q(J,6,IR)=WS(2,M+1)*P(J+JV*(JD-1),4)
                 Q(J,7,IR)=WS(2,M)*P(J+JV*(JD-1),4)
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,4,IR)=WS(1,M+1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 Q(J,5,IR)=WS(1,M)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 Q(J,6,IR)=WS(2,M+1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 Q(J,7,IR)=WS(2,M)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
              END DO
           END DO
        END IF
     ELSE
        L=0
        IF(IPOW.EQ.0) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,1,IR)=P(J+JV*(JD-1),5)
                 Q(J,2,IR)=1
                 Q(J,3,IR)=R(IC+2*L+1)*P(J+JV*(JD-1),5)+R(IC+2*L+2)
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,1,IR)=P(J+JV*(JD-1),5)
                 Q(J,2,IR)=P(J+JV*(JD-1),4)
                 Q(J,3,IR)=(R(IC+2*L+1)*P(J+JV*(JD-1),5)+R(IC+2*L+2))*P(J+JV*(JD-1),4)
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,1,IR)=P(J+JV*(JD-1),5)
                 Q(J,2,IR)=P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 Q(J,3,IR)=(R(IC+2*L+1)*P(J+JV*(JD-1),5)+R(IC+2*L+2))*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
              END DO
           END DO
        END IF
        Q(:,4:7,:)=0
        JB=JR
        DO N=M,NN-10,8
           L=(N-M)/2
           IL=IC+2*L+3
           IF(JV.EQ.4) THEN
              CALL LXQSWG(JB,R,WS(1,N),Q,IL,0_8)
           ELSE IF(JV.EQ.8) THEN
              CALL LXOSWG(JB,R,WS(1,N),Q,IL,0_8)
           ELSE
              CALL LXLSWG(JV,JB,R,WS(1,N),Q,IL,0_8)
           END IF
        END DO
        L=(N-M)/2
        IL=IC+2*L+3
        IF(JV.EQ.4) THEN
           CALL LXQSWG(JB,R,WS(1,N),Q,IL,NN-N)           
        ELSE IF(JV.EQ.8) THEN
           CALL LXOSWG(JB,R,WS(1,N),Q,IL,NN-N)
        ELSE           
           CALL LXLSWG(JV,JB,R,WS(1,N),Q,IL,NN-N)
        END IF
     END IF     

     IF(IFLAG.EQ.0) THEN   ! SVTSWG用
        IF(JV.EQ.4) THEN
           CALL LXQPZV(JR,JM,ID,P,Q,G1,G2)
        ELSE IF(JV.EQ.8) THEN
           CALL LXOPZV(JR,JM,ID,P,Q,G1,G2)
        ELSE
           CALL LXLPZV(JV,JR,JM,ID,P,Q,G1,G2)
        END IF
     ELSE
        DO IR=1,JR
           JD=IR+JR*(ID-1)
           JVD=JV*(JD-1)
           DO J=1,JV
              GQ1=Q(J,4,IR)*P(J+JVD,1)
              GQ2=Q(J,6,IR)*P(J+JVD,1)
              G1(JM/2+J+JVD)=GQ1+Q(J,5,IR)
              G2(JM/2+J+JVD)=GQ2+Q(J,7,IR)
              G1(JM/2+1-(J+JVD))=-GQ1+Q(J,5,IR)
              G2(JM/2+1-(J+JVD))=-GQ2+Q(J,7,IR)
           END DO
        END DO
     END IF

  END DO

END SUBROUTINE LXSSZV
