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
SUBROUTINE LXSGZS(NM,NN,JM,JV,JR,S,G,P,R,Q,WS,IPOW,IFLAG)

  IMPLICIT NONE
  INTEGER(8) :: JM,NM,NN,N,M,J,JV,JR,L,IR,ID,IA,IE,IC,JD,JVD,JB,IL,IPOW,IFLAG
  REAL(8) :: P(JM/2,5)
  REAL(8) :: R(NM/2*3+NM+1) ! NM=0 でも問題ないように
  REAL(8) :: S(0:NN),G(*)
  REAL(8) :: WS(0:NN),Q(JV,5,JR)  

  M=0

  IE=0
  IA=IE+(NM-M)/2*2
  IC=IA+(NM-M+1)/2

  S=0

  DO ID=1,JM/(2*JV*JR)
     IF(IFLAG.EQ.0) THEN   ! SVTG2S用
        IF(JV.EQ.4) THEN
           CALL LXQGZP(JR,JM,ID,P,Q,G)
        ELSE IF(JV.EQ.8) THEN
           CALL LXOGZP(JR,JM,ID,P,Q,G)
        ELSE
           CALL LXLGZP(JV,JR,JM,ID,P,Q,G)
        END IF
     ELSE
        DO IR=1,JR
           JD=IR+JR*(ID-1)
           JVD=JV*(JD-1)
           DO J=1,JV
              Q(J,5,IR)=(G(JM/2+J+JVD)+G(JM/2+1-(J+JVD)))*P(J+JVD,2)
              Q(J,4,IR)=(G(JM/2+J+JVD)-G(JM/2+1-(J+JVD)))*P(J+JVD,1)*P(J+JVD,2)
           END DO
        END DO
     END IF
     
     IF(NN.EQ.M) THEN
        IF(IPOW.EQ.0) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 S(M)=S(M)+Q(J,5,IR)
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 S(M)=S(M)+Q(J,5,IR)*P(J+JV*(JD-1),4)
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 S(M)=S(M)+Q(J,5,IR)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
              END DO
           END DO
        END IF
     ELSE IF(NN.EQ.M+1) THEN
        IF(IPOW.EQ.0) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 S(M+1)=S(M+1)+Q(J,4,IR)
                 S(M)=S(M)+Q(J,5,IR)              
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 S(M+1)=S(M+1)+Q(J,4,IR)*P(J+JV*(JD-1),4)
                 S(M)=S(M)+Q(J,5,IR)*P(J+JV*(JD-1),4)
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 S(M+1)=S(M+1)+Q(J,4,IR)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 S(M)=S(M)+Q(J,5,IR)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
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

        JB=JR

        DO N=M,NN-10,8
           L=(N-M)/2
           IL=IC+2*L+3
           IF(JV.EQ.4) THEN
              CALL LXQGZS(JB,R,S(N),Q,IL,0_8)                      
           ELSE IF(JV.EQ.8) THEN
              CALL LXOGZS(JB,R,S(N),Q,IL,0_8)                      
           ELSE IF(JV.EQ.256) THEN
              CALL LXXGZS(JB,R,S(N),Q,IL,0_8)                      
           ELSE
              CALL LXLGZS(JV,JB,R,S(N),Q,IL,0_8)
           END IF
        END DO

        L=(N-M)/2
        IL=IC+2*L+3
        IF(JV.EQ.4) THEN        
           CALL LXQGZS(JB,R,S(N),Q,IL,NN-N)
        ELSE IF(JV.EQ.8) THEN        
           CALL LXOGZS(JB,R,S(N),Q,IL,NN-N)
        ELSE IF(JV.EQ.256) THEN        
           CALL LXXGZS(JB,R,S(N),Q,IL,NN-N)
        ELSE
           CALL LXLGZS(JV,JB,R,S(N),Q,IL,NN-N)           
        END IF
     END IF
  END DO

  DO L=1,(NN+1-M)/2
     N=2*L-1+M
     S(N)=R(IA+L)*S(N)
  END DO

  DO N=(NN-M)/2*2+M,M+2,-2
     S(N)=R(IE+N-M)*S(N)+R(IE+N-M-1)*S(N-2) 
  END DO

END SUBROUTINE LXSGZS
