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
SUBROUTINE LXSVWS(NM,NN,JM,JV,JR,M,S1,S2,G1,G2,P,PM,R,JC,Q,WS,IPOW,IFLAG)

  IMPLICIT NONE
  INTEGER(8) :: JM,NM,NN,N,M,J,JV,JR,L,IR,ID,IA,IE,IC,JD,JVD,JB,IL,JS,JE
  INTEGER(8) :: JCT,JC1,JC2,JCT1,JCT2,ID1,ID2,LD,IJ,IPOW,IFLAG
  REAL(8) :: P(JM/2,5),PM(JM/2,2)
  !  REAL(8) :: R(NM/2*3+NM+1)
  REAL(8) :: R(*)
  REAL(8) :: S1(2,M:NN),S2(2,M:NN),G1(*),G2(*)
  INTEGER(8) :: JC(*)
  REAL(8) :: WS(4,M:NN),Q(JV,11,JR)    

  IE=0
  IA=IE+(NM-M)/2*2
  IC=IA+(NM-M+1)/2

  IJ=0
  WS=0
  S1=0
  S2=0    

  DO ID=1,JM/(2*JV*JR)
     IF(IFLAG.EQ.0) THEN
        IF(JV.EQ.4) THEN
           CALL LXQVWP(JR,JM,ID,P,Q,G1,G2)           
        ELSE IF(JV.EQ.8) THEN
           CALL LXOVWP(JR,JM,ID,P,Q,G1,G2)                      
        ELSE
           CALL LXLVWP(JV,JR,JM,ID,P,Q,G1,G2)
        END IF
     ELSE 
        DO IR=1,JR
           JD=IR+JR*(ID-1)
           JVD=JV*(JD-1)
           DO J=1,JV
              Q(J,5,IR)=(G1(1+2*(JM/2+J+JVD-1))+G1(1+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,2)
              Q(J,4,IR)=(G1(1+2*(JM/2+J+JVD-1))-G1(1+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,1)*P(J+JVD,2)
              Q(J,7,IR)=(G1(2+2*(JM/2+J+JVD-1))+G1(2+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,2)
              Q(J,6,IR)=(G1(2+2*(JM/2+J+JVD-1))-G1(2+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,1)*P(J+JVD,2)
              Q(J,9,IR)=(G2(1+2*(JM/2+J+JVD-1))+G2(1+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,2)
              Q(J,8,IR)=(G2(1+2*(JM/2+J+JVD-1))-G2(1+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,1)*P(J+JVD,2)
              Q(J,11,IR)=(G2(2+2*(JM/2+J+JVD-1))+G2(2+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,2)
              Q(J,10,IR)=(G2(2+2*(JM/2+J+JVD-1))-G2(2+2*(JM/2+1-(J+JVD)-1)))*P(J+JVD,1)*P(J+JVD,2)
           END DO
        END DO
     END IF

     IF(NN.EQ.M) THEN
        IF(IPOW.EQ.0) THEN        
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))                      
              DO J=1,JCT
                 WS(1,M)=WS(1,M)+Q(J,5,IR)*PM(J+JV*(JD-1),1)
                 WS(2,M)=WS(2,M)+Q(J,7,IR)*PM(J+JV*(JD-1),1)
                 WS(3,M)=WS(3,M)+Q(J,9,IR)*PM(J+JV*(JD-1),1)
                 WS(4,M)=WS(4,M)+Q(J,11,IR)*PM(J+JV*(JD-1),1)
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN        
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))                      
              DO J=1,JCT
                 WS(1,M)=WS(1,M)+Q(J,5,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(2,M)=WS(2,M)+Q(J,7,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(3,M)=WS(3,M)+Q(J,9,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(4,M)=WS(4,M)+Q(J,11,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN        
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))                      
              DO J=1,JCT
                 WS(1,M)=WS(1,M)+Q(J,5,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(2,M)=WS(2,M)+Q(J,7,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(3,M)=WS(3,M)+Q(J,9,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(4,M)=WS(4,M)+Q(J,11,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
              END DO
           END DO
        END IF
     ELSE IF(NN.EQ.M+1) THEN
        IF(IPOW.EQ.0) THEN        
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))           
              DO J=1,JCT
                 WS(1,M+1)=WS(1,M+1)+Q(J,4,IR)*PM(J+JV*(JD-1),1)
                 WS(2,M+1)=WS(2,M+1)+Q(J,6,IR)*PM(J+JV*(JD-1),1)
                 WS(1,M)=WS(1,M)+Q(J,5,IR)*PM(J+JV*(JD-1),1)
                 WS(2,M)=WS(2,M)+Q(J,7,IR)*PM(J+JV*(JD-1),1)
                 WS(3,M+1)=WS(3,M+1)+Q(J,8,IR)*PM(J+JV*(JD-1),1)
                 WS(4,M+1)=WS(4,M+1)+Q(J,10,IR)*PM(J+JV*(JD-1),1)
                 WS(3,M)=WS(3,M)+Q(J,9,IR)*PM(J+JV*(JD-1),1)
                 WS(4,M)=WS(4,M)+Q(J,11,IR)*PM(J+JV*(JD-1),1)
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN        
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))           
              DO J=1,JCT
                 WS(1,M+1)=WS(1,M+1)+Q(J,4,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(2,M+1)=WS(2,M+1)+Q(J,6,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(1,M)=WS(1,M)+Q(J,5,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(2,M)=WS(2,M)+Q(J,7,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(3,M+1)=WS(3,M+1)+Q(J,8,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(4,M+1)=WS(4,M+1)+Q(J,10,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(3,M)=WS(3,M)+Q(J,9,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 WS(4,M)=WS(4,M)+Q(J,11,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN        
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))           
              DO J=1,JCT
                 WS(1,M+1)=WS(1,M+1)+Q(J,4,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(2,M+1)=WS(2,M+1)+Q(J,6,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(1,M)=WS(1,M)+Q(J,5,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(2,M)=WS(2,M)+Q(J,7,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(3,M+1)=WS(3,M+1)+Q(J,8,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(4,M+1)=WS(4,M+1)+Q(J,10,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(3,M)=WS(3,M)+Q(J,9,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 WS(4,M)=WS(4,M)+Q(J,11,IR)*PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
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
              END DO
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))
              DO J=1,JCT
                 Q(J,2,IR)=PM(J+JV*(JD-1),1)
                 Q(J,3,IR)=PM(J+JV*(JD-1),2)
              END DO
              DO J=MAX(1,JCT+1),JV
                 Q(J,2,IR)=0
                 Q(J,3,IR)=0
              END DO
           END DO
        ELSE IF(IPOW.EQ.1) THEN
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,1,IR)=P(J+JV*(JD-1),5)
              END DO
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))
              DO J=1,JCT
                 Q(J,2,IR)=PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                 Q(J,3,IR)=PM(J+JV*(JD-1),2)*P(J+JV*(JD-1),4)
              END DO
              DO J=MAX(1,JCT+1),JV
                 Q(J,2,IR)=0
                 Q(J,3,IR)=0
              END DO
           END DO
        ELSE IF(IPOW.EQ.2) THEN           
           DO IR=1,JR
              JD=IR+JR*(ID-1)
              DO J=1,JV
                 Q(J,1,IR)=P(J+JV*(JD-1),5)
              END DO
              JCT=MIN(JV,JC(IJ+1)-JV*(JD-1))
              DO J=1,JCT
                 Q(J,2,IR)=PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                 Q(J,3,IR)=PM(J+JV*(JD-1),2)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
              END DO
              DO J=MAX(1,JCT+1),JV
                 Q(J,2,IR)=0
                 Q(J,3,IR)=0
              END DO
           END DO
        END IF

        LD=1
        JC1=JC(IJ+LD)
        JB=MIN(JR,(JC1-1-JV*JR*(ID-1)+JV)/JV)

        DO N=M,NN-10,8
           L=(N-M)/2
           LD=L/4+1
           JC1=JC(IJ+LD)
           JB=MIN(JR,(JC1-1-JV*JR*(ID-1)+JV)/JV)
           IF(JB.GE.1) THEN
              IL=IC+2*L+3
              IF(JV.EQ.4) THEN              
                 CALL LXQVWS(JB,R,WS(1,N),Q,IL,0_8)
              ELSE IF(JV.EQ.8) THEN                 
                 CALL LXOVWS(JB,R,WS(1,N),Q,IL,0_8)
              ELSE IF(JV.EQ.256) THEN                 
                 CALL LXXVWS(JB,R,WS(1,N),Q,IL,0_8)
              ELSE
                 CALL LXLVWS(JV,JB,R,WS(1,N),Q,IL,0_8)
              END IF
           END IF
           JC1=JC(IJ+LD)
           JC2=JC(IJ+LD+1)
           IF(JC2.GT.JC1) THEN
              ID1=(JC1-1)/(JV*JR)+1
              ID2=(JC2-1)/(JV*JR)+1
              JB=MIN(JR,(JC2-1-JV*JR*(ID-1)+JV)/JV)
              IF(ID.GE.ID1.AND.ID.LE.ID2) THEN
                 JS=MAX(1,(JC1-1-(ID-1)*JV*JR)/JV+1)
                 JE=MIN(JR,(JC2-1-(ID-1)*JV*JR)/JV+1)
                 IF(IPOW.EQ.0) THEN                            
                    DO IR=JS,JE
                       JD=IR+JR*(ID-1)
                       JCT1=MAX(0,JC1-JV*(JD-1))
                       JCT2=MIN(JV,JC2-JV*(JD-1))
                       DO J=JCT1+1,JCT2
                          Q(J,2,IR)=PM(J+JV*(JD-1),1)
                          Q(J,3,IR)=PM(J+JV*(JD-1),2)
                       END DO
                    END DO
                 ELSE IF(IPOW.EQ.1) THEN
                    DO IR=JS,JE
                       JD=IR+JR*(ID-1)
                       JCT1=MAX(0,JC1-JV*(JD-1))
                       JCT2=MIN(JV,JC2-JV*(JD-1))
                       DO J=JCT1+1,JCT2
                          Q(J,2,IR)=PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)
                          Q(J,3,IR)=PM(J+JV*(JD-1),2)*P(J+JV*(JD-1),4)
                       END DO
                    END DO
                 ELSE IF(IPOW.EQ.2) THEN
                    DO IR=JS,JE
                       JD=IR+JR*(ID-1)
                       JCT1=MAX(0,JC1-JV*(JD-1))
                       JCT2=MIN(JV,JC2-JV*(JD-1))
                       DO J=JCT1+1,JCT2
                          Q(J,2,IR)=PM(J+JV*(JD-1),1)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                          Q(J,3,IR)=PM(J+JV*(JD-1),2)*P(J+JV*(JD-1),4)*P(J+JV*(JD-1),4)
                       END DO
                    END DO
                 END IF
              END IF
           END IF
        END DO

        IF(JB.GE.1) THEN
           L=(N-M)/2
           IL=IC+2*L+3
           IF(JV.EQ.4) THEN                         
              CALL LXQVWS(JB,R,WS(1,N),Q,IL,NN-N)
           ELSE IF(JV.EQ.8) THEN
              CALL LXOVWS(JB,R,WS(1,N),Q,IL,NN-N)              
           ELSE IF(JV.EQ.256) THEN
              CALL LXXVWS(JB,R,WS(1,N),Q,IL,NN-N)              
           ELSE
              CALL LXLVWS(JV,JB,R,WS(1,N),Q,IL,NN-N)              
           END IF
        END IF
     END IF
  END DO

  S1(1,M)=WS(1,M)
  S1(2,M)=WS(2,M)
  S2(1,M)=WS(3,M)
  S2(2,M)=WS(4,M)
  
  DO L=1,(NN+1-M)/2
     N=2*L-1+M
     S1(1,N)=R(IA+L)*WS(1,N)
     S1(2,N)=R(IA+L)*WS(2,N)
     S2(1,N)=R(IA+L)*WS(3,N)
     S2(2,N)=R(IA+L)*WS(4,N)
  END DO

  DO N=(NN-M)/2*2+M,M+2,-2
     S1(1,N)=R(IE+N-M)*WS(1,N)+R(IE+N-M-1)*WS(1,N-2) 
     S1(2,N)=R(IE+N-M)*WS(2,N)+R(IE+N-M-1)*WS(2,N-2) 
     S2(1,N)=R(IE+N-M)*WS(3,N)+R(IE+N-M-1)*WS(3,N-2) 
     S2(2,N)=R(IE+N-M)*WS(4,N)+R(IE+N-M-1)*WS(4,N-2) 
  END DO

END SUBROUTINE LXSVWS
