************************************************************************
* ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
* Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
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
*************************************************************************
*     SPECTRAL TRANSFORM USING OPENMP                          2005/07/01
************************************************************************      
*     TRANSFORM SPECTRA TO GRID
*-----------------------------------------------------------------------
*     WS, WW はあちこちで作業領域として使用されるため,
*
*        KM*(IM+MM+1)*3*JM/2
*
*     以上の領域を確保しておくこと. WV は
*
*        KM*(MM+4)*(MM+3)*NP
*      
*     以上の領域を確保しておくこと. ここに, NP は利用しうる thread 数.
************************************************************************      
      SUBROUTINE SNTSOG(MM,IM,ID,JM,KM,
     &  S,G,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,WV,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION G(ID*JM*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION WS(*),WW(*),WV(*)
!$    INTEGER omp_get_num_threads,omp_get_thread_num

      JH=JM/2
      IW=IM+MM+1

      CALL SNCSOG(MM,KM,S,WV,IA,A,WW,IFLAG)              

      NP=1
      I=0
!$omp parallel private(i,np,jp,jd,js,je,jc,is)
!$    NP=omp_get_num_threads()
!$    I=omp_get_thread_num()
      JP=(JH-1)/NP+1
      JD=JP*2+1
      JS=JP*I+1
      JE=MIN(JP*(I+1),JH)
      JC=(JE-JS+1)*2
      IS=IW*KM*JD*I+1
      IF(JE.GE.JS) THEN
        CALL SNLS2G(MM,JC,KM,WV,WW(IS),Y(JS,1),P(1,JS),R,Q(1,JS))
        CALL SNPSOG(MM,JC,JD,KM,WW(IS),WS(IS),IP,Y(JS,4),IPOW)
        CALL SNFS2G(MM,IM,JD,KM,WS(IS),WW(IS),IT,T)
        CALL SNGSOG(IM,ID,JM,JD,JS,JE,KM,WW(IS),G)
      END IF
!$omp end parallel      

      END
************************************************************************
      SUBROUTINE SNGSOG(IM,ID,JM,JD,JS,JE,KM,WW,G)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WW(JD,KM,IM/2,2)
      DIMENSION G(ID,JM,KM)

      JH=JM/2
      JCH=JE-JS+1

      DO K=1,KM
        DO I=1,IM/2
          DO J=JS,JE
            G(2*I-1,JH+J,K)=WW(JCH+J-JS+1,K,I,1)
            G(2*I,JH+J,K)=WW(JCH+J-JS+1,K,I,2)
            G(2*I-1,JH-J+1,K)=WW(JCH-(J-JS),K,I,1)
            G(2*I,JH-J+1,K)=WW(JCH-(J-JS),K,I,2)
          END DO
        END DO
        DO I=IM+1,ID
          DO J=JS,JE
            G(I,JH+J,K)=WW(JCH+J-JS+1,K,1,1)
            G(I,JH-J+1,K)=WW(JCH-(J-JS),K,1,1)            
          END DO
        END DO
      END DO
      
      END
************************************************************************
      SUBROUTINE SNPSOG(MM,JM,JD,KM,W,S,IP,Y4,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y4(JM/2)

      JH=JM/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(JH+J,K)=        (W(K,J,1)+W(K,J,2))*Y4(J)**IPOW
          S(JH-J+1,K)=IP(K)*(W(K,J,1)-W(K,J,2))*Y4(J)**IPOW
        END DO
      END DO

      DO J=JM+1,JD
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(J,K)=S(JM,K)
          S(J,K)=S(JM,K)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNCSOG(MM,KM,S,WS,IA,A,WW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(KM,((MM+1)/2*2+3)*(MM/2+2)*2)

      LM=(MM+1)*(MM+1)
      CALL SNSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WS(K,IA(L,1))=A(L,1)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WS(K,IA(L,2))=A(L,2)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        CALL SNSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WW(K,IA(L,3))=A(L,3)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            WS(K,IA(L,4))=A(L,4)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        CALL SNOADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      ELSE IF(IFLAG.EQ.2) THEN
        CALL SNSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WW(K,IA(L,3))=A(L,5)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WS(K,IA(L,4))=A(L,6)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        CALL SNOADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      END IF

      END
************************************************************************
      SUBROUTINE SNOADD(N,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N)

!$omp parallel do
      DO I=1,N
        A(I)=A(I)+B(I)
      END DO
!$omp end parallel do
      
      END
************************************************************************
      SUBROUTINE SNSET0(N,A)
 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N)

!$omp parallel do      
      DO I=1,N
        A(I)=0
      END DO
!$omp end parallel do      
 
      END
************************************************************************
*     TRANSFORM GRID TO SPECTRA
*-----------------------------------------------------------------------
*     WS, WW はあちこちで作業領域として使用されるため,
*
*        KM*(IM+MM+1)*3*JM/2
*
*     以上の領域を確保しておくこと. WV は
*
*        KM*(MM+4)*(MM+3)*NP
*      
*     以上の領域を確保しておくこと. ここに, NP は利用しうる thread 数.
************************************************************************
      SUBROUTINE SNTGOS(MM,IM,ID,JM,KM,
     &  G,S,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,WV,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID*JM*KM)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(KM*((MM+1)/2+MM+1))
      DIMENSION P(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION WS(*),WW(*),WV(*)
!$    INTEGER omp_get_num_threads,omp_get_thread_num
      
      JH=JM/2
      LH=KM*(MM+4)*(MM+3)
      IW=IM+MM+1

      NP=1
      I=0
!$omp parallel private(i,np,jp,jd,js,je,jc,is,ib,ls,le,ips,l,lp)
!$    NP=omp_get_num_threads()
!$    I=omp_get_thread_num()
      JP=(JH-1)/NP+1
      JD=JP*2+1
      JS=JP*I+1
      JE=MIN(JP*(I+1),JH)
      JC=(JE-JS+1)*2
      IS=IW*KM*JD*I+1
      IB=LH*I+1
      IF(JE.GE.JS) THEN
        CALL SNGGOS(IM,ID,JM,JD,JS,JE,KM,G,WW(IS))
        CALL SNFG2S(MM,IM,JD,KM,WW(IS),WS(IS),IT,T)
        CALL SNPGOS(MM,JC,JD,KM,WS(IS),WW(IS),IP,Y(JS,2),Y(JS,4),IPOW)
        CALL SNLG2S(MM,JC,KM,WW(IS),WV(IB),Y(JS,1),P(1,JS),R,Q(1,JS))
      END IF
!$omp barrier
      LP=(LH-1)/NP+1
      LS=LP*I+1
      LE=MIN(LP*(I+1),LH)
      IF(LE.GE.LS) THEN
        DO IPS=1,NP-1
          JS=JP*IPS+1
          JE=MIN(JP*(IPS+1),JH)
          IB=LH*IPS+1        
          IF(JE.GE.JS) THEN
            DO L=LS,LE
              WV(L)=WV(L)+WV(IB+L-1)
            END DO
          END IF
        END DO
      END IF
!$omp end parallel

      CALL SNCGOS(MM,KM,WV,S,IA,A,IFLAG)

      END
************************************************************************
      SUBROUTINE SNGGOS(IM,ID,JM,JD,JS,JE,KM,G,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID,JM,KM)
      DIMENSION WW(JD,KM,IM/2,2)

      JH=JM/2
      JCH=JE-JS+1

      DO K=1,KM
        DO I=1,IM/2
          DO J=JS,JE
            WW(JCH+J-JS+1,K,I,1)=G(2*I-1,JH+J,K)
            WW(JCH+J-JS+1,K,I,2)=G(2*I,JH+J,K)
            WW(JCH-(J-JS),K,I,1)=G(2*I-1,JH-J+1,K)
            WW(JCH-(J-JS),K,I,2)=G(2*I,JH-J+1,K)
          END DO
        END DO
      END DO

      END
************************************************************************      
      SUBROUTINE SNPGOS(MM,JM,JD,KM,S,W,IP,Y2,Y4,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y2(JM/2),Y4(JM/2)

      JH=JM/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          W(K,J,1)=(S(JH+J,K)+IP(K)*S(JH-J+1,K))*(Y2(J)*Y4(J)**IPOW)
          W(K,J,2)=(S(JH+J,K)-IP(K)*S(JH-J+1,K))*(Y2(J)*Y4(J)**IPOW)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNCGOS(MM,KM,WS,S,IA,A,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=A(L,1)*WS(K,IA(L,1))
          END DO
!$omp end parallel do
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=-A(L,2)*WS(K,IA(L,2))
          END DO
!$omp end parallel do                
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=-A(L,3)*WS(K,IA(L,3))-A(L,4)*WS(K,IA(L,4))
          END DO
!$omp end parallel do                
        END DO
      ELSE IF(IFLAG.EQ.2) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=A(L,5)*WS(K,IA(L,3))+A(L,6)*WS(K,IA(L,4))
          END DO
!$omp end parallel do                
        END DO
      END IF

      END
