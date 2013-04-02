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
************************************************************************
*     SPECTRAL TRANSFORM                                        98/02/17
************************************************************************
*     TRANSFORM SPECTRA TO GRID
************************************************************************
      SUBROUTINE SMTS2G(MM,IM,ID,JM,JD,KM,S,G,W,IT,T,IP,P,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(KM*(MM+1),0:MM)
      DIMENSION G(KM*ID,JD)
      DIMENSION W(KM*(MM+1),0:MM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)

      JH=JM/2

      CALL SMSSSB(MM,KM,S,W)

      CALL BSSET0(KM*ID*JD,G)

      DO N=0,MM-1,2
        IA=KM*N
        IB=IA+KM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            G(IA+I,J)=G(IA+I,J)+W(I,N)*P(I,N,J)
            G(IB+I,JH+J)=G(IB+I,JH+J)+W(I,N+1)*P(I,N+1,J)
          END DO
        END DO
      END DO

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            G(IA+I,J)=G(IA+I,J)+W(I,MM)*P(I,MM,J)
          END DO
        END DO
      END IF

      CALL SMPGWB(MM,ID,JM,JD,KM,G,W,IP,Q)
      CALL SMFRUB(KM*JD,IM,MM,W,G,IT,T)
      CALL SMRGGB(IM,ID,JD,KM,W,G)

      END
************************************************************************
*     TRANSFORM GRID TO SPECTRA
************************************************************************
      SUBROUTINE SMTG2S(MM,IM,ID,JM,JD,KM,G,S,W,IT,T,IP,P,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(KM*ID,JD)
      DIMENSION S(KM*(MM+1),0:MM)
      DIMENSION W(JD*KM*ID,2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)

      CALL SMLG2S(MM,IM,ID,JM,JD,KM,G,S,W,W(1,2),IT,T,IP,P,Q)

      END
************************************************************************
      SUBROUTINE SMLG2S(MM,IM,ID,JM,JD,KM,G,S,W,WG,IT,T,IP,P,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(KM*ID,JD)
      DIMENSION S(KM*(MM+1),0:MM)
      DIMENSION W(KM*(MM+1),0:MM)
      DIMENSION WG(KM*ID,JD)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)

      JH=JM/2

      CALL SMRGGF(IM,ID,JD,KM,G,W)
      CALL SMFRUF(KM*JD,IM,MM,W,WG,IT,T)
      CALL SMPWGF(MM,ID,JM,JD,KM,W,WG,IP,Q)

      CALL BSSET0(KM*(MM+1)*(MM+1),W)

      DO N=0,MM-1,2
        IA=KM*N
        IB=IA+KM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            W(I,N)=W(I,N)+WG(IA+I,J)*P(I,N,J)
            W(I,N+1)=W(I,N+1)+WG(IB+I,JH+J)*P(I,N+1,J)
          END DO
        END DO
      END DO

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            W(I,MM)=W(I,MM)+WG(IA+I,J)*P(I,MM,J)
          END DO
        END DO
      END IF

      CALL SMSSSF(MM,KM,W,S)

      END
************************************************************************
*     TRANSFORM (chi,psi) TO (u,v)
************************************************************************
      SUBROUTINE SMTS2V(MM,IM,ID,JM,JD,KM,A,B,U,V,W,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM),B(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*ID*JD,2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      CALL SMLS2V(MM,IM,ID,JM,JD,KM,A,B,U,V,W,W(1,2),IT,T,IP,P,Q,R,ML)

      END
************************************************************************
      SUBROUTINE SMLS2V(MM,IM,ID,JM,JD,KM,A,B,U,V,W,WS,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM),B(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*(MM+1),0:MM,2)
      DIMENSION WS(KM*(MM+1),0:MM,2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      JH=JM/2

      CALL SMSSSB(MM,KM,A,WS(1,0,1))
      CALL SMSSSB(MM,KM,B,WS(1,0,2))
      CALL SMDX2A(MM,KM,WS(1,0,1),WS(1,0,2),W(1,0,1),W(1,0,2),ML)
      CALL SMDY2B(MM,KM,WS(1,0,1),WS(1,0,2),W(1,0,1),W(1,0,2),R)

      CALL BSSET0(KM*ID*JD,U)
      CALL BSSET0(KM*ID*JD,V)

      DO N=0,MM-1,2
        IA=KM*N
        IB=IA+KM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            U(IA+I,J)=U(IA+I,J)+W(I,N,1)*P(I,N,J)
            V(IA+I,J)=V(IA+I,J)+W(I,N,2)*P(I,N,J)
            U(IB+I,JH+J)=U(IB+I,JH+J)+W(I,N+1,1)*P(I,N+1,J)
            V(IB+I,JH+J)=V(IB+I,JH+J)+W(I,N+1,2)*P(I,N+1,J)
          END DO
        END DO
      END DO

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            U(IA+I,J)=U(IA+I,J)+W(I,MM,1)*P(I,MM,J)
            V(IA+I,J)=V(IA+I,J)+W(I,MM,2)*P(I,MM,J)
            U(IA+I,JH+J)=U(IA+I,JH+J)-WS(I,MM,2)*P(I,MM+1,J)
            V(IA+I,JH+J)=V(IA+I,JH+J)+WS(I,MM,1)*P(I,MM+1,J)
          END DO
        END DO
      ELSE
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            U(IA+I,J)=U(IA+I,J)-WS(I,MM,2)*P(I,MM+1,J)
            V(IA+I,J)=V(IA+I,J)+WS(I,MM,1)*P(I,MM+1,J)
          END DO
        END DO
      END IF

      DO J=1,JH
        DO I=1,KM*(MM+1)
          U(I,JH+J)=U(I,JH+J)-WS(I,0,2)*P(I,MM+2,J)
          V(I,JH+J)=V(I,JH+J)+WS(I,0,1)*P(I,MM+2,J)
        END DO
      END DO

      CALL SMPGWB(MM,ID,JM,JD,KM,U,W,IP,Q)
      CALL SMFRUB(KM*JD,IM,MM,W,U,IT,T)
      CALL SMRGGB(IM,ID,JD,KM,W,U)

      CALL SMPGWB(MM,ID,JM,JD,KM,V,W,IP,Q)
      CALL SMFRUB(KM*JD,IM,MM,W,V,IT,T)
      CALL SMRGGB(IM,ID,JD,KM,W,V)

      END
************************************************************************
*     TRANSFORM (u,v) TO (D,Z)
************************************************************************
      SUBROUTINE SMTV2S(MM,IM,ID,JM,JD,KM,U,V,A,B,W,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM),B(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*ID*JD,3)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      CALL SMLV2S(MM,IM,ID,JM,JD,KM,U,V,A,B,W,W(1,2),IT,T,IP,P,Q,R,ML)

      END
************************************************************************
      SUBROUTINE SMLV2S(MM,IM,ID,JM,JD,KM,U,V,A,B,W,WG,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM),B(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*(MM+1),0:MM,2)
      DIMENSION WG(KM*ID,JD,2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      JH=JM/2

      CALL SMRGGF(IM,ID,JD,KM,U,W)
      CALL SMFRUF(KM*JD,IM,MM,W,WG(1,1,1),IT,T)
      CALL SMPWGF(MM,ID,JM,JD,KM,W,WG(1,1,1),IP,Q)

      CALL SMRGGF(IM,ID,JD,KM,V,W)
      CALL SMFRUF(KM*JD,IM,MM,W,WG(1,1,2),IT,T)
      CALL SMPWGF(MM,ID,JM,JD,KM,W,WG(1,1,2),IP,Q)

      CALL BSSET0(KM*(MM+1)*(MM+1),A)
      CALL BSSET0(KM*(MM+1)*(MM+1),B)

      DO N=0,MM-1,2
        IA=KM*N
        IB=IA+KM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            A(I,N)=A(I,N)+WG(IA+I,J,1)*P(I,N,J)
            B(I,N)=B(I,N)+WG(IA+I,J,2)*P(I,N,J)
          END DO
        END DO
        DO J=1,JH
          DO I=1,KM*(MM+1)
            A(I,N+1)=A(I,N+1)+WG(IB+I,JH+J,1)*P(I,N+1,J)
            B(I,N+1)=B(I,N+1)+WG(IB+I,JH+J,2)*P(I,N+1,J)
          END DO
        END DO
      END DO

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            A(I,MM)=A(I,MM)+WG(IA+I,J,1)*P(I,MM,J)
            B(I,MM)=B(I,MM)+WG(IA+I,J,2)*P(I,MM,J)
          END DO
        END DO
      END IF

      CALL SMDX2A(MM,KM,A,B,W(1,0,1),W(1,0,2),ML)

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            W(I,MM,2)=W(I,MM,2)+WG(IA+I,JH+J,1)*P(I,MM+1,J)
            W(I,MM,1)=W(I,MM,1)-WG(IA+I,JH+J,2)*P(I,MM+1,J)
          END DO
        END DO
      ELSE
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            W(I,MM,2)=W(I,MM,2)+WG(IA+I,J,1)*P(I,MM+1,J)
            W(I,MM,1)=W(I,MM,1)-WG(IA+I,J,2)*P(I,MM+1,J)
          END DO
        END DO
      END IF

      DO J=1,JH
        DO I=1,KM*(MM+1)
          W(I,0,2)=W(I,0,2)+WG(I,JH+J,1)*P(I,MM+2,J)
          W(I,0,1)=W(I,0,1)-WG(I,JH+J,2)*P(I,MM+2,J)
        END DO
      END DO

      CALL SMDY2F(MM,KM,A,B,W(1,0,1),W(1,0,2),R)
      CALL SMSSSF(MM,KM,W(1,0,1),A)
      CALL SMSSSF(MM,KM,W(1,0,2),B)

      END
************************************************************************
*     TRANSFORM (chi) TO (u,v)
************************************************************************
      SUBROUTINE SMTS1V(MM,IM,ID,JM,JD,KM,A,U,V,W,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*ID*JD,2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      CALL SMLS1V(MM,IM,ID,JM,JD,KM,A,U,V,W,W(1,2),IT,T,IP,P,Q,R,ML)

      END
************************************************************************
      SUBROUTINE SMLS1V(MM,IM,ID,JM,JD,KM,A,U,V,W,WS,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*(MM+1),0:MM,2)
      DIMENSION WS(KM*(MM+1),0:MM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      JH=JM/2

      CALL SMSSSB(MM,KM,A,WS)
      CALL SMDX1A(MM,KM,WS,W(1,0,1),ML)
      CALL BSSET0(KM*(MM+1)*(MM+1),W(1,0,2))
      CALL SMDY1B(MM,KM,WS,W(1,0,2),R)

      CALL BSSET0(KM*ID*JD,U)
      CALL BSSET0(KM*ID*JD,V)

      DO N=0,MM-1,2
        IA=KM*N
        IB=IA+KM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            U(IA+I,J)=U(IA+I,J)+W(I,N,1)*P(I,N,J)
            V(IA+I,J)=V(IA+I,J)+W(I,N,2)*P(I,N,J)
            U(IB+I,JH+J)=U(IB+I,JH+J)+W(I,N+1,1)*P(I,N+1,J)
            V(IB+I,JH+J)=V(IB+I,JH+J)+W(I,N+1,2)*P(I,N+1,J)
          END DO
        END DO
      END DO

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            U(IA+I,J)=U(IA+I,J)+W(I,MM,1)*P(I,MM,J)
            V(IA+I,J)=V(IA+I,J)+W(I,MM,2)*P(I,MM,J)
            V(IA+I,JH+J)=V(IA+I,JH+J)+WS(I,MM)*P(I,MM+1,J)
          END DO
        END DO
      ELSE
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            V(IA+I,J)=V(IA+I,J)+WS(I,MM)*P(I,MM+1,J)
          END DO
        END DO
      END IF

      DO J=1,JH
        DO I=1,KM*(MM+1)
          V(I,JH+J)=V(I,JH+J)+WS(I,0)*P(I,MM+2,J)
        END DO
      END DO

      CALL SMPGWB(MM,ID,JM,JD,KM,U,W,IP,Q)
      CALL SMFRUB(KM*JD,IM,MM,W,U,IT,T)
      CALL SMRGGB(IM,ID,JD,KM,W,U)

      CALL SMPGWB(MM,ID,JM,JD,KM,V,W,IP,Q)
      CALL SMFRUB(KM*JD,IM,MM,W,V,IT,T)
      CALL SMRGGB(IM,ID,JD,KM,W,V)

      END
************************************************************************
*     TRANSFORM (u,v) TO (D)
************************************************************************
      SUBROUTINE SMTV1S(MM,IM,ID,JM,JD,KM,U,V,A,W,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*ID*JD,3)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      CALL SMLV1S(MM,IM,ID,JM,JD,KM,U,V,A,W,W(1,2),IT,T,IP,P,Q,R,ML)

      END
************************************************************************
      SUBROUTINE SMLV1S(MM,IM,ID,JM,JD,KM,U,V,A,W,WG,IT,T,IP,P,Q,R,ML)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(KM*(MM+1),0:MM)
      DIMENSION U(KM*ID,JD),V(KM*ID,JD)
      DIMENSION W(KM*(MM+1),0:MM,2)
      DIMENSION WG(KM*ID,JD,2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION IP(KM*(2*MM+1))
      DIMENSION P(KM*(MM+1),0:MM+2,JM/2)
      DIMENSION Q(JM/2)
      DIMENSION R(KM*MM*(MM-1)*2)
      DIMENSION ML((MM+1)*(MM+1))

      JH=JM/2

      CALL SMRGGF(IM,ID,JD,KM,U,W)
      CALL SMFRUF(KM*JD,IM,MM,W,WG(1,1,1),IT,T)
      CALL SMPWGF(MM,ID,JM,JD,KM,W,WG(1,1,1),IP,Q)

      CALL SMRGGF(IM,ID,JD,KM,V,W)
      CALL SMFRUF(KM*JD,IM,MM,W,WG(1,1,2),IT,T)
      CALL SMPWGF(MM,ID,JM,JD,KM,W,WG(1,1,2),IP,Q)

      CALL BSSET0(KM*(MM+1)*(MM+1),W(1,0,2))
      CALL BSSET0(KM*(MM+1)*(MM+1),A)

      DO N=0,MM-1,2
        IA=KM*N
        IB=IA+KM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            A(I,N)=A(I,N)+WG(IA+I,J,1)*P(I,N,J)
            W(I,N,2)=W(I,N,2)+WG(IA+I,J,2)*P(I,N,J)
          END DO
        END DO
        DO J=1,JH
          DO I=1,KM*(MM+1)
            A(I,N+1)=A(I,N+1)+WG(IB+I,JH+J,1)*P(I,N+1,J)
            W(I,N+1,2)=W(I,N+1,2)+WG(IB+I,JH+J,2)*P(I,N+1,J)
          END DO
        END DO
      END DO

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            A(I,MM)=A(I,MM)+WG(IA+I,J,1)*P(I,MM,J)
            W(I,MM,2)=W(I,MM,2)+WG(IA+I,J,2)*P(I,MM,J)
          END DO
        END DO
      END IF

      CALL SMDX1A(MM,KM,A,W(1,0,1),ML)

      IF(MOD(MM,2).EQ.0) THEN
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            W(I,MM,1)=W(I,MM,1)-WG(IA+I,JH+J,2)*P(I,MM+1,J)
          END DO
        END DO
      ELSE
        IA=KM*MM
        DO J=1,JH
          DO I=1,KM*(MM+1)
            W(I,MM,1)=W(I,MM,1)-WG(IA+I,J,2)*P(I,MM+1,J)
          END DO
        END DO
      END IF

      DO J=1,JH
        DO I=1,KM*(MM+1)
          W(I,0,1)=W(I,0,1)-WG(I,JH+J,2)*P(I,MM+2,J)
        END DO
      END DO

      CALL SMDY1F(MM,KM,W(1,0,2),W(1,0,1),R)
      CALL SMSSSF(MM,KM,W(1,0,1),A)

      END
