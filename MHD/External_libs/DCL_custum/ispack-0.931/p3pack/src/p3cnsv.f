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
*     CALCULATE CONSERVED QUANTITIES FOR 3D EULER EQ.         2002/03/12
************************************************************************
      SUBROUTINE P3CNSV(NM,MM,LM,Z,E,H)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-NM:NM,-MM:MM,-LM:LM,2)

* ENERGY --> E

      E=0

      DO L=-LM,-1
        DO M=-MM,MM
          DO N=-NM,NM
            E=E+((L*L+M*M)*Z(N,M,L,1)*Z(N,M,L,1)
     &        +(L*L+N*N)*Z(N,M,L,2)*Z(N,M,L,2)
     &        +2*M*N*Z(N,M,L,1)*Z(N,M,L,2))/(1D0*(L*L+M*M+N*N)*L*L)
          END DO
        END DO
      END DO
      DO L=1,LM
        DO M=-MM,MM
          DO N=-NM,NM
            E=E+((L*L+M*M)*Z(N,M,L,1)*Z(N,M,L,1)
     &        +(L*L+N*N)*Z(N,M,L,2)*Z(N,M,L,2)
     &        +2*M*N*Z(N,M,L,1)*Z(N,M,L,2))/(1D0*(L*L+M*M+N*N)*L*L)
          END DO
        END DO
      END DO

      L=0
      DO M=-MM,-1
        DO N=-NM,NM
          E=E+(M*M*Z(N,M,L,2)*Z(N,M,L,2)
     &      +(M*M+N*N)*Z(N,M,L,1)*Z(N,M,L,1))
     &      /(1D0*M*M*(M*M+N*N))
        END DO
      END DO
      DO M=1,MM
        DO N=-NM,NM
          E=E+(M*M*Z(N,M,L,2)*Z(N,M,L,2)
     &      +(M*M+N*N)*Z(N,M,L,1)*Z(N,M,L,1))
     &      /(1D0*M*M*(M*M+N*N))
        END DO
      END DO
      
      L=0
      M=0
      DO N=-NM,-1
        E=E+(Z(N,M,L,1)*Z(N,M,L,1)+Z(N,M,L,2)*Z(N,M,L,2))/(N*N)
      END DO
      DO N=1,NM
        E=E+(Z(N,M,L,1)*Z(N,M,L,1)+Z(N,M,L,2)*Z(N,M,L,2))/(N*N)        
      END DO

* HELICITY --> H

      H=0

      DO L=-LM,-1
        DO M=-MM,MM
          DO N=-NM,NM
            H=H+(4D0/L)*(Z(N,M,L,1)*Z(-N,-M,-L,2))
          END DO
        END DO
      END DO
      DO L=1,LM
        DO M=-MM,MM
          DO N=-NM,NM
            H=H+(4D0/L)*(Z(N,M,L,1)*Z(-N,-M,-L,2))
          END DO
        END DO
      END DO

      L=0
      DO M=-MM,-1
        DO N=-NM,NM
          H=H+(4D0/M)*(Z(N,M,L,1)*Z(-N,-M,-L,2))
        END DO
      END DO
      DO M=1,MM
        DO N=-NM,NM
          H=H+(4D0/M)*(Z(N,M,L,1)*Z(-N,-M,-L,2))
        END DO
      END DO
      
      L=0
      M=0
      DO N=-NM,-1
        H=H+(4D0/N)*(Z(N,M,L,1)*Z(-N,-M,-L,2))
      END DO
      DO N=1,NM
        H=H+(4D0/N)*(Z(N,M,L,1)*Z(-N,-M,-L,2))        
      END DO

      END
