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
*     TRANSFORM SPECTRA TO GRID                               2002/04/23
************************************************************************
      SUBROUTINE P3S2GA(NM,MM,LM,KM,JM,IM,S,G,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-NM:NM,-MM:MM,-LM:LM)
      DIMENSION G(-MM:MM,0:KM-1,2,0:LM)
      DIMENSION W(0:KM-1,0:LM,0:JM-1,2)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      DO L=0,LM
        DO N=NM+1,KM-NM-1 
          DO M=-MM,MM                  
            G(M,N,1,L)=0
            G(M,N,2,L)=0
          END DO
        END DO
      END DO

      DO L=0,LM
        DO N=1,NM 
          DO M=-MM,MM                  
            G(M,   N,1,L)=S( N, M, L)
            G(M,   N,2,L)=S(-N,-M,-L)
            G(M,KM-N,1,L)=S(-N, M, L)
            G(M,KM-N,2,L)=S( N,-M,-L)
          END DO
        END DO
      END DO

* N=0
      DO L=1,LM
        DO M=-MM,MM
          G(M,0,1,L)=S(0, M, L)
          G(M,0,2,L)=S(0,-M,-L)
        END DO
      END DO

* L=0
      DO N=1,NM
        DO M=1,MM        
          G( M,   N,1,0)= S( N, M, 0)
          G( M,   N,2,0)= S(-N,-M, 0)
          G( M,KM-N,1,0)= S(-N, M, 0)
          G( M,KM-N,2,0)= S( N,-M, 0)
          G(-M,   N,1,0)= S(-N, M, 0)
          G(-M,   N,2,0)=-S( N,-M, 0)
          G(-M,KM-N,1,0)= S( N, M, 0)
          G(-M,KM-N,2,0)=-S(-N,-M, 0)
        END DO
      END DO

* L=M=0
      DO N=1,NM
        G(0,   N,1,0)= S( N, 0, 0)
        G(0,   N,2,0)= S(-N, 0, 0)
        G(0,KM-N,1,0)= S( N, 0, 0)
        G(0,KM-N,2,0)=-S(-N, 0, 0)
      END DO

* L=N=0
      DO M=1,MM        
        G( M,0,1,0)= S(0, M, 0)
        G( M,0,2,0)= S(0,-M, 0)
        G(-M,0,1,0)= S(0, M, 0)
        G(-M,0,2,0)=-S(0,-M, 0)
      END DO

* L=M=N=0

      G(0,0,1,0)=S(0,0,0)
      G(0,0,2,0)=0

      DO L=0,LM
        CALL FTTZUB(2*MM+1,KM,G(-MM,0,1,L),W,ITK,TK)
      END DO

      DO M=MM+1,JM-MM-1
        DO L=0,LM        
          DO K=0,KM-1
            W(K,L,M,1)=0
            W(K,L,M,2)=0
          END DO
        END DO
      END DO

      DO M=1,MM
        DO L=0,LM
          DO K=0,KM-1
            W(K,L,   M,1)=G( M,K,1,L)
            W(K,L,   M,2)=G( M,K,2,L)              
            W(K,L,JM-M,1)=G(-M,K,1,L)
            W(K,L,JM-M,2)=G(-M,K,2,L)                            
          END DO
        END DO
      END DO

* M=0
      DO L=0,LM        
        DO K=0,KM-1
          W(K,L,0,1)=G(0,K,1,L)
          W(K,L,0,2)=G(0,K,2,L)            
        END DO
      END DO

      CALL FTTZUB(KM*(LM+1),JM,W,G,ITJ,TJ)

      CALL P3S2GB(LM,KM,JM,IM,W,G,ITI,TI)

      END
************************************************************************
*     LOWER ROUTINE FOR P3S2GA                                2002/04/23
************************************************************************
      SUBROUTINE P3S2GB(LM,KM,JM,IM,W,G,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:KM-1,0:JM-1,2,0:IM/2-1)      
      DIMENSION W(0:KM-1,0:LM,0:JM-1,2)
      DIMENSION ITI(5),TI(IM*2)

      DO L=LM+1,IM/2-1
        DO J=0,JM-1
          DO K=0,KM-1
            G(K,J,1,L)=0
            G(K,J,2,L)=0
          END DO
        END DO
      END DO
      
      DO L=1,LM        
        DO J=0,JM-1
          DO K=0,KM-1
            G(K,J,1,L)=W(K,L,J,1)
            G(K,J,2,L)=W(K,L,J,2)              
          END DO
        END DO
      END DO
* L=0
      DO J=0,JM-1
        DO K=0,KM-1
          G(K,J,1,0)=W(K,0,J,1)
          G(K,J,2,0)=0
        END DO
      END DO

      CALL FTTRUB(KM*JM,IM,G,W,ITI,TI)

      END
