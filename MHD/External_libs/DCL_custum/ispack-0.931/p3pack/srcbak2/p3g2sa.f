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
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2002 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     TRANSFORM GRID TO SPECTRA                               2002/02/22
************************************************************************
      SUBROUTINE P3G2SA(NM,MM,LM,KM,JM,IM,G,S,W,ITK,TK,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(-NM:NM,-MM:MM,-LM:LM)
      DIMENSION G(-MM:MM,0:LM,0:KM-1,2)
      DIMENSION W(0:KM-1,0:LM,0:JM-1,2)
      DIMENSION ITK(5),TK(KM*2),ITJ(5),TJ(JM*2),ITI(5),TI(IM*2)

      CALL P3G2SB(LM,KM,JM,IM,G,W,ITI,TI)
      
      CALL FTTZUF(KM*(LM+1),JM,W,G,ITJ,TJ)

      DO IR=1,2
        DO L=0,LM        
          DO M=1,MM
            DO K=0,KM-1
              G( M,L,K,IR)=W(K,L,   M,IR)
              G(-M,L,K,IR)=W(K,L,JM-M,IR)
            END DO
          END DO
        END DO
      END DO

* M=0
      DO IR=1,2
        DO L=0,LM        
          DO K=0,KM-1
            G(0,L,K,IR)=W(K,L,0,IR)
          END DO
        END DO
      END DO
      
      CALL FTTZUF((2*MM+1)*(LM+1),KM,G,W,ITK,TK)

      DO N=1,NM
        DO L=1,LM
          DO M=-MM,MM                  
            S( N, M, L)=G(M,L,   N,1)
            S(-N,-M,-L)=G(M,L,   N,2)
            S(-N, M, L)=G(M,L,KM-N,1)
            S( N,-M,-L)=G(M,L,KM-N,2)
          END DO
        END DO
      END DO

* N=0
      DO L=1,LM
        DO M=-MM,MM
          S(0, M, L)=G(M,L,0,1)
          S(0,-M,-L)=G(M,L,0,2)
        END DO
      END DO

* L=0
      DO N=1,NM
        DO M=1,MM        
          S( N, M, 0)=G( M,0,   N,1)
          S(-N,-M, 0)=G( M,0,   N,2)
          S(-N, M, 0)=G( M,0,KM-N,1)
          S( N,-M, 0)=G( M,0,KM-N,2)
        END DO
      END DO

* L=M=0
      DO N=1,NM
        S( N, 0, 0)=G(0,0,N,1)
        S(-N, 0, 0)=G(0,0,N,2)
      END DO

* L=N=0
      DO M=1,MM        
        S(0, M, 0)=G(M,0,0,1)
        S(0,-M, 0)=G(M,0,0,2)
      END DO

* L=M=N=0
      S(0,0,0)=G(0,0,0,1)

      END
************************************************************************
*     LOWER ROUTINE FOR P3G2SA                                2002/02/22
************************************************************************
      SUBROUTINE P3G2SB(LM,KM,JM,IM,G,W,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:KM-1,0:JM-1,2,0:IM/2-1)      
      DIMENSION W(0:KM-1,0:LM,0:JM-1,2)
      DIMENSION ITI(5),TI(IM*2)

      CALL FTTRUF(KM*JM,IM,G,W,ITI,TI)      

      DO IR=1,2
        DO L=1,LM        
          DO J=0,JM-1
            DO K=0,KM-1
              W(K,L,J,IR)=G(K,J,IR,L)
            END DO
          END DO
        END DO
      END DO

* L=0
      DO J=0,JM-1
        DO K=0,KM-1
          W(K,0,J,1)=G(K,J,1,0)
          W(K,0,J,2)=0
        END DO
      END DO

      END
