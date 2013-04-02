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
      SUBROUTINE DKJACB(JM,MM,K,L,X,P)
*-----------------------------------------------------------------------
*     w=x^K(1-x)^L �򥦥����ȤȤ���[0,1]��֤�����ľ��¿�༰
*     (�䥳��¿�༰)������. ������, P(J,1,N)�ϥ䥳��¿�༰�� ��w ��
*     ��������Τ�, P(J,2,N) �� dP_n/dx �Ǥ���.
*-----------------------------------------------------------------------      
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(JM),P(JM,2,0:MM)

      D=1+K+L
      DO I=1,K
        D=D*(L+I)/I
      END DO
      D=SQRT(D)

      E=(K*K-L*L)/8D0

      DO J=1,JM                
        P(J,1,0)=D*X(J)**(K/2D0)*(1-X(J))**(L/2D0)
        P(J,2,0)=(K-(K+L)*X(J))/2*P(J,1,0)
      END DO
      DO N=1,MM
        A=2*N+K+L
        B=A*SQRT((A+1)/((A-1)*N*(N+K)*(N+L)*(N+K+L)))
        C=A*SQRT((A-1)/((A+1)*N*(N+K)*(N+L)*(N+K+L)))
        A=A/2
        DO J=1,JM
          F=A*(X(J)-0.5D0)-E/A
          P(J,1,N)=B*(F*P(J,1,N-1)-P(J,2,N-1))
          P(J,2,N)=P(J,1,N-1)/C-F*P(J,1,N)
        END DO
      END DO

      DO N=0,MM
        DO J=1,JM
          P(J,2,N)=P(J,2,N)/(X(J)*(1-X(J)))
        END DO
      END DO

      END
