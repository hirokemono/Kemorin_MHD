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
*     INITIALIZE E for SJABNL                                 2010/02/02
*-----------------------------------------------------------------------
      SUBROUTINE SJINIE(MM,E)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION E((5*MM*(MM+1)+4)/2)

      IND=0
      M=0
      N=M
      IND=IND+1
      E(IND)=-1D0*N*(N-1)*SQRT(
     &  (1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &  *(1D0*(N+2)*(N+2)-M*M)/(4D0*(N+2)*(N+2)-1))
      IND=IND+1
      E(IND)=1D0*N*(N+1)
     &  -1D0*N*(N-1)*(1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &  -1D0*(N+1)*(N+2)*(1D0*N*N-M*M)/(4D0*N*N-1)
     &  -2D0*M*M
      N=M+1
      IND=IND+1
      E(IND)=-1D0*N*(N-1)*SQRT(
     &  (1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &  *(1D0*(N+2)*(N+2)-M*M)/(4D0*(N+2)*(N+2)-1))
      IND=IND+1
      E(IND)=1D0*N*(N+1)
     &  -1D0*N*(N-1)*(1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &  -1D0*(N+1)*(N+2)*(1D0*N*N-M*M)/(4D0*N*N-1)
     &  -2D0*M*M
      DO N=M+2,MM
        IND=IND+1
        E(IND)=-1D0*N*(N-1)*SQRT(
     &    (1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &    *(1D0*(N+2)*(N+2)-M*M)/(4D0*(N+2)*(N+2)-1))
        IND=IND+1
        E(IND)=1D0*N*(N+1)
     &    -1D0*N*(N-1)*(1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &    -1D0*(N+1)*(N+2)*(1D0*N*N-M*M)/(4D0*N*N-1)
     &    -2D0*M*M
        IND=IND+1
        E(IND)=-1D0*(N+1)*(N+2)*SQRT(
     &    (1D0*N*N-M*M)/(4D0*N*N-1)
     &    *(1D0*(N-1)*(N-1)-M*M)/(4D0*(N-1)*(N-1)-1))
      END DO
      DO M=1,MM-1
        N=M
        IND=IND+1
        E(IND)=-1D0*N*(N-1)*SQRT(
     &    (1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &    *(1D0*(N+2)*(N+2)-M*M)/(4D0*(N+2)*(N+2)-1))
        IND=IND+1
        E(IND)=1D0*N*(N+1)
     &    -1D0*N*(N-1)*(1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &    -1D0*(N+1)*(N+2)*(1D0*N*N-M*M)/(4D0*N*N-1)
     &    -2D0*M*M
        IND=IND+1
        E(IND)=-1D0*M*(N-1)*
     &    SQRT((1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1))
        N=M+1
        IND=IND+1
        E(IND)=-1D0*N*(N-1)*SQRT(
     &    (1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &    *(1D0*(N+2)*(N+2)-M*M)/(4D0*(N+2)*(N+2)-1))
        IND=IND+1
        E(IND)=1D0*N*(N+1)
     &    -1D0*N*(N-1)*(1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &    -1D0*(N+1)*(N+2)*(1D0*N*N-M*M)/(4D0*N*N-1)
     &    -2D0*M*M
        IND=IND+1
        E(IND)=-1D0*M*(N-1)*
     &    SQRT((1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1))
        IND=IND+1
        E(IND)=1D0*M*(N+2)*SQRT((1D0*N*N-M*M)/(4D0*N*N-1))
        DO N=M+2,MM
          IND=IND+1
          E(IND)=-1D0*N*(N-1)*SQRT(
     &      (1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &      *(1D0*(N+2)*(N+2)-M*M)/(4D0*(N+2)*(N+2)-1))
          IND=IND+1
          E(IND)=1D0*N*(N+1)
     &      -1D0*N*(N-1)*(1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &      -1D0*(N+1)*(N+2)*(1D0*N*N-M*M)/(4D0*N*N-1)
     &      -2D0*M*M
          IND=IND+1
          E(IND)=-1D0*(N+1)*(N+2)*SQRT(
     &      (1D0*N*N-M*M)/(4D0*N*N-1)
     &      *(1D0*(N-1)*(N-1)-M*M)/(4D0*(N-1)*(N-1)-1))
          IND=IND+1
          E(IND)=-1D0*M*(N-1)*
     &      SQRT((1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1))
          IND=IND+1
          E(IND)=1D0*M*(N+2)*SQRT((1D0*N*N-M*M)/(4D0*N*N-1))
        END DO
      END DO
      M=MM
      N=M
      IND=IND+1
      E(IND)=-1D0*N*(N-1)*SQRT(
     &  (1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &  *(1D0*(N+2)*(N+2)-M*M)/(4D0*(N+2)*(N+2)-1))
      IND=IND+1
      E(IND)=1D0*N*(N+1)
     &  -1D0*N*(N-1)*(1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1)
     &  -1D0*(N+1)*(N+2)*(1D0*N*N-M*M)/(4D0*N*N-1)
     &  -2D0*M*M
      IND=IND+1
      E(IND)=-1D0*M*(N-1)*
     &  SQRT((1D0*(N+1)*(N+1)-M*M)/(4D0*(N+1)*(N+1)-1))

      END
