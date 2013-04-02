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
*     FEPACK: FILE EXCHANGE     (VERSION 0.0)      95/05/25 BY K.ISHIOKA
************************************************************************
*     PUT CHARACTER
*-----------------------------------------------------------------------
      SUBROUTINE FEPUTC(IU,ND,CD)
#ifdef ASCII
      CHARACTER CD*(*)

      CALL FHUPUT(IU,ND,CD)
#elif EBCDIC
      CHARACTER CD*(*),CW*1

      DO 10 I=1,ND
        CALL FLCECA(CD(I:I),CW)
        CALL FHUPUT(IU,1,CW)
   10 CONTINUE
#endif
      END
************************************************************************
*     GET CHARACTER
*-----------------------------------------------------------------------
      SUBROUTINE FEGETC(IU,ND,CD)
#ifdef ASCII
      CHARACTER CD*(*)

      CALL FHUGET(IU,ND,CD)
#elif EBCDIC
      CHARACTER CD*(*),CW*1

      DO 10 I=1,ND
        CALL FHUGET(IU,1,CW)
        CALL FLCACE(CW,CD(I:I))
   10 CONTINUE
#endif
      END
************************************************************************
*     PUT INTEGER
*-----------------------------------------------------------------------
      SUBROUTINE FEPUTI(IU,ND,ID)

      INTEGER ID(ND)
#ifdef LEN
      DO I=1,ND
        CALL FLCEN4(ID(I),IW)
        CALL FHUPUT(IU,4,IW)
      END DO
#else
      CALL FHUPUT(IU,ND*4,ID)
#endif
      END
************************************************************************
*     GET INTEGER
*-----------------------------------------------------------------------
      SUBROUTINE FEGETI(IU,ND,ID)

      INTEGER ID(ND)
#ifdef LEN
      DO I=1,ND
        CALL FHUGET(IU,4,IW)
        CALL FLCEN4(IW,ID(I))
      END DO
#else
      CALL FHUGET(IU,ND*4,ID)
#endif

      END
************************************************************************
*     PUT REAL
*-----------------------------------------------------------------------
      SUBROUTINE FEPUTR(IU,ND,RD)

      REAL RD(ND)
#ifdef LEN
      DO 10 I=1,ND
        CALL FLCEN4(RD(I),IW)
        CALL FHUPUT(IU,4,IW)
   10 CONTINUE
#elif IEEE
      CALL FHUPUT(IU,ND*4,RD)
#elif IBM
      DO 10 I=1,ND
        CALL FLRBRE(RD(I),IW)
        CALL FHUPUT(IU,4,IW)
   10 CONTINUE
#endif
      END
************************************************************************
*     GET REAL
*-----------------------------------------------------------------------
      SUBROUTINE FEGETR(IU,ND,RD)

      REAL RD(ND)
#ifdef LEN
      DO I=1,ND
        CALL FHUGET(IU,4,IW)
        CALL FLCEN4(IW,RD(I))
      END DO
#elif IEEE
      CALL FHUGET(IU,ND*4,RD)
#elif IBM
      DO 10 I=1,ND
        CALL FHUGET(IU,4,IW)
        CALL FLRERB(IW,RD(I))
   10 CONTINUE
#endif
      END
************************************************************************
*     PUT DOUBLE PRECISION
*-----------------------------------------------------------------------
      SUBROUTINE FEPUTD(IU,ND,DD)
#ifdef LEN
      REAL*8 DD(ND)
      INTEGER IW(2)

      DO I=1,ND
        CALL FLCEN8(DD(I),IW)
        CALL FHUPUT(IU,8,IW)
      END DO
#elif IEEE
      REAL*8 DD(ND)

      CALL FHUPUT(IU,ND*8,DD)
#elif IBM
      REAL*8 DD(ND)
      INTEGER IW(2)

      DO 10 I=1,ND
        CALL FLDBDE(DD(I),IW)
        CALL FHUPUT(IU,8,IW)
   10 CONTINUE
#endif
      END
************************************************************************
*     GET DOUBLE PRECISION
*-----------------------------------------------------------------------
      SUBROUTINE FEGETD(IU,ND,DD)
#ifdef LEN
      REAL*8 DD(ND)
      INTEGER IW(2)

      DO I=1,ND
        CALL FHUGET(IU,8,IW)
        CALL FLCEN8(IW,DD(I))
      END DO
#elif IEEE
      REAL*8 DD(ND)

      CALL FHUGET(IU,ND*8,DD)
#elif IBM
      REAL*8 DD(ND)
      INTEGER IW(2)

      DO 10 I=1,ND
        CALL FHUGET(IU,8,IW)
        CALL FLDEDB(IW,DD(I))
   10 CONTINUE
#endif
      END
************************************************************************
*     PUT DOUBLE PRECISION TRANSFORMING TO REAL (SHORTENING)
*-----------------------------------------------------------------------
      SUBROUTINE FEPUTS(IU,ND,DD)

      REAL*8 DD(ND)
#ifdef LEN
      DO I=1,ND
        RD=DD(I)
        CALL FLCEN4(RD,IW)
        CALL FHUPUT(IU,4,IW)
      END DO
#elif IEEE
      DO 10 I=1,ND
        RD=DD(I)
        CALL FHUPUT(IU,4,RD)
   10 CONTINUE
#elif IBM
      DO 10 I=1,ND
        CALL FLDBRE(DD(I),IW)
        CALL FHUPUT(IU,4,IW)
   10 CONTINUE
#endif
      END
************************************************************************
*     GET DOUBLE PRECISION TRANSFORMING FROM REAL (SHORTENING)
*-----------------------------------------------------------------------
      SUBROUTINE FEGETS(IU,ND,DD)

      REAL*8 DD(ND)
#ifdef LEN
      DO I=1,ND
        CALL FHUGET(IU,4,IW)
        CALL FLCEN4(IW,RD)
        DD(I)=RD
      END DO
#elif IEEE
      DO 10 I=1,ND
        CALL FHUGET(IU,4,RD)
        DD(I)=RD
   10 CONTINUE
#elif IBM
      DO 10 I=1,ND
        CALL FHUGET(IU,4,IW)
        CALL FLREDB(IW,DD(I))
   10 CONTINUE
#endif
      END
************************************************************************
*     PUT REAL TRANSFORMING TO DOUBLE PRECISION (LENGTHENING)
*-----------------------------------------------------------------------
      SUBROUTINE FEPUTL(IU,ND,RD)
#ifdef LEN
      REAL RD(ND)
      REAL*8 DD
      INTEGER IW(2)

      DO I=1,ND
        DD=RD(I)
        CALL FLCEN8(DD,IW)
        CALL FHUPUT(IU,8,IW)
      END DO
#elif IEEE
      REAL RD(ND)
      REAL*8 DD

      DO 10 I=1,ND
        DD=RD(I)
        CALL FHUPUT(IU,8,DD)
   10 CONTINUE
#elif IBM
      REAL RD(ND)
      INTEGER IW(2)

      DO 10 I=1,ND
        CALL FLRBDE(RD(I),IW)
        CALL FHUPUT(IU,8,IW)
   10 CONTINUE
#endif
      END
************************************************************************
*     GET REAL TRANSFORMING FROM DOUBLE PRECISION (LENGTHENING)
*-----------------------------------------------------------------------
      SUBROUTINE FEGETL(IU,ND,RD)
#ifdef LEN
      REAL RD(ND)
      REAL*8 DD
      INTEGER IW(2)

      DO 10 I=1,ND
        CALL FHUGET(IU,8,IW)
        CALL FLCEN8(IW,DD)
        RD(I)=DD
   10 CONTINUE
#elif IEEE
      REAL RD(ND)
      REAL*8 DD

      DO 10 I=1,ND
        CALL FHUGET(IU,8,DD)
        RD(I)=DD
   10 CONTINUE
#elif IBM
      REAL RD(ND)
      INTEGER IW(2)

      DO 10 I=1,ND
        CALL FHUGET(IU,8,IW)
        CALL FLDERB(IW,RD(I))
   10 CONTINUE
#endif
      END
************************************************************************
*     SKIP 1-BYTE VARIABLES
*-----------------------------------------------------------------------
      SUBROUTINE FESKP1(IU,ND)

      CALL FHUSKP(IU,ND)

      END
************************************************************************
*     SKIP 4-BYTE VARIABLES
*-----------------------------------------------------------------------
      SUBROUTINE FESKP4(IU,ND)

      CALL FHUSKP(IU,ND*4)

      END
************************************************************************
*     SKIP 8-BYTE VARIABLES
*-----------------------------------------------------------------------
      SUBROUTINE FESKP8(IU,ND)

      CALL FHUSKP(IU,ND*8)

      END
