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
*     FHPACK: FILE HANDLER      (VERSION 1.0)      95/12/04 BY K.ISHIOKA
*                                                  99/03/12 BY K.ISHIOKA
************************************************************************
*     FHU: UPPER ROUTINES OF FHPACK
************************************************************************
      SUBROUTINE FHUOPN(IU,CF,CAD,NBD)

      PARAMETER(NU1=10,NU2=30,NU=NU2-NU1+1,MB=23476)
      DIMENSION NB(NU1:NU2),IB(NU1:NU2),IR(NU1:NU2),IC(NU1:NU2)
      CHARACTER CB(NU1:NU2)*(MB),CA(NU1:NU2)*1
      CHARACTER CF*(*),CD*(*),CAD*1
      CHARACTER CX*10,CY*10
      LOGICAL LX,LC
      DATA CA/NU*'*'/,LC/.FALSE./
      SAVE

      IF(IU.LT.NU1.OR.IU.GT.NU2) THEN
        CALL FHDMSG('E','FHUOPN','IU IS INVALID')
      ELSE IF(NBD.GT.MB) THEN
        CALL FHDMSG('E','FHUOPN','NB IS TOO LARGE')
      END IF

      IF(CAD.EQ.'W'.OR.CAD.EQ.'w') THEN
        CA(IU)='W'
        NB(IU)=NBD
        IR(IU)=1
        IB(IU)=0
      ELSE IF(CAD.EQ.'R'.OR.CAD.EQ.'r') THEN
        CA(IU)='R'
        NB(IU)=NBD
        IR(IU)=0
        IB(IU)=NBD
      ELSE IF(CAD.EQ.'A'.OR.CAD.EQ.'a') THEN
        CA(IU)='A'
        NB(IU)=NBD
        IR(IU)=0
        IB(IU)=NBD
      ELSE
        CALL FHDMSG('E','FHUOPN','CA IS INVALID')
      END IF

      IF(CF(1:1).EQ.'*') THEN
        INQUIRE(IU,OPENED=LX)
        IF(.NOT.LX) THEN
          CALL FHDMSG('E','FHUOPN','THE UNIT IS NOT CONNECTED')
        END IF
      ELSE
        INQUIRE(FILE=CF,EXIST=LX)
        IF(CA(IU).EQ.'R'.AND..NOT.LX) THEN
          CALL FHDMSG('E','FHUOPN','THE FILE DOES NOT EXIST')
        END IF
      END IF

      CALL FHLOPN(IU,CF,CA(IU),NB(IU),IC(IU))

      CALL FHCKIC(LC,'FHUOPN',IC(IU))

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUPUT(IU,ND,CD)

      IF(CA(IU).EQ.'R') THEN
        CALL FHDMSG('E','FHUPUT','INVALID ACTION MODE')
      END IF

      CALL FHMPUT(IU,CB(IU),NB(IU),IR(IU),IB(IU),ND,CD,IC(IU))

      CALL FHCKIC(LC,'FHUPUT',IC(IU))

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUGET(IU,ND,CD)

      IF(CA(IU).NE.'R') THEN
        CALL FHDMSG('E','FHUGET','INVALID ACTION MODE')
      END IF

      CALL FHMGET(IU,CB(IU),NB(IU),IR(IU),IB(IU),ND,CD,IC(IU))

      CALL FHCKIC(LC,'FHUGET',IC(IU))

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUSKP(IU,IS)

      IF(CA(IU).NE.'R') THEN
        CALL FHDMSG('E','FHUSKP','INVALID ACTION MODE')
      END IF

      CALL FHMSKP(IU,CB(IU),NB(IU),IR(IU),IB(IU),IS,IC(IU))

      CALL FHCKIC(LC,'FHUSKP',IC(IU))

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUJMP(IU,IP)

      IF(CA(IU).NE.'R') THEN
        CALL FHDMSG('E','FHUJMP','INVALID ACTION MODE')
      END IF

      IPD=NB(IU)*(IR(IU)-1)+IB(IU)
      CALL FHMSKP(IU,CB(IU),NB(IU),IR(IU),IB(IU),IP-IPD,IC(IU))

      CALL FHCKIC(LC,'FHUJMP',IC(IU))

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHURWD(IU)

      IF(CA(IU).NE.'R') THEN
        CALL FHDMSG('E','FHURWD','INVALID ACTION MODE')
      END IF

      CALL FHMRWD(IU,NB(IU),IR(IU),IB(IU),IC(IU))

      CALL FHCKIC(LC,'FHURWD',IC(IU))

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUQNP(IU,NP)

      IF(CA(IU).NE.'R') THEN
        CALL FHDMSG('E','FHUQNP','INVALID ACTION MODE')
      END IF

      IF(NB(IU)*(IR(IU)-1)+IB(IU).NE.0) THEN
        CALL FHDMSG('E','FHUQNP',
     &                  'FHUQNP MUST BE CALLED JUST AFTER FHUOPN.')
      ELSE
        CALL FHMQNP(IU,CB(IU),NB(IU),NP,IC(IU))
      END IF

      CALL FHCKIC(LC,'FHUQNP',IC(IU))

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUQIP(IU,IP)

      IF(CA(IU).EQ.'*') THEN
        CALL FHDMSG('E','FHUQIP','THE FILE IS NOT OPENED.')
      END IF

      IP=NB(IU)*(IR(IU)-1)+IB(IU)

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUQIC(IU,ICD)

      IF(CA(IU).EQ.'*') THEN
        CALL FHDMSG('E','FHUQIP','THE FILE IS NOT OPENED.')
      END IF

      ICD=IC(IU)

      RETURN
*-----------------------------------------------------------------------
      ENTRY FHUCLS(IU)

      IF(CA(IU).EQ.'W'.OR.CA(IU).EQ.'A') THEN
        WRITE(CX,'(I10)') NB(IU)*(IR(IU)-1)+IB(IU)
        WRITE(CY,'(I10)') NB(IU)*IR(IU)
        CALL FHDMSG('M','FHUCLS','WRITTEN DATA SIZE IS '//CX//' BYTES')
        CALL FHDMSG('M','FHUCLS','TOTAL DATA LENGTH IS '//CY//' BYTES')
      END IF

      CALL FHMCLS(IU,CA(IU),CB(IU),NB(IU),IR(IU),IB(IU),IC(IU))

      CA(IU)='*'

      CALL FHCKIC(LC,'FHUCLS',IC(IU))

      END
************************************************************************
*     CHECK IOSTAT
*-----------------------------------------------------------------------
      SUBROUTINE FHCKIC(LC,CS,IC)

      CHARACTER CS*(*)
      LOGICAL LC

      IF(LC.AND.IC.NE.0) THEN
        CALL FHDMSG('E',CS,'I/O TROUBLE OCCURRED.')
      END IF

      END
************************************************************************
*     DUMP MESSAGES
*-----------------------------------------------------------------------
      SUBROUTINE FHDMSG(CL,CS,CM)

      CHARACTER CL*1,CS*(*),CM*(*)
      CHARACTER CSD*6,CMD*53
      DATA MMSG,IMSG/20,0/
      SAVE

      CSD=CS
      CMD=CM

      IF(CL.EQ.'E') THEN
        WRITE(*,'(A)') '***** ERROR ('//CSD//') ***  '//CMD
        STOP
      END IF

      IF(IMSG.LT.MMSG) THEN
        IF(CL.EQ.'W') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** WARNING ('//CSD//') ***  '//CMD
        ELSE IF(CL.EQ.'M') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** MESSAGE ('//CSD//') ***  '//CMD
        END IF
        IF(IMSG.EQ.MMSG) THEN
          WRITE(*,*) '+++ THE FOLLOWING MESSAGES ARE SUPRRESSED.'
        END IF
      END IF

      END
************************************************************************
*     FHM: LOWER ROUTINES OF FHPACK
************************************************************************
      SUBROUTINE FHMPUT(IU,CB,NB,IR,IB,ND,CD,IC)

      CHARACTER CB*(*),CD*(*)

      ID=0
   10 IF(ND-ID.GE.NB-IB) THEN
        CB(IB+1:NB)=CD(ID+1:ID+NB-IB)
        CALL FHLPUT(IU,CB,NB,IR,IC)
        IR=IR+1
        ID=ID+NB-IB
        IB=0
        GO TO 10
      END IF

      IF(ND-ID.GT.0) THEN
        CB(IB+1:IB+ND-ID)=CD(ID+1:ND)
        IB=IB+ND-ID
      END IF

      END
************************************************************************
      SUBROUTINE FHMGET(IU,CB,NB,IR,IB,ND,CD,IC)

      CHARACTER CB*(*),CD*(*)

      IF(IB.EQ.NB) THEN
        IR=IR+1
        CALL FHLGET(IU,CB,NB,IR,IC)
        IB=0
      END IF

      ID=0
   10 IF(ND-ID.GT.NB-IB) THEN
        CD(ID+1:ID+NB-IB)=CB(IB+1:NB)
        IR=IR+1
        CALL FHLGET(IU,CB,NB,IR,IC)
        ID=ID+NB-IB
        IB=0
        GO TO 10
      END IF

      CD(ID+1:ND)=CB(IB+1:IB+ND-ID)
      IB=IB+ND-ID

      END
************************************************************************
      SUBROUTINE FHMSKP(IU,CB,NB,IR,IB,IS,IC)

      CHARACTER CB*(*)

      IF(IB+IS.GT.NB) THEN
        IT=(IB+IS-1)/NB
        IR=IR+IT
        IB=IB+IS-IT*NB
        DO 10 I=1,IT-1
          CALL FHLFWD(IU,IC)
   10   CONTINUE
        CALL FHLGET(IU,CB,NB,IR,IC)
      ELSE IF(IB+IS.LT.0) THEN
        IT=(IB+IS-NB+1)/NB
        IR=IR+IT
        IB=IB+IS-IT*NB
        DO 20 I=1,1-IT
          CALL FHLBSP(IU,IC)
   20   CONTINUE
        CALL FHLGET(IU,CB,NB,IR,IC)
      ELSE
        IB=IB+IS
      END IF

      END
************************************************************************
      SUBROUTINE FHMRWD(IU,NB,IR,IB,IC)

      IR=0
      IB=NB
      CALL FHLRWD(IU,IC)

      END
************************************************************************
      SUBROUTINE FHMQNP(IU,CB,NB,NP,IC)

      CHARACTER CB*(*)
      CHARACTER CEOF*1

      CEOF=CHAR(32)

      CALL FHLEND(IU,CB,NB,IR,IC)

      IF(IC.EQ.0) THEN
        IB=NB
   10   IF(IB.GT.0.AND.CB(IB:IB).NE.CEOF) THEN
          IB=IB-1
          GOTO 10
        END IF
        NP=NB*(IR-1)+IB-1
        CALL FHLRWD(IU,IC)
      END IF

      END
************************************************************************
      SUBROUTINE FHMCLS(IU,CA,CB,NB,IR,IB,IC)

      CHARACTER CB*(*),CA*1
      CHARACTER CEOF*1,CNULL*1

      CEOF=CHAR(32)
      CNULL=CHAR(64)

      IF(CA.EQ.'W') THEN
        CB(IB+1:IB+1)=CEOF
        DO 10 I=IB+2,NB
          CB(I:I)=CNULL
   10   CONTINUE
        CALL FHLPUT(IU,CB,NB,IR,IC)
      END IF

      CLOSE(IU,IOSTAT=IC)

      END
************************************************************************
*     FHL: THE LOWEST ROUTINES OF FHPACK
************************************************************************
#ifdef FORMAT
*-----------------------------------------------------------------------
      SUBROUTINE FHLOPN(IU,CF,CA,NB,IC)

      CHARACTER CF*(*),CA*1
      LOGICAL LX

      IF(CF(1:1).EQ.'*') THEN
        OPEN(IU,ACCESS='DIRECT',RECL=NB,FORM='FORMATTED',IOSTAT=IC)
      ELSE
        IF(CA.EQ.'W') THEN
          INQUIRE(FILE=CF,EXIST=LX)
          IF(LX) THEN
            OPEN(IU,FILE=CF)
            CLOSE(IU,STATUS='DELETE')
          END IF
        END IF
        OPEN(IU,FILE=CF,ACCESS='DIRECT',RECL=NB,FORM='FORMATTED',
     &      IOSTAT=IC)
      END IF

      END
*-----------------------------------------------------------------------
#elif DIRECT
*-----------------------------------------------------------------------
      SUBROUTINE FHLOPN(IU,CF,CA,NB,IC)

      CHARACTER CF*(*),CA*1
      LOGICAL LX

      IF(CF(1:1).EQ.'*') THEN
        OPEN(IU,ACCESS='DIRECT',RECL=NB,IOSTAT=IC)
      ELSE
        IF(CA.EQ.'W') THEN
          INQUIRE(FILE=CF,EXIST=LX)
          IF(LX) THEN
            OPEN(IU,FILE=CF)
            CLOSE(IU,STATUS='DELETE')
          END IF
        END IF
        OPEN(IU,FILE=CF,ACCESS='DIRECT',RECL=NB,IOSTAT=IC)
      END IF

      END
*-----------------------------------------------------------------------
#else
*-----------------------------------------------------------------------
      SUBROUTINE FHLOPN(IU,CF,CA,NB,IC)

      CHARACTER CF*(*),CA*1,CDMMY*1

      IDMMY=NB
      CDMMY=CA

      IF(CF(1:1).NE.'*') THEN
        OPEN(IU,FILE=CF,IOSTAT=IC)
      END IF

      END
*-----------------------------------------------------------------------
#endif
************************************************************************
#ifdef FORMAT
*-----------------------------------------------------------------------
      SUBROUTINE FHLPUT(IU,CB,NB,IR,IC)

      CHARACTER CB*(*)

      WRITE(IU,'(A)',REC=IR,IOSTAT=IC) CB(1:NB)

      END
*-----------------------------------------------------------------------
      SUBROUTINE FHLGET(IU,CB,NB,IR,IC)

      CHARACTER CB*(*)

      READ(IU,'(A)',REC=IR,IOSTAT=IC) CB(1:NB)

      END
*-----------------------------------------------------------------------
#elif DIRECT
*-----------------------------------------------------------------------
      SUBROUTINE FHLPUT(IU,CB,NB,IR,IC)

      CHARACTER CB*(*)

      WRITE(IU,REC=IR,IOSTAT=IC) CB(1:NB)

      END
*-----------------------------------------------------------------------
      SUBROUTINE FHLGET(IU,CB,NB,IR,IC)

      CHARACTER CB*(*)

      READ(IU,REC=IR,IOSTAT=IC) CB(1:NB)

      END
*-----------------------------------------------------------------------
#elif SEQUENTIAL
*-----------------------------------------------------------------------
      SUBROUTINE FHLPUT(IU,CB,NB,IR,IC)

      CHARACTER CB*(*)

      IDMMY=IR
      WRITE(IU,'(A)',IOSTAT=IC) CB(1:NB)

      END
*-----------------------------------------------------------------------
      SUBROUTINE FHLGET(IU,CB,NB,IR,IC)

      CHARACTER CB*(*)

      IDMMY=IR
      READ(IU,'(A)',IOSTAT=IC) CB(1:NB)

      END
*-----------------------------------------------------------------------
#endif
************************************************************************
#ifdef DIRECT
*-----------------------------------------------------------------------
      SUBROUTINE FHLFWD(IU,IC)

      IDUMMY=IU
      IC=0

      END
*-----------------------------------------------------------------------
      SUBROUTINE FHLBSP(IU,IC)

      IDUMMY=IU
      IC=0

      END
*-----------------------------------------------------------------------
      SUBROUTINE FHLRWD(IU,IC)

      IDUMMY=IU
      IC=0

      END
*-----------------------------------------------------------------------
#elif SEQUENTIAL
*-----------------------------------------------------------------------
      SUBROUTINE FHLFWD(IU,IC)

      READ(IU,'(A)',IOSTAT=IC)

      END
*-----------------------------------------------------------------------
      SUBROUTINE FHLBSP(IU,IC)

      BACKSPACE(IU,IOSTAT=IC)

      END
*-----------------------------------------------------------------------
      SUBROUTINE FHLRWD(IU,IC)

      REWIND(IU,IOSTAT=IC)

      END
*-----------------------------------------------------------------------
#endif
************************************************************************
      SUBROUTINE FHLEND(IU,CB,NB,IR,IC)

      CHARACTER CB*(*)

      IC=0
      IR=0
   10 IF(IC.EQ.0) THEN
        IR=IR+1
        CALL FHLGET(IU,CB,NB,IR,IC)
        GOTO 10
      END IF

      IR=IR-1
      CALL FHLBSP(IU,IC)
      CALL FHLBSP(IU,IC)
      CALL FHLGET(IU,CB,NB,IR,IC)

      END
