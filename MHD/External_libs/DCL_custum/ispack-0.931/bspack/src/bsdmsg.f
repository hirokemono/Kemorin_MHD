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
*     DUMP MESSAGES
*-----------------------------------------------------------------------
      SUBROUTINE BSDMSG(CL,CS,CM)

      CHARACTER CL*1,CS*(*),CM*(*)
      CHARACTER CSD*6,CMD*53
      DATA MMSG,IMSG/20,0/
      SAVE

      CSD=CS
      CMD=CM

      IF(CL.EQ.'E') THEN
        WRITE(6,'(A)') '***** ERROR ('//CSD//') ***  '//CMD
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
