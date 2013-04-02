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
*     GET THE SHIFT BYTES FOR ALIGNMENT                       2009/08/20
************************************************************************
      SUBROUTINE APALGN(C,NALIGN,IS)

      CHARACTER C(*)*1
      DIMENSION IP(2)

      CALL FJGTAD(C,IP)
      
      IF(IP(1).LT.0) THEN
        IS=NALIGN+MOD(IP(1),NALIGN)
      ELSE
        IS=MOD(IP(1),NALIGN)
      END IF
      IS=MOD(NALIGN-IS,NALIGN)

      END
