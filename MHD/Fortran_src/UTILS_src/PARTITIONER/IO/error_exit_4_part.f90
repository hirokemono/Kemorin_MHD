!error_exit_4_part.f90
!      module error_exit_4_part
!
      module error_exit_4_part
!
!      Written by H. Matsui on Sep., 2007
!
      use m_precision
!
      implicit none
!
!      subroutine ERROR_EXIT (IFLAG, nn)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine ERROR_EXIT (IFLAG, nn)
!
      integer(kind = kint), intent(in) :: IFLAG, nn
!
      write (*,'(/,a)')                                                 &
     &        "********** Error MESSAGE from Partitioner **********"
      if (IFLAG.ge. 1001 .and. IFLAG.lt.2000) then
        write (*,'(/,a)')                                               &
     &        " ### ABORT : unexpected ZERO/minus in the orginal file"
        if (IFLAG.eq.1001) write (*,'(  a,/)')                          &
     &        "     TOTAL NODE and/or ELEMENT NUMBER"
        if (IFLAG.eq.1002) write (*,'(  a,i12/)')                       &
     &        "     BOUNDARY GROUP NUMBER (1:node, 2:elem, 3:suf)", nn
        if (IFLAG.eq.1003) write (*,'(  a,i12/)')                       &
     &        "     BOUNDARY info ITEMs   (1:node, 2:elem, 3:suf)", nn
        if (IFLAG.eq.1004) write (*,'(  a,i12/)')                       &
     &        "     ELEMENT type", nn
        if (IFLAG.eq.1005) write (*,'(  a,i12/)')                       &
     &        "     ELEMENT connectivity in ", nn
        stop
      endif

      if (IFLAG.eq. 2001) then
        write (*,'(/,a,i12/)')                                          &
     &        " ### ABORT : local node ID > numnod appears in ELEMENT", nn
        stop
      endif

      if (IFLAG.eq.2002) then
        write (*,'(/,a  )')                                             &
     &        " ### ABORT : local node/elem ID > numnod appears in GROUPS"
        write (*,'(  a,i12/  )')                                        &
     &        "     (1:node, 2:elem, 3:suf)", nn
        stop
      endif

      if (IFLAG.eq.2003) then
        write (*,'(/,a,i12/)')                                          &
     &        " ### ABORT : local surface ID inconsistent in SUF.GRP.", &
     &          nn
        stop
      endif

      if (IFLAG.eq.6000) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : ERROR in ORIGINAL GRID FILE : Parallel Info"
        stop
      endif

      if (IFLAG.eq.21) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : ERROR in GRID/MeTiS FILE"
        stop
      endif

      if (IFLAG.eq.22) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : UNEXPECTED EOF in GRID/MeTiS FILE"
        stop
      endif

      if (IFLAG.eq.32) then
        write (*,'(/,a,2i12/)')                                         &
     &        " ### ABORT : INVALID PE  and node #", nn
        stop
      endif

      if (IFLAG.eq.5000) then
        write (*,'(/,a,i12/)')                                          &
     &        " ### ABORT : INVALID element type", nn
        stop
      endif

      if (IFLAG.eq.5001) then
        write (*,'(/,a,i12/)')                                          &
     &        " ### ABORT : UNSUPPORTED element type", nn
        stop
      endif

      end subroutine ERROR_EXIT
!
!   --------------------------------------------------------------------
!
      end module error_exit_4_part
