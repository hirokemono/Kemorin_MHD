!error_exit_4_part.f90
!      module error_exit_4_part
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine ERROR_EXIT (IFLAG, nn)
!
      module error_exit_4_part
!
      use m_precision
      use m_error_IDs
      use calypso_mpi
!
      implicit none
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
      if (IFLAG.ge. ierr_ele .and. IFLAG.lt. (ierr_ovnod-1)) then
        write (*,'(/,a)')                                               &
     &        " ### ABORT : unexpected ZERO/minus in the orginal file"
        if (IFLAG.eq.ierr_ele) write (*,'(  a,/)')                      &
     &        "     TOTAL NODE and/or ELEMENT NUMBER"
        if (IFLAG.eq.ierr_ngrp) write (*,'(  a,i12/)')                  &
     &        "     BOUNDARY GROUP NUMBER (1:node, 2:elem, 3:suf)", nn
        if (IFLAG.eq.ierr_grp) write (*,'(  a,i12/)')                   &
     &        "     BOUNDARY info ITEMs   (1:node, 2:elem, 3:suf)", nn
        if (IFLAG.eq.ierr_etype) write (*,'(  a,i12/)')                 &
     &        "     ELEMENT type", nn
        if (IFLAG.eq.ierr_econ) write (*,'(  a,i12/)')                  &
     &        "     ELEMENT connectivity in ", nn
      endif

      if (IFLAG.eq. ierr_ovnod) then
        write (*,'(/,a,i12/)')                                          &
     &        " ### ABORT : local node ID > numnod appears in ELEMENT", nn
      endif

      if (IFLAG.eq.ierr_ov_grp) then
        write (*,'(/,a  )')                                             &
     &        " ### ABORT : local node/elem ID > numnod appears in GROUPS"
        write (*,'(  a,i12/  )')                                        &
     &        "     (1:node, 2:elem, 3:suf)", nn
      endif

      if (IFLAG.eq.ierr_sf_grp) then
        write (*,'(/,a,i12/)')                                          &
     &       " ### ABORT : local surface ID inconsistent in SUF.GRP.",  &
     &          nn
      endif

      if (IFLAG.eq.ierr_file) then
        write (*,'(/,a,/)')                                             &
     &       " ### ABORT : ERROR in ORIGINAL GRID FILE : Parallel Info"
      endif

      if (IFLAG.eq.ierr_MeTISfile) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : ERROR in GRID/MeTiS FILE"
      endif

      if (IFLAG.eq.ierr_MeTISData) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : UNEXPECTED EOF in GRID/MeTiS FILE"
      endif

      if (IFLAG.eq.ierr_P_MPI) then
        write (*,'(/,a,2i12/)')                                         &
     &        " ### ABORT : INVALID PE  and node #", nn
      endif

      if (IFLAG.eq.5000) then
        write (*,'(/,a,i12/)')                                          &
     &        " ### ABORT : INVALID element type", nn
      endif

      if (IFLAG.eq.5001) then
        write (*,'(/,a,i12/)')                                          &
     &        " ### ABORT : UNSUPPORTED element type", nn
      endif
      if(IFLAG .GT. 0) call calypso_MPI_abort(IFLAG, 'Read file error')
!
      end subroutine ERROR_EXIT
!
!   --------------------------------------------------------------------
!
      end module error_exit_4_part
