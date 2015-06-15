!filter_send_recv_test.f90
!      module filter_send_recv_test
!
      module filter_send_recv_test
!
!     Written by H. Matsui on May, 2008
!
      use m_precision
!
      implicit  none
!
!      subroutine nod_filter_send_recv_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine nod_filter_send_recv_test
!
      use calypso_mpi
      use m_nod_filter_comm_table
      use m_geometry_filter_comm_test
      use solver_SR_type
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, inter_nod_3dfilter
        xx_filter_comm(3*inod-2) = xx_filtering(inod,1)
        xx_filter_comm(3*inod-1) = xx_filtering(inod,2)
        xx_filter_comm(3*inod  ) = xx_filtering(inod,3)
      end do
!
      call SOLVER_SEND_RECV_3_type                                      &
     &   (nnod_filtering, flt_comm, xx_filter_comm)
!
      end subroutine nod_filter_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module filter_send_recv_test
