!
!      module write_diff_filter_comm_test
!
      module write_diff_filter_comm_test
!
!     Written by H. Matsui on Sep., 2007
!
      use m_precision
!
      use write_diff_4_comm_test
      use m_geometry_filter_comm_test
!
      implicit  none
!
!
!      subroutine output_diff_filter_nod_ctest
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_diff_filter_nod_ctest
!
      open(id_comm_test, file = comm_test_name)
!
      write(id_comm_test,*) 'ntot_nod_filter_diff_pe',                  &
     &                       ntot_nod_filter_diff_pe
!
      write(id_comm_test,*) 'domain, local_nod_id, ',                   &
     &      'global_nod_org, global_nod_get, ',                         &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(istack_filter_nod_diff_pe,              &
     &    ntot_nod_filter_diff_pe, inod_filter_diff_IO,                 &
     &    xx_filter_diff_IO)
!
      close(id_comm_test)
!
      end subroutine output_diff_filter_nod_ctest
!
!  ---------------------------------------------------------------------
!
      end module write_diff_filter_comm_test
