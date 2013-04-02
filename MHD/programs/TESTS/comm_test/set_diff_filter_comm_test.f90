!
!      module set_diff_filter_comm_test
!
      module set_diff_filter_comm_test
!
!     Written by H. Matsui on Sep., 2007
!
      use m_precision
!
      use m_nod_filter_comm_table
      use m_geometry_filter_comm_test
      use set_diff_geom_comm_test
!
!
      implicit  none
!
!      subroutine count_filter_node_comm_test
!      subroutine set_diff_filter_nod_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_filter_node_comm_test
!
!
      call count_node_comm_test(nnod_filtering, inter_nod_3dfilter,     &
     &    id_globalnod_filtering, xx_filtering, inod_gl_filter_comm,    &
     &    xx_filter_comm, nnod_filter_diff_local)
!
      end subroutine count_filter_node_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_diff_filter_nod_comm_test
!
!
      call compare_nod_comm_test(nnod_filtering,                        &
     &    inter_nod_3dfilter, id_globalnod_filtering, xx_filtering,     &
     &    inod_gl_filter_comm, xx_filter_comm, nnod_filter_diff_local,  &
     &    inod_filter_diff, inod_gl_filter_diff, xx_filter_diff)
!
      end subroutine set_diff_filter_nod_comm_test
!
! ----------------------------------------------------------------------
!
      end module set_diff_filter_comm_test
