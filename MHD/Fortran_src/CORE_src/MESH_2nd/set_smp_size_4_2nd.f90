!
!      module set_smp_size_4_2nd
!
!     Written by H. Matsui on Sep., 2005
!     Modified by H. Matsui on Aug., 2006
!
!      subroutine s_count_all_smp_size_4_2nd
!
!      subroutine s_count_smp_size_4_2nd
!      subroutine s_count_smp_size_2nd_surf_edge
!
      module set_smp_size_4_2nd
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_2nd_geometry_param
!
      use cal_minmax_and_stacks
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_count_all_smp_size_4_2nd
!
      call s_count_smp_size_4_2nd
      call s_count_smp_size_2nd_surf_edge
!
      end subroutine s_count_all_smp_size_4_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_count_smp_size_4_2nd
!
!
      call allocate_2nd_geomet_param_smp
!
!
       call count_number_4_smp( np_smp, ione, nnod_2nd,                 &
     &       inod_smp_stack_2nd, maxnod_4_smp_2nd )
!
       call count_number_4_smp( np_smp, ione, internal_nod_2nd,         &
     &       inter_smp_stack_2nd, max_in_nod_4_smp_2nd )
!
       call count_number_4_smp( np_smp, ione, nele_2nd,                 &
     &       iele_smp_stack_2nd, maxele_4_smp_2nd )
!
      end subroutine s_count_smp_size_4_2nd
!
!-----------------------------------------------------------------------
!
      subroutine s_count_smp_size_2nd_surf_edge
!
      call allocate_2nd_surf_param_smp
      call allocate_2nd_edge_param_smp
!
      call count_number_4_smp( np_smp, ione, nsurf_2nd,                 &
     &       isurf_smp_stack_2nd, maxsurf_4_smp_2nd )
      call count_number_4_smp( np_smp, ione, nedge_2nd,                 &
     &       iedge_smp_stack_2nd, maxedge_4_smp_2nd )
!
      end subroutine s_count_smp_size_2nd_surf_edge
!
!-----------------------------------------------------------------------
!
      end module set_smp_size_4_2nd
