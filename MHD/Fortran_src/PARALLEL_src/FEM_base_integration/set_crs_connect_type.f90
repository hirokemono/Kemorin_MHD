!
!      module set_crs_connect_type
!
!        programmed by H.Matsui on Dec., 2008
!
!      subroutine s_set_crs_connect_type(nod, neib_nod, tbl_crs)
!        type(node_data),          intent(in) :: nod
!        type(next_nod_id_4_nod),  intent(in) :: neib_nod
!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
      module set_crs_connect_type
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_crs_connect_type(nod, neib_nod, tbl_crs)
!
      use m_machine_parameter
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_crs_connect
!
      use set_crs_connection
      use cal_minmax_and_stacks
!
      type(node_data),          intent(in) :: nod
      type(next_nod_id_4_nod),  intent(in) :: neib_nod
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!
      call alloc_type_crs_stack(nod%numnod, tbl_crs)
!
      call count_item_crs(nod%numnod, np_smp, nod%istack_nod_smp,       &
     &    neib_nod%ntot, neib_nod%istack_next, neib_nod%inod_next,      &
     &    tbl_crs%num_crs_l, tbl_crs%num_crs_u)
!
      call s_cal_minmax_and_stacks(nod%numnod, tbl_crs%num_crs_l,       &
     &    izero, tbl_crs%istack_crs_l, tbl_crs%ntot_crs_l,              &
     &    tbl_crs%max_crs_l, tbl_crs%min_crs_l)
      call s_cal_minmax_and_stacks(nod%numnod, tbl_crs%num_crs_u,       &
     &    izero, tbl_crs%istack_crs_u, tbl_crs%ntot_crs_u,              &
     &    tbl_crs%max_crs_u, tbl_crs%min_crs_u)
!
      call alloc_type_crs_connect(tbl_crs)
!
      call set_item_crs(nod%numnod, np_smp, nod%istack_nod_smp,         &
     &    neib_nod%ntot, neib_nod%istack_next, neib_nod%inod_next,      &
     &    tbl_crs%ntot_crs_l, tbl_crs%ntot_crs_u,                       &
     &    tbl_crs%istack_crs_l, tbl_crs%istack_crs_u,                   &
     &    tbl_crs%item_crs_l, tbl_crs%item_crs_u)
!
      end subroutine s_set_crs_connect_type
!
!-----------------------------------------------------------------------
!
      end module set_crs_connect_type
