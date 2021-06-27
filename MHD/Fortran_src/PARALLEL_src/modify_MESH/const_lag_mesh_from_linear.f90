!>@file   const_lag_mesh_from_linear.f90
!!@brief  module const_lag_mesh_from_linear
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief Construct quad mesh data from linear mesh
!!
!!@verbatim
!!      subroutine const_lag_node_by_linear                             &
!!     &         (node_l, ele_l, surf_l, edge_l, l_to_lag, node_q)
!!        type(node_data), intent(in) :: node_l
!!        type(element_data), intent(in) :: ele_l
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        type(node_data), intent(inout) :: node_q
!!      subroutine const_lag_ele_by_linear                              &
!!     &         (ele_l, surf_l, edge_l, l_to_lag, ele_q)
!!        type(element_data), intent(in) :: ele_l
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        type(element_data), intent(inout) :: ele_q
!!      subroutine const_lag_surf_by_linear                             &
!!     &         (surf_l, edge_l, l_to_lag, ele_q, surf_q)
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        type(element_data), intent(in) :: ele_q
!!        type(surface_data), intent(inout) :: surf_q
!!      subroutine const_lag_edge_by_linear                             &
!!     &         (edge_l, l_to_lag, ele_q, surf_q, edge_q)
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        type(element_data), intent(in) :: ele_q
!!        type(surface_data), intent(in) :: surf_q
!!        type(edge_data), intent(inout) :: edge_q
!!
!!      subroutine const_node_comm_table_l2lag                          &
!!     &         (node_l, ele_l, surf_l, edge_l,                        &
!!     &          nod_comm_l, ele_comm_l, surf_comm_l, edge_comm_l,     &
!!     &          l_to_lag, nod_comm_q)
!!        type(node_data), intent(in) :: node_l
!!        type(element_data), intent(in) :: ele_l
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(communication_table), intent(in) :: nod_comm_l
!!        type(communication_table), intent(in) :: ele_comm_l
!!        type(communication_table), intent(in) :: surf_comm_l
!!        type(communication_table), intent(in) :: edge_comm_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        type(communication_table), intent(inout) :: nod_comm_q
!!
!!      subroutine const_node_group_item_l2lag                          &
!!     &         (node_l, ele_l, surf_l, edge_l, nod_grp_l, l_to_lag,   &
!!     &          nod_grp_q)
!!        type(node_data), intent(in) :: node_l
!!        type(element_data), intent(in) :: ele_l
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(group_data), intent(in) :: nod_grp_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        type(group_data), intent(inout) :: nod_grp_q
!!@endverbatim
      module const_lag_mesh_from_linear
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
      use t_calypso_comm_table
      use t_group_data
      use t_linear_to_lag_list
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_lag_node_by_linear                               &
     &         (node_l, ele_l, surf_l, edge_l, l_to_lag, node_q)
!
      use copy_node_data
      use set_lag_node_ele_by_linear
!
      type(node_data), intent(in) :: node_l
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      type(node_data), intent(inout) :: node_q
!
!
      node_q%numnod =        node_l%numnod + edge_l%numedge             &
     &                      + surf_l%numsurf + ele_l%numele
      node_q%internal_node = node_l%internal_node                       &
     &                      + l_to_lag%internal_edge_l2lag              &
     &                      + l_to_lag%internal_surf_l2lag              &
     &                      + l_to_lag%internal_ele_l2lag
      call alloc_node_geometry_base(node_q)
      call set_lag_node_by_linear(node_l, ele_l, surf_l, edge_l,        &
     &    l_to_lag, node_q%numnod, node_q%inod_global, node_q%xx)
!
      call dup_derived_node_data(nprocs, node_l, node_q)
!
      end subroutine const_lag_node_by_linear
!
! ----------------------------------------------------------------------
!
      subroutine const_lag_ele_by_linear                                &
     &         (ele_l, surf_l, edge_l, l_to_lag, ele_q)
!
      use copy_element_data
      use set_lag_node_ele_by_linear
!
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      type(element_data), intent(inout) :: ele_q
!
!
      ele_q%numele =     ele_l%numele
      ele_q%nnod_4_ele = num_t_lag
      call alloc_element_types(ele_q)
      call copy_global_element_id                                       &
     &   (ele_l, ele_q%numele, ele_q%first_ele_type,                    &
     &    ele_q%iele_global, ele_q%elmtyp, ele_q%nodelm)
!
      call alloc_ele_connectivity(ele_q)
      call set_lag_ele_connect_by_linear                                &
     &   (ele_l, surf_l, edge_l, l_to_lag, ele_q%numele, ele_q%ie)
!
      call dup_derived_element_data(nprocs, ele_l, ele_q)
!
      end subroutine const_lag_ele_by_linear
!
! ----------------------------------------------------------------------
!
      subroutine const_lag_surf_by_linear                               &
     &         (surf_l, edge_l, l_to_lag, ele_q, surf_q)
!
      use copy_surface_data
      use set_lag_node_ele_by_linear
!
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
      type(element_data), intent(in) :: ele_q
!
      type(surface_data), intent(inout) :: surf_q
!
!
      surf_q%numsurf =     surf_l%numsurf
      surf_q%nnod_4_surf = num_lag_sf
      call allocate_inod_in_surf(surf_q)
      call set_inod_in_surf(surf_q%nnod_4_surf,                         &
     &                      surf_q%node_on_sf, surf_q%node_on_sf_n)
!
      call alloc_surface_connect(surf_q, ele_q%numele)
      call set_lag_surf_connect_by_linear(surf_l, edge_l, l_to_lag,     &
     &    surf_q%numsurf, surf_q%ie_surf)
      call copy_surface_to_element (surf_l, ele_q%numele,               &
     &    surf_q%isf_4_ele, surf_q%isf_rot_ele)
!
      call dup_derived_surface_data(surf_l, surf_q)
!
      end subroutine const_lag_surf_by_linear
!
!-----------------------------------------------------------------------
!
      subroutine const_lag_edge_by_linear                               &
     &         (edge_l, l_to_lag, ele_q, surf_q, edge_q)
!
      use set_local_id_table_4_1ele
      use set_lag_node_ele_by_linear
!
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
!
      type(edge_data), intent(inout) :: edge_q
!
!
      edge_q%numedge =     edge_l%numedge
      edge_q%nnod_4_edge = num_quad_edge
      call allocate_inod_in_edge(edge_q)
      call copy_inod_in_edge(edge_q%nnod_4_edge,                        &
     &    edge_q%node_on_edge, edge_q%node_on_edge_sf)
!
      call alloc_edge_connect(edge_q, surf_q%numsurf)
      call set_lag_edge_connect_by_linear                               &
     &   (edge_l, l_to_lag, edge_q%numedge, edge_q%ie_edge)
      call copy_edge_to_surface(edge_l, surf_q%numsurf,                 &
     &                          edge_q%iedge_4_sf)
!
      call alloc_edge_4_ele(edge_q, ele_q%numele)
      call copy_edge_to_element(edge_l, ele_q%numele,                   &
     &                          edge_q%iedge_4_ele)
!
      call dup_derived_edge_data(edge_l, edge_q)
!
      end subroutine const_lag_edge_by_linear
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_node_comm_table_l2lag                            &
     &         (node_l, ele_l, surf_l, edge_l,                          &
     &          nod_comm_l, ele_comm_l, surf_comm_l, edge_comm_l,       &
     &          l_to_lag, nod_comm_q)
!
      use cvt_calypso_geofem_comm_tbl
      use append_communication_table
!
      type(node_data), intent(in) :: node_l
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(communication_table), intent(in) :: nod_comm_l
      type(communication_table), intent(in) :: ele_comm_l
      type(communication_table), intent(in) :: surf_comm_l
      type(communication_table), intent(in) :: edge_comm_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      type(communication_table), intent(inout) :: nod_comm_q
!
      type(communication_table) :: comm_tmp1, comm_tmp2, comm_tmp3
      type(calypso_comm_table) :: cps_nod_comm, cps_edge_comm
      type(calypso_comm_table) :: cps_surf_comm, cps_ele_comm
!
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    node_l%numnod, nod_comm_l, cps_nod_comm)
      call convert_comm_item_q2l                                        &
     &   (node_l%numnod, l_to_lag%inod_linear_to_lag,                   &
     &    cps_nod_comm%ntot_import, cps_nod_comm%item_import,           &
     &    cps_nod_comm%ntot_export, cps_nod_comm%item_export)
      call dup_calypso_comm_to_comm_tbl                                 &
     &   (my_rank, nprocs, cps_nod_comm, comm_tmp1)
      call dealloc_calypso_comm_table(cps_nod_comm)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    edge_l%numedge, edge_comm_l, cps_edge_comm)
      call convert_comm_item_q2l                                        &
     &   (edge_l%numedge, l_to_lag%iedge_linear_to_lag,                 &
     &    cps_edge_comm%ntot_import, cps_edge_comm%item_import,         &
     &    cps_edge_comm%ntot_export, cps_edge_comm%item_export)
      call append_communication_tbl                                     &
     &   (comm_tmp1, cps_edge_comm, comm_tmp2)
      call dealloc_calypso_comm_table(cps_edge_comm)
      call dealloc_comm_table(comm_tmp1)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    surf_l%numsurf, surf_comm_l, cps_surf_comm)
      call convert_comm_item_q2l                                        &
     &   (surf_l%numsurf, l_to_lag%isurf_linear_to_lag,                 &
     &    cps_surf_comm%ntot_import, cps_surf_comm%item_import,         &
     &    cps_surf_comm%ntot_export, cps_surf_comm%item_export)
      call append_communication_tbl                                     &
     &   (comm_tmp2, cps_surf_comm, comm_tmp3)
      call dealloc_calypso_comm_table(cps_surf_comm)
      call dealloc_comm_table(comm_tmp2)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    ele_l%numele, ele_comm_l, cps_ele_comm)
      call convert_comm_item_q2l                                        &
     &   (ele_l%numele, l_to_lag%iele_linear_to_lag,                    &
     &    cps_ele_comm%ntot_import, cps_ele_comm%item_import,           &
     &    cps_ele_comm%ntot_export, cps_ele_comm%item_export)
      call append_communication_tbl                                     &
     &   (comm_tmp3, cps_ele_comm, nod_comm_q)
      call dealloc_calypso_comm_table(cps_ele_comm)
      call dealloc_comm_table(comm_tmp3)
!
      end subroutine const_node_comm_table_l2lag
!
!-----------------------------------------------------------------------
!
      subroutine const_node_group_item_l2lag                            &
     &         (node_l, ele_l, surf_l, edge_l, nod_grp_l, l_to_lag,     &
     &          nod_grp_q)
!
      use const_node_group_item_q2l
!
      type(node_data), intent(in) :: node_l
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(group_data), intent(in) :: nod_grp_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      type(group_data), intent(inout) :: nod_grp_q
!
      integer(kind = kint), allocatable :: istack_new_ed_smp(:,:)
      integer(kind = kint), allocatable :: istack_new_sf_smp(:,:)
      integer(kind = kint), allocatable :: istack_new_ele_smp(:,:)
      integer(kind = kint), allocatable :: iflag_nod(:)
!
!
      nod_grp_q%num_grp = nod_grp_l%num_grp
      call alloc_group_num(nod_grp_q)
      allocate(istack_new_ed_smp(0:np_smp,nod_grp_q%num_grp))
      allocate(istack_new_sf_smp(0:np_smp,nod_grp_q%num_grp))
      allocate(istack_new_ele_smp(0:np_smp,nod_grp_q%num_grp))
      allocate(iflag_nod(node_l%numnod))
!
!$omp parallel workshare
      nod_grp_q%grp_name(1:nod_grp_q%num_grp)                           &
     &      = nod_grp_l%grp_name(1:nod_grp_q%num_grp)
!$omp end parallel workshare
!
      call count_node_group_item_l2lag                                  &
     &   (node_l, ele_l, surf_l, edge_l, nod_grp_l,                     &
     &    nod_grp_q%num_grp, nod_grp_q%nitem_grp,                       &
     &    istack_new_ed_smp, istack_new_sf_smp, istack_new_ele_smp,     &
     &    nod_grp_q%istack_grp, nod_grp_q%num_item, iflag_nod)
!
      call alloc_group_item(nod_grp_q)
      call set_node_group_item_l2lag                                    &
     &   (node_l, ele_l, surf_l, edge_l, nod_grp_l,                     &
     &    l_to_lag, nod_grp_q%num_grp, istack_new_ed_smp,               &
     &    istack_new_sf_smp, istack_new_ele_smp, nod_grp_q%istack_grp,  &
     &    nod_grp_q%num_item, nod_grp_q%item_grp, iflag_nod)
!
      deallocate(iflag_nod)
      deallocate(istack_new_ele_smp, istack_new_sf_smp)
      deallocate(istack_new_ed_smp)
!
      end subroutine const_node_group_item_l2lag
!
!-----------------------------------------------------------------------
!
      end module  const_lag_mesh_from_linear
