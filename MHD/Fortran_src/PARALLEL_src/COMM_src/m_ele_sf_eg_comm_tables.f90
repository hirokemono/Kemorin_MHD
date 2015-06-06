!>@file   m_ele_sf_eg_comm_tables.f90
!!@brief  module m_ele_sf_eg_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine dealloc_ele_sf_eg_comm_tables
!!      subroutine const_element_comm_tables_1st
!!@endverbatim
!
      module m_ele_sf_eg_comm_tables
!
      use m_precision
      use t_comm_table
!
      use calypso_mpi
!
      implicit none
!
      type(communication_table), save :: ele_comm
      type(communication_table), save :: surf_comm
      type(communication_table), save :: edge_comm
!
      private :: const_element_comm_table_1st
      private :: const_edge_comm_table_1st
      private :: const_surf_comm_table_1st
      private :: const_ele_comm_table_1st
      private :: const_global_numnod_list_1st
      private :: const_global_element_id_1st
      private :: const_global_surface_id_1st, const_global_edge_id_1st
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_sf_eg_comm_tables
!
      use m_geometry_data
!
!
      call deallocate_numnod_stack
      call deallocate_numele_stack
      call deallocate_numsurf_stack
      call deallocate_numedge_stack
!
      call deallocate_type_comm_tbl(ele_comm)
      call deallocate_type_comm_tbl(surf_comm)
      call deallocate_type_comm_tbl(edge_comm)
!
      end subroutine dealloc_ele_sf_eg_comm_tables
!
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_tables_1st
!
!
      call const_global_numnod_list_1st
!
      call const_element_comm_table_1st
      call const_global_element_id_1st
!
      call const_surf_comm_table_1st
      call const_global_surface_id_1st
!
      call const_edge_comm_table_1st
      call const_global_edge_id_1st
!
      end subroutine const_element_comm_tables_1st
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_global_numnod_list_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use const_global_element_ids
!
!
      call allocate_numnod_stack(nprocs)
!
      call count_number_of_node_stack(numnod, istack_numnod)
      call count_number_of_node_stack(internal_node, istack_internod)
!
      end subroutine const_global_numnod_list_1st
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_element_id_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use const_global_element_ids
!
!
      call allocate_numele_stack(nprocs)
!
      call count_number_of_node_stack(numele, istack_numele)
      call count_number_of_node_stack(internal_ele, istack_interele)
!
      call set_global_ele_id                                            &
     &         (numele, istack_interele, interior_ele,                  &
     &          ele_comm%num_neib, ele_comm%id_neib,                    &
     &          ele_comm%istack_import, ele_comm%item_import,           &
     &          ele_comm%istack_export, ele_comm%item_export,           &
     &          iele_global)
!
      end subroutine const_global_element_id_1st
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_surface_id_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use const_global_element_ids
!
!
      call allocate_numsurf_stack(nprocs)
!
      call count_number_of_node_stack(numsurf, istack_numsurf)
      call count_number_of_node_stack(internal_surf, istack_intersurf)
!
      call set_global_ele_id                                            &
     &         (numsurf, istack_intersurf, interior_surf,               &
     &          surf_comm%num_neib, surf_comm%id_neib,                  &
     &          surf_comm%istack_import, surf_comm%item_import,         &
     &          surf_comm%istack_export, surf_comm%item_export,         &
     &          isurf_global)
!
      end subroutine const_global_surface_id_1st
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_edge_id_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use const_global_element_ids
!
!
      call allocate_numedge_stack(nprocs)
!
      call count_number_of_node_stack(numedge, istack_numedge)
      call count_number_of_node_stack(internal_edge, istack_interedge)
!
      call set_global_ele_id                                            &
     &         (numedge, istack_interele, interior_edge,                &
     &          edge_comm%num_neib, edge_comm%id_neib,                  &
     &          edge_comm%istack_import, edge_comm%item_import,         &
     &          edge_comm%istack_export, edge_comm%item_export,         &
     &          iedge_global)
!
      end subroutine const_global_edge_id_1st
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_table_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
      use set_element_id_4_node
!
!
!
      call set_ele_id_4_node_t
      call belonged_ele_id_4_node_1(blng_tbls%host_ele)
      call const_ele_comm_table_1st(numnod, numele, inod_global,        &
     &    interior_ele, x_ele, blng_tbls%host_ele, blng_tbls%host_ele,  &
     &    ele_comm)
      call dealloc_iele_belonged_type(blng_tbls%host_ele)
      call dealloc_iele_belonged_type(ele_4_nod)
!
      end subroutine const_element_comm_table_1st
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
      use set_element_id_4_node
!
!
      call set_surf_id_4_node
      call belonged_surf_id_4_node_1(blng_tbls%host_surf)
      call const_ele_comm_table_1st(numnod, numsurf, inod_global,       &
     &    interior_surf, x_surf, blng_tbls%host_surf,                   &
     &    blng_tbls%host_surf, surf_comm)
      call dealloc_iele_belonged_type(blng_tbls%host_surf)
      call dealloc_iele_belonged_type(surf_4_nod)
!
      end subroutine const_surf_comm_table_1st
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use m_edge_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
      use set_element_id_4_node
!
!
      call set_edge_id_4_node
      call belonged_edge_id_4_node_1(blng_tbls%host_edge)
      call const_ele_comm_table_1st(numnod, numedge, inod_global,       &
     &    interior_edge, x_edge, edge_4_nod, blng_tbls%host_edge,       &
     &    edge_comm)
      call dealloc_iele_belonged_type(blng_tbls%host_edge)
      call dealloc_iele_belonged_type(edge_4_nod)
!
      end subroutine const_edge_comm_table_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_table_1st(numnod, numele, inod_global,  &
     &          internal_flag, x_ele, neib_e, host, e_comm)
!
      use m_nod_comm_table
      use t_comm_table
      use t_next_node_ele_4_node
      use const_element_comm_table
      use const_global_element_ids
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(element_around_node), intent(in) :: host
      type(element_around_node), intent(in) :: neib_e
!
      type(communication_table), intent(inout) :: e_comm
!
!
      e_comm%num_neib = num_neib
      call allocate_type_neib_id(e_comm)
      call allocate_type_import_num(e_comm)
!
      call count_element_import_num(numnod, host%istack_4_node,         &
     &    num_neib, id_neib, istack_import, item_import,                &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
!
      call allocate_element_rev_imports(e_comm%ntot_import)
      call allocate_type_import_item(e_comm)
!
!
      call set_element_import_item(numnod, numele,                      &
     &    inod_global, x_ele, host%istack_4_node, host%iele_4_node,     &
     &    num_neib, istack_import, item_import,                         &
     &    e_comm%num_neib, e_comm%istack_import, e_comm%item_import)
!
      call allocate_type_export_num(e_comm)
!
      call element_num_reverse_SR(e_comm%num_neib, e_comm%id_neib,      &
     &    e_comm%num_import, e_comm%num_export, e_comm%istack_export,   &
     &    e_comm%ntot_export)
!
      call allocate_element_rev_exports(e_comm%ntot_export)
      call allocate_type_export_item(e_comm)
!
      call element_position_reverse_SR(e_comm%num_neib, e_comm%id_neib, &
     &          e_comm%istack_import, e_comm%istack_export)
!
      call set_eleent_export_item(numnod, numele, inod_global,          &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, num_neib, istack_export, item_export,     &
     &    e_comm%num_neib, e_comm%istack_export, e_comm%item_export)
!
      call deallocate_element_rev_list
!
      call check_element_position                                       &
     &   (numele, x_ele, e_comm%num_neib, e_comm%id_neib,               &
     &    e_comm%istack_import, e_comm%item_import,                     &
     &    e_comm%istack_export, e_comm%item_export)
!
      end subroutine const_ele_comm_table_1st
!
!-----------------------------------------------------------------------
!
      end module m_ele_sf_eg_comm_tables
