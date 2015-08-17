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
!!
!!      subroutine copy_ele_comm_tbl_from_type(org_ele_comm)
!!      subroutine copy_ele_comm_tbl_to_type(new_ele_comm)
!!
!!      subroutine copy_surf_comm_tbl_from_type(org_surf_comm)
!!      subroutine copy_surf_comm_tbl_to_type(new_surf_comm)
!!
!!      subroutine copy_edge_comm_tbl_from_type(org_edge_comm)
!!      subroutine copy_edge_comm_tbl_to_type(new_edge_comm)
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
      use m_geometry_data
      use const_global_element_ids
!
!
      call allocate_numnod_stack(nprocs)
!
      call count_number_of_node_stack(node1%numnod, istack_numnod)
      call count_number_of_node_stack                                   &
     &   (node1%internal_node, istack_internod)
!
      end subroutine const_global_numnod_list_1st
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_element_id_1st
!
      use m_geometry_data
      use const_global_element_ids
!
      character(len=kchara), parameter :: txt = 'element'
!
      call allocate_numele_stack(nprocs)
!
      call count_number_of_node_stack(ele1%numele, istack_numele)
      call count_number_of_node_stack                                   &
     &   (ele1%internal_ele, istack_interele)
!
      call set_global_ele_id(txt, ele1%numele, istack_interele,         &
     &         interior_ele, ele_comm, ele1%iele_global)
!
      end subroutine const_global_element_id_1st
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_surface_id_1st
!
      use m_geometry_data
      use const_global_element_ids
!
      character(len=kchara), parameter :: txt = 'surface'
!
      call allocate_numsurf_stack(nprocs)
!
      call count_number_of_node_stack(surf1%numsurf, istack_numsurf)
      call count_number_of_node_stack                                   &
     &   (surf1%internal_surf, istack_intersurf)
!
      call set_global_ele_id(txt, surf1%numsurf, istack_intersurf,      &
     &    surf1%interior_surf, surf_comm, surf1%isurf_global)
!
      end subroutine const_global_surface_id_1st
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_edge_id_1st
!
      use m_geometry_data
      use const_global_element_ids
!
      character(len=kchara), parameter :: txt = 'edge'
!
      call allocate_numedge_stack(nprocs)
!
      call count_number_of_node_stack(edge1%numedge, istack_numedge)
      call count_number_of_node_stack                                   &
     &   (edge1%internal_edge, istack_interedge)
!
      call set_global_ele_id(txt, edge1%numedge, istack_interele,       &
     &    edge1%interior_edge, edge_comm, iedge_global)
!
      end subroutine const_global_edge_id_1st
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_table_1st
!
      use m_nod_comm_table
      use m_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call set_ele_id_4_node_comm
      call belonged_ele_id_4_node_1(blng_tbls%host_ele)
      call const_ele_comm_table_1st                                     &
     &   (txt, node1%numnod, ele1%numele, node1%inod_global,            &
     &    interior_ele, x_ele, nod_comm, ele_4_nod_comm,                &
     &    blng_tbls%host_ele, ele_comm)
      call dealloc_iele_belonged(blng_tbls%host_ele)
      call dealloc_iele_belonged(ele_4_nod_comm)
!
      end subroutine const_element_comm_table_1st
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table_1st
!
      use m_nod_comm_table
      use m_geometry_data
      use m_surface_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
!
      character(len=kchara), parameter :: txt = 'surface'
!
!
      call set_surf_id_4_node
      call belonged_surf_id_4_node_1(blng_tbls%host_surf)
      call const_ele_comm_table_1st                                     &
     &   (txt, node1%numnod, surf1%numsurf, node1%inod_global,          &
     &    surf1%interior_surf, x_surf, nod_comm, surf_4_nod1,           &
     &    blng_tbls%host_surf, surf_comm)
      call dealloc_iele_belonged(blng_tbls%host_surf)
      call dealloc_iele_belonged(surf_4_nod1)
!
      end subroutine const_surf_comm_table_1st
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table_1st
!
      use m_nod_comm_table
      use m_geometry_data
      use m_edge_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
!
      character(len=kchara), parameter :: txt = 'edge'
!
!
      call set_edge_id_4_node
      call belonged_edge_id_4_node_1(blng_tbls%host_edge)
      call const_ele_comm_table_1st                                     &
     &   (txt, node1%numnod, edge1%numedge, node1%inod_global,          &
     &    edge1%interior_edge, x_edge, nod_comm, edge_4_nod1,           &
     &    blng_tbls%host_edge,  edge_comm)
      call dealloc_iele_belonged(blng_tbls%host_edge)
      call dealloc_iele_belonged(edge_4_nod1)
!
      end subroutine const_edge_comm_table_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_table_1st                               &
     &         (txt, numnod, numele, inod_global,                       &
     &          internal_flag, x_ele, nod_comm, neib_e, host, e_comm)
!
      use t_comm_table
      use t_next_node_ele_4_node
      use const_element_comm_table
      use const_global_element_ids
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(element_around_node), intent(in) :: host
      type(element_around_node), intent(in) :: neib_e
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: e_comm
!
!
      e_comm%num_neib = nod_comm%num_neib
      call allocate_type_neib_id(e_comm)
      call allocate_type_import_num(e_comm)
!
      call count_element_import_num(numnod, host%istack_4_node,         &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
!
      call allocate_element_rev_imports(e_comm%ntot_import)
      call allocate_type_import_item(e_comm)
!
!
      call set_element_import_item(numnod, numele, inod_global, x_ele,  &
     &    host%istack_4_node, host%iele_4_node, nod_comm%num_neib,      &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
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
     &    e_comm%istack_import, e_comm%istack_export)
!
      call set_element_export_item(txt, numnod, numele, inod_global,    &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, nod_comm%num_neib,                        &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export, e_comm%item_export)
!
      call deallocate_element_rev_list
!
      call check_element_position(txt, numele, x_ele, e_comm)
!
      end subroutine const_ele_comm_table_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_ele_comm_tbl_from_type(org_ele_comm)
!
      type(communication_table), intent(in) :: org_ele_comm
!
!
      call copy_comm_tbl_types(org_ele_comm, ele_comm)
!
      end subroutine copy_ele_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_ele_comm_tbl_to_type(new_ele_comm)
!
      type(communication_table), intent(inout) :: new_ele_comm
!
!
      call copy_comm_tbl_types(ele_comm, new_ele_comm)
!
      end subroutine copy_ele_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_surf_comm_tbl_from_type(org_surf_comm)
!
      type(communication_table), intent(in) :: org_surf_comm
!
!
      call copy_comm_tbl_types(org_surf_comm, surf_comm)
!
      end subroutine copy_surf_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_surf_comm_tbl_to_type(new_surf_comm)
!
      type(communication_table), intent(inout) :: new_surf_comm
!
!
      call copy_comm_tbl_types(surf_comm, new_surf_comm)
!
      end subroutine copy_surf_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_edge_comm_tbl_from_type(org_edge_comm)
!
      type(communication_table), intent(in) :: org_edge_comm
!
!
      call copy_comm_tbl_types(org_edge_comm, edge_comm)
!
      end subroutine copy_edge_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_comm_tbl_to_type(new_edge_comm)
!
      type(communication_table), intent(inout) :: new_edge_comm
!
!
      call copy_comm_tbl_types(edge_comm, new_edge_comm)
!
      end subroutine copy_edge_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!
      end module m_ele_sf_eg_comm_tables
