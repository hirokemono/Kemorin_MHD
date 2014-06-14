!
!      module set_geometry_to_types
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine set_mesh_data_2_type(femmesh)
!        type(mesh_data), intent(inout) :: femmesh
!
!      subroutine set_nod_comm_tbl_2_type(nod_comm)
!        type(communication_table), intent(inout) :: nod_comm
!      subroutine set_nod_geometry_2_type(nod)
!        type(node_data), intent(inout) :: nod
!
!
      module set_geometry_to_types
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_2_type(femmesh)
!
      use t_mesh_data
      use link_data_type_to_1st_mesh
!
      type(mesh_data), intent(inout) :: femmesh
!
      call set_nod_comm_tbl_2_type(femmesh%mesh%nod_comm)
      call set_nod_geometry_2_type(femmesh%mesh%node)
!
      call link_element_data_type(femmesh%mesh%ele)
      call link_ele_geometry_type(femmesh%mesh%ele)
!
      end subroutine set_mesh_data_2_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nod_comm_tbl_2_type(nod_comm)
!
      use t_comm_table
      use m_nod_comm_table
!
      type(communication_table), intent(inout) :: nod_comm
!
      nod_comm%num_neib =    num_neib
      nod_comm%ntot_import = ntot_import
      nod_comm%ntot_export = ntot_export
!
      nod_comm%id_neib =>        id_neib
      nod_comm%num_import =>     num_import
      nod_comm%istack_import =>  istack_import
      nod_comm%item_import =>    item_import
      nod_comm%num_export =>     num_export
      nod_comm%istack_export =>  istack_export
      nod_comm%item_export =>    item_export
!
      end subroutine set_nod_comm_tbl_2_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_geometry_2_type(nod)
!
      use t_geometry_data
      use m_geometry_parameter
      use m_geometry_data
      use link_data_type_to_1st_mesh
!
      type(node_data), intent(inout) :: nod
!
!
      call link_node_data_type(nod)
      nod%max_nod_smp =          maxnod_4_smp
      nod%max_internal_nod_smp = max_in_nod_4_smp
!
      nod%istack_nod_smp =>      inod_smp_stack
      nod%istack_internal_smp => inter_smp_stack
!
      end subroutine set_nod_geometry_2_type
!
!  ---------------------------------------------------------------------
!
      end module set_geometry_to_types
