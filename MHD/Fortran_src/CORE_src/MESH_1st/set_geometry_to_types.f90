!
!      module set_geometry_to_types
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine set_mesh_data_2_type(femmesh)
!        type(mesh_data), intent(inout) :: femmesh
!
!      subroutine set_nod_comm_tbl_2_type(new_comm)
!        type(communication_table), intent(inout) :: new_comm
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
      subroutine set_nod_comm_tbl_2_type(new_comm)
!
      use t_comm_table
      use m_nod_comm_table
!
      type(communication_table), intent(inout) :: new_comm
!
      new_comm%num_neib =    num_neib
      new_comm%ntot_import = ntot_import
      new_comm%ntot_export = ntot_export
!
      new_comm%id_neib =>        id_neib
      new_comm%num_import =>     num_import
      new_comm%istack_import =>  istack_import
      new_comm%item_import =>    item_import
      new_comm%num_export =>     num_export
      new_comm%istack_export =>  istack_export
      new_comm%item_export =>    item_export
!
      end subroutine set_nod_comm_tbl_2_type
!
!  ---------------------------------------------------------------------
!
      end module set_geometry_to_types
