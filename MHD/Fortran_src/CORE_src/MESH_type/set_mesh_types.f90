!set_mesh_types.f90
!      module set_mesh_types
!
!        programmed by H. Matsui on Dec., 2008
!
!!      subroutine set_mesh_data_type_to_IO(my_rank, femmesh)
!!       type(mesh_data), intent(mesh_groups) :: femmesh
!!
!!      subroutine set_mesh_data_to_IO(my_rank, nod_comm, node, ele)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(communication_table), intent(inout) :: nod_comm
!!        type(node_data), intent(inout) ::           node
!!        type(element_data), intent(inout) ::        ele
!
      module set_mesh_types
!
      use m_precision
!
      use t_mesh_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_type_to_IO(my_rank, femmesh)
!
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: femmesh
!
!
      call set_mesh_data_to_IO(my_rank, femmesh%mesh%nod_comm,          &
     &   femmesh%mesh%node, femmesh%mesh%ele)
      call set_grp_data_to_IO                                           &
     &   (femmesh%group%nod_grp, femmesh%group%ele_grp, femmesh%group%surf_grp)
!
      end subroutine set_mesh_data_type_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data(nod_comm, node, ele,                     &
     &                         nod_grp, ele_grp, surf_grp)
!
      use set_group_types_4_IO
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call set_mesh_geometry_data(nod_comm, node, ele)
      call set_grp_data_from_IO(nod_grp, ele_grp, surf_grp)
!
      end subroutine set_mesh_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_geometry_data(nod_comm, node, ele)
!
      use set_comm_table_4_IO
      use set_node_data_4_IO
      use set_element_data_4_IO
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) ::           node
      type(element_data), intent(inout) ::        ele
!
!
      call copy_comm_tbl_type_from_IO(nod_comm)
!
      call copy_node_geometry_from_IO(node)
      call copy_ele_connect_from_IO(ele)
!
      call allocate_sph_node_geometry(node)
!
      end subroutine set_mesh_geometry_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_to_IO(my_rank, nod_comm, node, ele)
!
      use t_comm_table
      use t_geometry_data
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use set_node_data_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) ::           node
      type(element_data), intent(in) ::        ele
!
!
      call copy_comm_tbl_type_to_IO(my_rank, nod_comm)
      call copy_node_geometry_to_IO(node)
      call copy_ele_connect_to_IO(ele)
!
      end subroutine set_mesh_data_to_IO
!
!  ---------------------------------------------------------------------
!
      end module set_mesh_types
