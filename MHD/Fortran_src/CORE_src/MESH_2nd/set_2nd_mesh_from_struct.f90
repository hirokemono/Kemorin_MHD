!set_2nd_mesh_from_struct.f90
!      module set_2nd_mesh_from_struct
!
!        programmed by H.Matsui on Aug., 2006
!        Modified by H.Matsui on June, 2007
!
!      subroutine set_2nd_mesh_from_struct(mesh_info)
!        type(mesh_data), intent(in) :: mesh_info
!
      module set_2nd_mesh_from_struct
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_2nd_mesh_from_struct(mesh_info)
!
      use t_mesh_data
      use m_2nd_geometry_data
      use link_2nd_comm_tbl_type
      use link_2nd_group_type
      use link_geometry_to_mesh_type
!
      type(mesh_data), intent(in) :: mesh_info
!
!
      call link_2nd_nod_comm_tbl_type(mesh_info%mesh%nod_comm)
      call link_2nd_node_type(mesh_info%mesh%node)
      call link_2nd_ele_connect_type(mesh_info%mesh%ele)
      call s_link_2nd_group_type(mesh_info%group)
!
      end subroutine s_set_2nd_mesh_from_struct
!
!  ---------------------------------------------------------------------
!
      end module set_2nd_mesh_from_struct
