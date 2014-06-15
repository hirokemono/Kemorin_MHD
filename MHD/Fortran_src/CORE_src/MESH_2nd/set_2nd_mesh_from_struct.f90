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
      use t_group_data
      use m_2nd_geometry_data
      use m_2nd_group_data
!
      type(mesh_data), intent(in) :: mesh_info
!
!
      call link_comm_tbl_types(mesh_info%mesh%nod_comm, comm_2nd)
      call link_new_nod_geometry_type(mesh_info%mesh%node, node_2nd)
      call link_new_ele_connect_type(mesh_info%mesh%ele, ele_2nd)
!
      call link_group_type(mesh_info%group%nod_grp, nod_grp_2nd)
      call link_group_type(mesh_info%group%ele_grp, ele_grp_2nd)
      call link_surf_group_type(mesh_info%group%surf_grp, sf_grp_2nd)
!
      end subroutine s_set_2nd_mesh_from_struct
!
!  ---------------------------------------------------------------------
!
      end module set_2nd_mesh_from_struct
