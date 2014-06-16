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
      subroutine set_new_mesh_type(mesh_info, newmesh, newgroup)
!
      use t_mesh_data
!
      type(mesh_data), intent(in) :: mesh_info
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
!
!
      call link_comm_tbl_types                                          &
     &   (mesh_info%mesh%nod_comm, newmesh%nod_comm)
      call link_new_nod_geometry_type                                   &
     &   (mesh_info%mesh%node, newmesh%node)
      call link_new_ele_connect_type                                    &
     &   (mesh_info%mesh%ele, newmesh%ele)
!
      call link_group_type                                              &
     &   (mesh_info%group%nod_grp, newgroup%nod_grp)
      call link_group_type                                              &
     &   (mesh_info%group%ele_grp, newgroup%ele_grp)
      call link_surf_group_type                                         &
     &   (mesh_info%group%surf_grp, newgroup%surf_grp)
!
      end subroutine set_new_mesh_type
!
!  ---------------------------------------------------------------------
!
      end module set_2nd_mesh_from_struct
