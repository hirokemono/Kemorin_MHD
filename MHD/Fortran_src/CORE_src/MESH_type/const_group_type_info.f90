!
!      module const_group_type_info
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine s_const_group_type_info(mesh, surf_mesh,              &
!     &          edge_mesh, group)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(edge_geometry),    intent(in) :: edge_mesh
!        type(mesh_groups), intent(inout) :: group
!
      module const_group_type_info
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_group_type_info(mesh, surf_mesh,               &
     &          edge_mesh, group)
!
      use m_machine_parameter
      use t_mesh_data
      use set_tables_4_ele_grp_type
      use set_tables_4_surf_grp_type
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(edge_geometry),    intent(in) :: edge_mesh
      type(mesh_groups), intent(inout) :: group
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_grp_type'
      call set_surf_4_ele_grp_type(mesh%ele, surf_mesh%surf,            &
     &    group%ele_grp, group%tbls_ele_grp)
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_grp_type'
      call set_edge_4_ele_grp_type(mesh%ele, edge_mesh%edge,            &
     &    group%ele_grp, group%tbls_ele_grp)
!
       if (iflag_debug.eq.1) write(*,*) 'set_nod_4_ele_grp_type'
      call set_nod_4_ele_grp_type(mesh%ele, mesh%node,                  &
     &    group%ele_grp, group%tbls_ele_grp)
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_grp_type'
      call set_surf_id_4_surf_grp_type(mesh%ele, surf_mesh%surf,        &
     &    group%surf_grp, group%tbls_surf_grp)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_grp_type'
      call set_edge_4_surf_grp_type(surf_mesh%surf, edge_mesh%edge,     &
     &    group%surf_grp,  group%tbls_surf_grp)
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_surf_grp_type'
      call set_node_4_surf_grp_type(surf_mesh%surf, mesh%node,          &
     &    group%surf_grp,  group%tbls_surf_grp)
!
!
      end subroutine s_const_group_type_info
!
! ----------------------------------------------------------------------
!
      end module const_group_type_info
