!t_mesh_data.f90
!      module t_mesh_data
!
!> @brief Structure for mesh data
!
!>   including geometry, connectivity, and groups
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine link_mesh_data_type(org_mesh, new_mesh)
!      subroutine link_groups_type(org_group, new_group)
!
!!      subroutine dealloc_mesh_infomations(nod_comm,                   &
!!     &          node, ele, surf, edge, nod_grp, ele_grp, surf_grp,    &
!!     &          tbls_ele_grp, tbls_sf_grp, surf_nod_grp)
!!      subroutine dealloc_nod_ele_infos(node, ele, surf, edge,         &
!!     &          nod_grp, ele_grp, surf_grp, nod_comm)
!!      subroutine dealloc_mesh_infos                                   &
!!     &         (nod_comm, node, ele, nod_grp, ele_grp, surf_grp)
!!
!      subroutine dealloc_base_mesh_type_info(femmesh)
!      type(mesh_data), intent(inout) :: femmesh
!
!      subroutine dealloc_mesh_data_type(femmesh)
!      subroutine dealloc_mesh_type(mesh)
!      subroutine dealloc_groups_data(group)
!
!      subroutine dealloc_ele_surf_edge_type                            &
!     &          (ele_mesh, surf_mesh, edge_mesh)
!      subroutine dealloc_surf_mesh_type(surf_mesh)
!      subroutine dealloc_edge_mesh_type(my_rank, mesh)
!
      module t_mesh_data
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_data
      use t_edge_data
      use t_surface_group_geometry
      use t_group_connects
      use t_surface_boundary
!
      implicit  none
!
!
!>     Structure for grid data
!>        (position, connectivity, and communication)
      type mesh_geometry
!>     Structure for node communication
        type(communication_table) :: nod_comm
!>     Structure for node position
        type(node_data) ::           node
!>     Structure for element position and connectivity
        type(element_data) ::        ele
      end type mesh_geometry
!
!>     Structure for group data (node, element, surface, and infinity)
      type mesh_groups
!>     Structure for node group
        type (group_data) ::             nod_grp
!>     Structure for element group
        type (group_data) ::             ele_grp
!>     Structure for surface group
        type (surface_group_data) ::     surf_grp
!
!>     Structure for node data on surface group
        type (surface_node_grp_data) ::   surf_nod_grp
!>     Structure for grometry data for surface group
        type (surface_group_geometry) ::  surf_grp_geom
!>     Structure for work area  for surface integration
        type (surf_grp_geom_4_fem_int) :: surf_grp_int
!
!>     Structure for element group connectivity
        type (element_group_table) ::    tbls_ele_grp
!>     Structure for surface group connectivity
        type (surface_group_table) ::    tbls_surf_grp
!
!>     Structure for infinity surface
        type (scalar_surf_BC_list) ::    infty_grp
      end type mesh_groups
!
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type mesh_data
!>     Structure for grid data
        type(mesh_geometry) :: mesh
!>     Structure for group data
        type(mesh_groups) ::   group
      end type mesh_data
!
!
!
!>     Structure for element data (communication)
      type element_comms
!>     Structure for element communication
        type(communication_table) :: ele_comm
      end type element_comms
!
!>     Structure for surface data
!>        (position, connectivity, and communication)
      type surface_geometry
!>     Structure for surface communication
        type(communication_table) :: surf_comm
!>     Structure for surface position and connectivity
        type(surface_data) ::        surf
      end type surface_geometry
!
!>     Structure for edge data
!>        (position, connectivity, and communication)
      type edge_geometry
!>     Structure for edge communication
        type(communication_table) :: edge_comm
!>     Structure for edge position and connectivity
        type(edge_data) ::           edge
      end type edge_geometry
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine link_mesh_data_type(org_mesh, new_mesh)
!
      type(mesh_geometry), intent(in) :: org_mesh
      type(mesh_geometry), intent(inout) :: new_mesh
!
!
      call link_comm_tbl_types(org_mesh%nod_comm, new_mesh%nod_comm)
      call link_new_nod_geometry_type(org_mesh%node, new_mesh%node)
      call link_new_ele_connect_type(org_mesh%ele, new_mesh%ele)
!
      end subroutine link_mesh_data_type
!
!------------------------------------------------------------------
!
      subroutine link_groups_type(org_group, new_group)
!
      type(mesh_groups), intent(in) :: org_group
      type(mesh_groups), intent(inout) :: new_group
!
      call link_group_type(org_group%nod_grp, new_group%nod_grp)
      call link_group_type(org_group%ele_grp, new_group%ele_grp)
      call link_surf_group_type(org_group%surf_grp, new_group%surf_grp)
!
      end subroutine link_groups_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_mesh_infomations(nod_comm,                     &
     &          node, ele, surf, edge, nod_grp, ele_grp, surf_grp,      &
     &          tbls_ele_grp, tbls_sf_grp, surf_nod_grp)
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data),    intent(inout) :: edge
!
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
      type(element_group_table), intent(inout) :: tbls_ele_grp
      type(surface_group_table), intent(inout) :: tbls_sf_grp
      type(surface_node_grp_data), intent(inout) :: surf_nod_grp
!
!
      call dealloc_grp_connect(tbls_sf_grp%edge)
      call dealloc_surf_item_sf_grp_type(tbls_sf_grp)
      call dealloc_num_surf_grp_nod_smp(surf_nod_grp)
      call dealloc_surf_grp_nod(surf_nod_grp)
!
      call dealloc_grp_connect(tbls_ele_grp%surf)
      call dealloc_grp_connect(tbls_ele_grp%edge)
      call dealloc_grp_connect(tbls_ele_grp%node)
!
!
      call deallocate_surface_geom_type(surf)
      call deallocate_edge_geom_type(edge)
!
!      call deallocate_iso_surface_type(surf)
!      call deallocate_ext_surface_type(surf)
!
      call deallocate_edge_param_smp_type(edge)
      call deallocate_surf_param_smp_type(surf)
!
      call deallocate_edge_4_ele_type(edge)
      call deallocate_edge_connect_type(edge)
      call deallocate_surface_connect_type(surf)
      call dealloc_ele_4_surf_type(surf)
!
      call dealloc_nod_ele_infos(nod_comm, node, ele, surf, edge,       &
     &    nod_grp, ele_grp, surf_grp)
!
      end subroutine dealloc_mesh_infomations
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_nod_ele_infos(nod_comm,                        &
     &          node, ele, surf, edge, nod_grp, ele_grp, surf_grp)
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data),    intent(inout) :: edge
!
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call deallocate_sf_grp_type_smp(surf_grp)
      call deallocate_grp_type_smp(ele_grp)
      call deallocate_grp_type_smp(nod_grp)
!
      call deallocate_inod_in_edge_type(edge)
      call deallocate_inod_in_surf_type(surf)
      call deallocate_ele_param_smp_type(ele)
      call deallocate_node_param_smp_type(node)
!
      call deallocate_ele_geometry_type(ele)
!
      call dealloc_mesh_infos                                           &
     &   (nod_comm, node, ele, nod_grp, ele_grp, surf_grp)
!
      end subroutine dealloc_nod_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mesh_infos                                     &
     &         (nod_comm, node, ele, nod_grp, ele_grp, surf_grp)
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
      call deallocate_ele_connect_type(ele)
      call deallocate_node_geometry_type(node)
      call deallocate_type_comm_tbl(nod_comm)
!
      call deallocate_grp_type(nod_grp)
      call deallocate_grp_type(ele_grp)
      call deallocate_sf_grp_type(surf_grp)
!
      end subroutine dealloc_mesh_infos
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_base_mesh_type_info(femmesh)
!
      type(mesh_data), intent(inout) :: femmesh
!
!
      call deallocate_ele_geometry_type(femmesh%mesh%ele)
      call deallocate_ele_param_smp_type(femmesh%mesh%ele)
      call deallocate_node_param_smp_type(femmesh%mesh%node)
!
      call dealloc_mesh_data_type(femmesh)
!
      end subroutine dealloc_base_mesh_type_info
!
!------------------------------------------------------------------
!
      subroutine dealloc_groups_data(group)
!
      type(mesh_groups), intent(inout) :: group
!
!
!      call deallocate_grp_type_smp(group%nod_grp)
!      call deallocate_grp_type_smp(group%ele_grp)
!      call deallocate_sf_grp_type_smp(group%surf_grp)
!
      call deallocate_grp_type(group%nod_grp)
      call deallocate_grp_type(group%ele_grp)
      call deallocate_sf_grp_type(group%surf_grp)
!
      end subroutine dealloc_groups_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_mesh_data_type(femmesh)
!
      type(mesh_data), intent(inout) :: femmesh
!
!
      call dealloc_groups_data(femmesh%group)
      call dealloc_mesh_type(femmesh%mesh)
!
      end subroutine dealloc_mesh_data_type
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_mesh_type(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
      call deallocate_type_comm_tbl(mesh%nod_comm)
!
      end subroutine dealloc_mesh_type
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_surf_edge_type                            &
     &          (ele_mesh, surf_mesh, edge_mesh)
!
      type(element_comms), intent(inout) :: ele_mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry), intent(inout) ::    edge_mesh
!
!
      call deallocate_type_comm_tbl(ele_mesh%ele_comm)
!
      call dealloc_surf_mesh_type(surf_mesh)
      call dealloc_edge_mesh_type(edge_mesh)
!
      end subroutine dealloc_ele_surf_edge_type
!
!------------------------------------------------------------------
!
      subroutine dealloc_surf_mesh_type(surf_mesh)
!
      type(surface_geometry), intent(inout) :: surf_mesh
!
!
      call deallocate_type_comm_tbl(surf_mesh%surf_comm)
      call deallocate_surface_connect_type(surf_mesh%surf)
!
      end subroutine dealloc_surf_mesh_type
!
!------------------------------------------------------------------
!
      subroutine dealloc_edge_mesh_type(edge_mesh)
!
      type(edge_geometry), intent(inout) :: edge_mesh
!
!
      call deallocate_type_comm_tbl(edge_mesh%edge_comm)
      call deallocate_edge_connect_type(edge_mesh%edge)
      call deallocate_edge_4_ele_type(edge_mesh%edge)
!
      end subroutine dealloc_edge_mesh_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_smp_size_type(my_rank, mesh)
!
      type(mesh_geometry) :: mesh
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &        'mesh%node%istack_nod_smp: ', mesh%node%istack_nod_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &        'mesh%node%istack_nod_smp: ', mesh%node%istack_nod_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &        'mesh%ele%istack_ele_smp: ', mesh%ele%istack_ele_smp
!
      end subroutine check_smp_size_type
!
!-----------------------------------------------------------------------
!
      subroutine check_smp_size_surf_edge_type(surf_mesh, edge_mesh)
!
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry), intent(inout) ::  edge_mesh
!
!
      write(*,*) 'surf_mesh%surfistack_surf_smp ',                      &
     &           surf_mesh%surf%istack_surf_smp
      write(*,*) 'edge_mesh%edge%istack_edge_smp ',                     &
     &           edge_mesh%edge%istack_edge_smp
!
      end subroutine check_smp_size_surf_edge_type
!
!-----------------------------------------------------------------------
!
      end module t_mesh_data
