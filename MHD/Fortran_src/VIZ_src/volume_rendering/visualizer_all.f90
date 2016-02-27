!visualizer_all.f90
!      module visualizer_all
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine init_visualize(mesh, group, surf, edge, edge_comm,   &
!!     &          nod_fld)
!!      subroutine visualize_all                                        &
!!     &         (istep_psf, istep_iso, istep_pvr, istep_fline,         &
!!     &          mesh, group, surf, edge, edge_comm,                   &
!!     &          nod_fld, ele_4_nod, jac_3d)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(jacobians_3d), intent(in) :: jac_3d
!
      module visualizer_all
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize(mesh, group, surf, edge, edge_comm,     &
     &          nod_fld)
!
      use m_cross_section
      use m_isosurface
      use set_psf_case_table
      use volume_rendering
      use fieldline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
!
      type(phys_data), intent(in) :: nod_fld
!
!
      if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
      call set_sectioning_case_table
!
      call SECTIONING_initialize                                        &
     &   (mesh%node, mesh%ele, surf, edge, mesh%nod_comm, edge_comm,    &
     &    group%ele_grp, group%surf_grp, group%surf_nod_grp, nod_fld)
!
      call ISOSURF_initialize                                           &
     &   (mesh%node, mesh%ele, surf, edge, group%ele_grp, nod_fld)
!
      call PVR_initialize                                               &
     &   (mesh%node, mesh%ele, surf, group%ele_grp, nod_fld)
      call calypso_MPI_barrier
!
      call FLINE_initialize                                             &
     &   (mesh%node, mesh%ele, group%ele_grp, group%surf_grp, nod_fld)
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all                                          &
     &         (istep_psf, istep_iso, istep_pvr, istep_fline,           &
     &          mesh, group, surf, edge, edge_comm,                     &
     &          nod_fld, ele_4_nod, jac_3d)
!
      use m_cross_section
      use m_isosurface
      use volume_rendering
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
      integer(kind = kint), intent(in) :: istep_pvr, istep_fline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(communication_table), intent(in) :: edge_comm
!
      type(phys_data), intent(in) :: nod_fld
      type(element_around_node), intent(in) :: ele_4_nod
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      call SECTIONING_visualize(istep_psf, edge, nod_fld)
!
      call ISOSURF_visualize(istep_iso, mesh%node, mesh%ele, edge,      &
      &                      edge_comm, nod_fld)
!
      call PVR_visualize                                                &
     &   (istep_pvr, mesh%node, mesh%ele, surf, jac_3d, nod_fld)
!
      call FLINE_visualize(istep_fline, mesh%node, mesh%ele, surf,      &
     &    group%ele_grp, ele_4_nod, nod_fld, mesh%nod_comm)
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module visualizer_all
