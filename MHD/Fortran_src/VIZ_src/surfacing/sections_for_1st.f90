!sections_for_1st.f90
!      module sections_for_1st
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine init_visualize_surface(mesh, group, surf,            &
!!     &          edge, edge_comm, sf_grp_nod, nod_fld)
!!      subroutine visualize_surface(istep_psf, istep_iso,              &
!!     &          mesh, edge, edge_comm, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(in) :: edge_comm
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(phys_data), intent(in) :: nod_fld
!
      module sections_for_1st
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
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_surface(mesh, group, surf,              &
     &          edge, edge_comm, sf_grp_nod, nod_fld)
!
      use m_cross_section
      use m_isosurface
!
      use set_psf_case_table
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(communication_table), intent(in) :: edge_comm
!
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      type(phys_data), intent(in) :: nod_fld
!
!
      if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
      call set_sectioning_case_table
!
      call SECTIONING_initialize                                        &
     &   (mesh%node, mesh%ele, surf, edge, mesh%nod_comm, edge_comm,    &
     &    group%ele_grp, group%surf_grp, sf_grp_nod, nod_fld)
!
      call ISOSURF_initialize(mesh%node, mesh%ele, surf, edge,          &
     &    group%ele_grp, nod_fld)
!
      end subroutine init_visualize_surface
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_surface(istep_psf, istep_iso,                &
     &          mesh, edge, edge_comm, nod_fld)
!
      use m_cross_section
      use m_isosurface
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
!
      call SECTIONING_visualize(istep_psf, edge, nod_fld)
!
      call ISOSURF_visualize                                            &
     &   (istep_iso, mesh%node, mesh%ele, edge, edge_comm, nod_fld)
!
      end subroutine visualize_surface
!
!  ---------------------------------------------------------------------
!
      end module sections_for_1st
