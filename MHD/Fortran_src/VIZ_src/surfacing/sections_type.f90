!
!      module sections_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_sections_type(                                   &
!     &         (fem, sf_mesh_psf, eg_mesh_psf, fld_nod)
!      subroutine sectioning_type(istep_psf, istep_iso,                 &
!     &          fem, eg_mesh_psf, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_geometry), intent(in) :: sf_mesh_psf
!        type(edge_geometry), intent(in) :: eg_mesh_psf
!        type(phys_data), intent(in) :: fld_nod
!
!      subroutine cross_section_init_type(fem, surf, edge_mesh, fld_nod)
!      subroutine isosurface_init_type(fem, surf, edge, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_data), intent(in) :: surf
!        type(edge_data), intent(in) :: edge
!        type(phys_data), intent(in) :: fld_nod
!
!!      subroutine isosurface_main_type                                 &
!!     &         (istep_iso, fem, edge_mesh, fld_nod)
!        integer(kind = kint), intent(in) :: istep_psf
!        integer(kind = kint), intent(in) :: istep_iso
!        type(mesh_data), intent(in) :: fem
!        type(edge_data), intent(in) :: edge
!        type(phys_data), intent(in) :: fld_nod
!
      module sections_type
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
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
      subroutine init_sections_type                                     &
     &         (fem, sf_mesh_psf, eg_mesh_psf, fld_nod)
!
      use m_control_data_sections
      use m_cross_section
      use m_isosurface
      use m_quad_2_triangle
!
      use set_psf_case_table
!
      type(mesh_data), intent(inout) :: fem
      type(surface_geometry), intent(inout) :: sf_mesh_psf
      type(edge_geometry), intent(inout) :: eg_mesh_psf
      type(phys_data), intent(inout) :: fld_nod
!
!
      if ( (num_psf_ctl+num_iso_ctl) .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
        call set_sectioning_case_table
      end if
!
      num_psf = num_psf_ctl
      if (num_psf .gt. 0)  then
        call cross_section_init_type(fem,                               &
     &      sf_mesh_psf%surf, eg_mesh_psf, fld_nod)
      end if
!
      num_iso = num_iso_ctl
      if (num_iso .gt. 0) then
        call isosurface_init_type(fem,                                  &
     &      sf_mesh_psf%surf, eg_mesh_psf%edge, fld_nod)
      end if
!
      end subroutine init_sections_type
!
!  ---------------------------------------------------------------------
!
      subroutine sectioning_type(istep_psf, istep_iso,                  &
     &          fem, eg_mesh_psf, fld_nod)
!
      use m_cross_section
      use m_isosurface
!
      type(mesh_data), intent(in) :: fem
      type(edge_geometry), intent(in) :: eg_mesh_psf
      type(phys_data), intent(in) :: fld_nod
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
!
!
      if (num_psf.gt.0 .and. istep_psf.gt.0) then
        call cross_section_main(istep_psf, eg_mesh_psf%edge, fld_nod)
      end if
      if (num_iso.gt.0 .and. istep_iso.gt.0) then
        call isosurface_main_type(istep_psf, fem,                       &
     &      eg_mesh_psf, fld_nod)
      end if
!
      end subroutine sectioning_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init_type(fem, surf, edge_mesh, fld_nod)
!
      use t_surface_data
      use m_cross_section
!
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(edge_geometry), intent(in) :: edge_mesh
      type(phys_data), intent(in) :: fld_nod
!
!
      call cross_section_init                                           &
     &   (fem%mesh%node, fem%mesh%ele, surf, edge_mesh%edge,            &
     &    fem%mesh%nod_comm, edge_mesh%edge_comm,                       &
     &    fem%group%ele_grp, fem%group%surf_grp,                        &
     &    fem%group%surf_nod_grp, fld_nod)
!
      end subroutine cross_section_init_type
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init_type(fem, surf, edge, fld_nod)
!
      use t_surface_data
      use t_edge_data
      use m_isosurface
!
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: fld_nod
!
!
      call isosurface_init                                              &
     &   (fem%mesh%node, fem%mesh%ele,                                  &
     &    surf, edge, fem%group%ele_grp, fld_nod)
!
      end subroutine isosurface_init_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main_type                                   &
     &         (istep_iso, fem, edge_mesh, fld_nod)
!
      use m_isosurface
!
      integer(kind = kint), intent(in) :: istep_iso
      type(mesh_data), intent(in) :: fem
      type(edge_geometry), intent(in) :: edge_mesh
      type(phys_data), intent(in) :: fld_nod
!
      call isosurface_main(istep_iso, fem%mesh%node, fem%mesh%ele,      &
     &    edge_mesh%edge, edge_mesh%edge_comm, fld_nod)
!
      end subroutine isosurface_main_type
!
!  ---------------------------------------------------------------------
!
      end module sections_type
