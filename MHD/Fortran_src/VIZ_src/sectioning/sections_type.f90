!
!      module sections_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_sections_type(ierror,                            &
!     &          fem, sf_mesh_psf, eg_mesh_psf, fld_nod)
!      subroutine sectioning_type(istep_psf, istep_iso, ierror,         &
!     &          fem, eg_mesh_psf, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_geometry), intent(in) :: sf_mesh_psf
!        type(edge_geometry), intent(in) :: eg_mesh_psf
!        type(phys_data), intent(in) :: fld_nod
!
!      subroutine cross_section_init_type(fem, surf, edge, fld_nod)
!      subroutine isosurface_init_type(fem, surf, edge, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_data), intent(in) :: surf
!        type(edge_data), intent(in) :: edge
!        type(phys_data), intent(in) :: fld_nod
!
!      subroutine cross_section_main_type(istep_psf, fem, edge,         &
!     &          fld_nod)
!      subroutine isosurface_main_type(istep_iso, fem, edge, fld_nod)
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
      use m_parallel_var_dof
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
      subroutine init_sections_type(ierror,                             &
     &          fem, sf_mesh_psf, eg_mesh_psf, fld_nod)
!
      use m_control_data_sections
      use m_control_params_4_psf
      use m_control_params_4_iso
!
      use m_quad_2_triangle
!
      use const_linear_mesh_type
      use set_psf_case_table
!
      type(mesh_data), intent(inout) :: fem
      type(surface_geometry), intent(inout) :: sf_mesh_psf
      type(edge_geometry), intent(inout) :: eg_mesh_psf
      type(phys_data), intent(inout) :: fld_nod
!
      integer(kind = kint), intent(inout) :: ierror
!
!
      if ( (num_psf_ctl+num_iso_ctl) .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
        call set_sectioning_case_table
        if (iflag_debug.eq.1)  write(*,*) 's_const_linear_mesh_type'
        call s_const_linear_mesh_type(fem, sf_mesh_psf,                 &
     &      eg_mesh_psf, fld_nod)
      end if
!
      num_psf = num_psf_ctl
      if (num_psf .gt. 0)  then
        call cross_section_init_type(fem,                               &
     &      sf_mesh_psf%surf, eg_mesh_psf%edge, fld_nod)
      end if
      call time_prog_barrier
!
      num_iso = num_iso_ctl
      if (num_iso .gt. 0) then
        call isosurface_init_type(fem,                                  &
     &      sf_mesh_psf%surf, eg_mesh_psf%edge, fld_nod)
      end if
      call time_prog_barrier
!
      ierror = ierr
!
      end subroutine init_sections_type
!
!  ---------------------------------------------------------------------
!
      subroutine sectioning_type(istep_psf, istep_iso, ierror,          &
     &          fem, eg_mesh_psf, fld_nod)
!
      use m_control_params_4_psf
      use m_control_params_4_iso
!
      use const_linear_mesh_type
!
      type(mesh_data), intent(in) :: fem
      type(edge_geometry), intent(in) :: eg_mesh_psf
      type(phys_data), intent(in) :: fld_nod
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
      integer(kind = kint), intent(inout) :: ierror
!
!
!      call set_linear_phy_data_type(fem, fld_nod)
!
      if (num_psf.gt.0 .and. istep_psf.gt.0) then
        call cross_section_main_type(istep_psf, fem,                    &
     &      eg_mesh_psf%edge, fld_nod)
      end if
      if (num_iso.gt.0 .and. istep_iso.gt.0) then
        call isosurface_main_type(istep_psf, fem,                       &
     &      eg_mesh_psf%edge, fld_nod)
      end if
      ierror = ierr
!
      end subroutine sectioning_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init_type(fem, surf, edge, fld_nod)
!
      use t_surface_data
      use t_edge_data
!
      use cross_section
!
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: fld_nod
!
!
      call cross_section_init                                           &
     &   (fem%mesh%node%numnod, fem%mesh%ele%numele, surf%numsurf,      &
     &    edge%numedge, fem%mesh%ele%nnod_4_ele, edge%nnod_4_edge,      &
     &    fem%mesh%ele%ie, edge%ie_edge, surf%isf_4_ele,                &
     &    edge%iedge_4_sf, edge%iedge_4_ele, fem%mesh%ele%interior_ele, &
     &    fem%mesh%node%inod_global, fem%mesh%node%xx,                  &
     &    fem%mesh%node%istack_nod_smp, fem%mesh%ele%istack_ele_smp,    &
     &    surf%istack_surf_smp, edge%istack_edge_smp,                   &
     &    fem%group%ele_grp%num_grp, fem%group%ele_grp%num_item,        &
     &    fem%group%ele_grp%grp_name, fem%group%ele_grp%istack_grp,     &
     &    fem%group%ele_grp%item_grp, fem%group%surf_grp%num_grp,       &
     &    fem%group%surf_grp%num_item, fem%group%surf_grp%grp_name,     &
     &    fem%group%surf_grp%istack_grp,                                &
     &    fem%group%surf_grp%item_sf_grp,                               &
     &    fem%group%surf_nod_grp%ntot_node_sf_grp,                      &
     &    fem%group%surf_nod_grp%inod_stack_sf_grp,                     &
     &    fem%group%surf_nod_grp%inod_surf_grp,                         &
     &    fld_nod%num_phys, fld_nod%phys_name)
!
      end subroutine cross_section_init_type
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init_type(fem, surf, edge, fld_nod)
!
      use t_surface_data
      use t_edge_data
!
      use isosurface
!
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: fld_nod
!
!
      call isosurface_init                                              &
     &   (fem%mesh%node%numnod, fem%mesh%ele%numele, surf%numsurf,      &
     &    edge%numedge, edge%nnod_4_edge, edge%ie_edge, surf%isf_4_ele, &
     &    edge%iedge_4_sf, fem%mesh%ele%interior_ele,                   &
     &    fem%mesh%node%istack_nod_smp, fem%mesh%ele%istack_ele_smp,    &
     &    surf%istack_surf_smp, edge%istack_edge_smp,                   &
     &    fem%group%ele_grp%num_grp, fem%group%ele_grp%num_item,        &
     &    fem%group%ele_grp%grp_name, fem%group%ele_grp%istack_grp,     &
     &    fem%group%ele_grp%item_grp, fld_nod%num_phys,                 &
     &    fld_nod%phys_name)
!
      end subroutine isosurface_init_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main_type(istep_psf, fem, edge,          &
     &          fld_nod)
!
      use t_edge_data
!
      use cross_section
!
      integer(kind = kint), intent(in) :: istep_psf
      type(mesh_data), intent(in) :: fem
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: fld_nod
!
!
      call cross_section_main(istep_psf, fem%mesh%node%numnod,          &
     &    edge%numedge, edge%nnod_4_edge, edge%ie_edge,                 &
     &    fld_nod%num_phys, fld_nod%ntot_phys,                          &
     &    fld_nod%istack_component, fld_nod%d_fld)
!
      end subroutine cross_section_main_type
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main_type(istep_iso, fem, edge, fld_nod)
!
      use t_edge_data
!
      use isosurface
!
      integer(kind = kint), intent(in) :: istep_iso
      type(mesh_data), intent(in) :: fem
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: fld_nod
!
      call isosurface_main(istep_iso,                                   &
     &    fem%mesh%node%numnod, fem%mesh%ele%numele, edge%numedge,      &
     &    fem%mesh%ele%nnod_4_ele, edge%nnod_4_edge, fem%mesh%ele%ie,   &
     &    edge%ie_edge, edge%iedge_4_ele, fem%mesh%node%inod_global,    &
     &    fem%mesh%node%xx, fem%mesh%node%rr, fem%mesh%node%a_r,        &
     &    fem%mesh%node%ss, fem%mesh%node%a_s,                          &
     &    fem%mesh%node%istack_nod_smp,                                 &
     &    fld_nod%num_phys, fld_nod%ntot_phys,                          &
     &    fld_nod%istack_component, fld_nod%d_fld)
!
      end subroutine isosurface_main_type
!
!  ---------------------------------------------------------------------
!
      end module sections_type
