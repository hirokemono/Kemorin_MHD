!sections_for_2nd.f90
!      module sections_for_2nd
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_sections_2nd
!      subroutine sectioning_2nd(istep_psf, istep_iso)
!
!      subroutine cross_section_init_2nd
!      subroutine isosurface_init_2nd
!
!      subroutine cross_section_main_2nd(istep_psf)
!      subroutine isosurface_main_2nd(istep_iso)
!
      module sections_for_2nd
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_sections_2nd
!
      use m_quad_2_triangle
!
      use m_control_params_4_psf
      use m_control_params_4_iso
      use m_control_data_sections
!
      use const_linear_mesh_2nd
      use set_psf_case_table
!
!
      if ( (num_psf_ctl+num_iso_ctl) .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
        call set_sectioning_case_table
        if (iflag_debug.eq.1)  write(*,*) 'set_2nd_geometry'
        call set_2nd_geometry(my_rank)
      end if
!
      num_psf = num_psf_ctl
      if (num_psf .gt. 0)  call cross_section_init_2nd
!
      num_iso = num_iso_ctl
      if (num_iso .gt. 0) call isosurface_init_2nd
!
      end subroutine init_sections_2nd
!
!  ---------------------------------------------------------------------
!
      subroutine sectioning_2nd(istep_psf, istep_iso)
!
      use m_control_params_4_psf
      use m_control_params_4_iso
!
      use const_linear_mesh_2nd
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
!
!
      call set_linear_phys_data_2nd
!
      if (num_psf.gt.0 .and. istep_psf.gt.0) then
        call cross_section_main_2nd(istep_psf)
      end if
      if (num_iso.gt.0 .and. istep_iso.gt.0) then
        call isosurface_main_2nd(istep_iso)
      end if
!
      end subroutine sectioning_2nd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init_2nd
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_group_data
      use m_2nd_surf_group_data
      use m_2nd_phys_data
!
      use cross_section
!
!
      call cross_section_init(nnod_2nd, nele_2nd, nsurf_2nd, nedge_2nd, &
     &   nnod_4_ele_2nd, nnod_4_edge_2nd, ie_2nd, ie_edge_2nd,          &
     &   isf_4_ele_2nd, iedge_4_sf_2nd, iedge_4_ele_2nd,                &
     &   interior_ele_2nd, globalnodid_2nd, xx_2nd,                     &
     &   inod_smp_stack_2nd, iele_smp_stack_2nd,                        &
     &   isurf_smp_stack_2nd, iedge_smp_stack_2nd, num_mat_2nd,         &
     &   num_mat_bc_2nd,  mat_name_2nd, mat_istack_2nd,  mat_item_2nd,  &
     &   num_surf_2nd, num_surf_bc_2nd, surf_name_2nd, surf_istack_2nd, &
     &   surf_item_2nd, ntot_node_sf_grp_2nd, inod_stack_sf_grp_2nd,    &
     &   inod_surf_grp_2nd, num_nod_phys_2nd, phys_nod_name_2nd)
!
      end subroutine cross_section_init_2nd
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init_2nd
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_group_data
      use m_2nd_phys_data
!
      use isosurface
!
      call isosurface_init                                              &
     &   (nnod_2nd, nele_2nd, nsurf_2nd, nedge_2nd, nnod_4_edge_2nd,    &
     &    ie_edge_2nd, isf_4_ele_2nd, iedge_4_sf_2nd, interior_ele_2nd, &
     &   inod_smp_stack_2nd, iele_smp_stack_2nd,                        &
     &   isurf_smp_stack_2nd, iedge_smp_stack_2nd, num_mat_2nd,         &
     &    num_mat_bc_2nd,  mat_name_2nd, mat_istack_2nd,  mat_item_2nd, &
     &   num_nod_phys_2nd, phys_nod_name_2nd)
!
      end subroutine isosurface_init_2nd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main_2nd(istep_psf)
!
      use m_2nd_geometry_data
      use m_2nd_geometry_param
      use m_2nd_phys_data
!
      use cross_section
!
      integer(kind = kint), intent(in) :: istep_psf
!
!
      call cross_section_main(istep_psf, nnod_2nd, nedge_2nd,           &
     &    nnod_4_edge_2nd, ie_edge_2nd, num_nod_phys_2nd,               &
     &    ntot_nod_phys_2nd, istack_nod_comps_2nd, d_nod_2nd)
!
      end subroutine cross_section_main_2nd
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main_2nd(istep_iso)
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      use isosurface
!
      integer(kind = kint), intent(in) :: istep_iso
!
      call isosurface_main(istep_iso,                                   &
     &    nnod_2nd, nele_2nd, nedge_2nd, nnod_4_ele_2nd,                &
     &    nnod_4_edge_2nd, ie_2nd, ie_edge_2nd, iedge_4_ele_2nd,        &
     &    globalnodid_2nd, xx_2nd, radius_2nd, a_radius_2nd,            &
     &    s_cyl_2nd, a_s_cyl_2nd, inod_smp_stack_2nd, num_nod_phys_2nd, &
     &    ntot_nod_phys_2nd, istack_nod_comps_2nd, d_nod_2nd )
!
      end subroutine isosurface_main_2nd
!
!  ---------------------------------------------------------------------
!
      end module sections_for_2nd
