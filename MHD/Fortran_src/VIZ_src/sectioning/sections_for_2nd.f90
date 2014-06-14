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
      use m_2nd_geometry_data
      use m_2nd_group_data
      use m_2nd_surf_group_data
      use m_2nd_phys_data
!
      use cross_section
!
!
      call cross_section_init(node_2nd%numnod, ele_2nd%numele, surf_2nd%numsurf,  &
     &   edge_2nd%numedge, ele_2nd%nnod_4_ele, edge_2nd%nnod_4_edge,    &
     &   ele_2nd%ie, edge_2nd%ie_edge,     &
     &   surf_2nd%isf_4_ele, edge_2nd%iedge_4_sf, edge_2nd%iedge_4_ele, &
     &   ele_2nd%interior_ele, node_2nd%inod_global, node_2nd%xx,    &
     &   node_2nd%istack_nod_smp, ele_2nd%istack_ele_smp,                        &
     &   surf_2nd%istack_surf_smp, edge_2nd%istack_edge_smp, num_mat_2nd,    &
     &   num_mat_bc_2nd,  mat_name_2nd, mat_istack_2nd,  mat_item_2nd,  &
     &   num_surf_2nd, num_surf_bc_2nd, surf_name_2nd, surf_istack_2nd, &
     &   surf_item_2nd, ntot_node_sf_grp_2nd, inod_stack_sf_grp_2nd,    &
     &   inod_surf_grp_2nd, phys_2nd%num_phys, phys_2nd%phys_name)
!
      end subroutine cross_section_init_2nd
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init_2nd
!
      use m_2nd_geometry_data
      use m_2nd_group_data
      use m_2nd_phys_data
!
      use isosurface
!
      call isosurface_init(node_2nd%numnod, ele_2nd%numele,           &
     &   surf_2nd%numsurf, edge_2nd%numedge, edge_2nd%nnod_4_edge,      &
     &    edge_2nd%ie_edge, surf_2nd%isf_4_ele, edge_2nd%iedge_4_sf, &
     &   ele_2nd%interior_ele, node_2nd%istack_nod_smp, ele_2nd%istack_ele_smp,  &
     &   surf_2nd%istack_surf_smp, edge_2nd%istack_edge_smp, num_mat_2nd,    &
     &    num_mat_bc_2nd,  mat_name_2nd, mat_istack_2nd,  mat_item_2nd, &
     &   phys_2nd%num_phys, phys_2nd%phys_name)
!
      end subroutine isosurface_init_2nd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main_2nd(istep_psf)
!
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      use cross_section
!
      integer(kind = kint), intent(in) :: istep_psf
!
!
      call cross_section_main(istep_psf, node_2nd%numnod, edge_2nd%numedge,    &
     &    edge_2nd%nnod_4_edge, edge_2nd%ie_edge, phys_2nd%num_phys,         &
     &    phys_2nd%ntot_phys, phys_2nd%istack_component,                &
     &    phys_2nd%d_fld)
!
      end subroutine cross_section_main_2nd
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main_2nd(istep_iso)
!
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      use isosurface
!
      integer(kind = kint), intent(in) :: istep_iso
!
      call isosurface_main(istep_iso,                                   &
     &    node_2nd%numnod, ele_2nd%numele, edge_2nd%numedge, ele_2nd%nnod_4_ele,     &
     &    edge_2nd%nnod_4_edge, ele_2nd%ie, edge_2nd%ie_edge,   &
     &    edge_2nd%iedge_4_ele, node_2nd%inod_global, node_2nd%xx, node_2nd%rr, &
     &    node_2nd%a_r, node_2nd%ss, node_2nd%a_s, node_2nd%istack_nod_smp,  &
     &    phys_2nd%num_phys, phys_2nd%ntot_phys,                        &
     &    phys_2nd%istack_component, phys_2nd%d_fld)
!
      end subroutine isosurface_main_2nd
!
!  ---------------------------------------------------------------------
!
      end module sections_for_2nd
