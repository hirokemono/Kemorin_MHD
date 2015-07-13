!fieldline_1st.f90
!      module fieldline_1st
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_fline
!      subroutine visualize_fline(istep_fline)
!
!      subroutine field_line_init_1st
!      subroutine field_line_main_1st(istep_psf)
!
      module fieldline_1st
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
      subroutine init_visualize_fline
!
      use m_control_data_flines
      use m_control_params_4_fline
!
!
      num_fline = num_fline_ctl
      if (num_fline .gt. 0) call field_line_init_1st
!
      end subroutine init_visualize_fline
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_fline(istep_fline)
!
      use m_control_params_4_fline
!
      integer(kind = kint), intent(in) :: istep_fline
!
!
      if (num_fline.gt.0 .and. istep_fline .gt. 0) then
        call field_line_main_1st(istep_fline)
      end if
!
      end subroutine visualize_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine field_line_init_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_group
      use m_surface_group
      use m_node_phys_data
!
      use fieldline
!
!
      call field_line_init(numnod, numele, e_multi,                     &
     &    ele_grp1%num_grp, ele_grp1%num_item, ele_grp1%grp_name,       &
     &    ele_grp1%istack_grp, mat_item,          &
     &    sf_grp1%num_grp, sf_grp1%num_item, sf_grp1%grp_name,          &
     &    sf_grp1%istack_grp, sf_grp1%item_sf_grp,                      &
     &    num_nod_phys, phys_nod_name)
!
      end subroutine field_line_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main_1st(istep_psf)
!
      use m_nod_comm_table
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_geometry_data
      use m_element_group
      use m_element_id_4_node
      use m_node_phys_data
!
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_psf
!
!
      call field_line_main(istep_psf, numnod, numele, numsurf,          &
     &       nnod_4_surf, inod_smp_stack, inod_global,                  &
     &       xx, radius, a_radius, s_cylinder, a_s_cylinder,            &
     &       iele_global, e_multi, ie_surf, isf_4_ele, iele_4_surf,     &
     &       x_surf, vnorm_surf, area_surf, interior_surf,              &
     &       ele_grp1%num_grp, ele_grp1%num_item, ele_grp1%istack_grp,  &
     &       mat_item,                &
     &       ele_4_nod1%ntot, ele_4_nod1%istack_4_node,                 &
     &       ele_4_nod1%iele_4_node, num_nod_phys, num_tot_nod_phys,    &
     &       istack_nod_component, d_nod, nod_comm)
!
      end subroutine field_line_main_1st
!
!  ---------------------------------------------------------------------
!
      end module fieldline_1st
