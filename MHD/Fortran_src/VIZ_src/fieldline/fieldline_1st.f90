!fieldline_1st.f90
!      module fieldline_1st
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_fline(ierror)
!      subroutine visualize_fline(istep_fline, ierror)
!
!      subroutine field_line_init_1st
!      subroutine field_line_main_1st(istep_psf)
!
      module fieldline_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_fline(ierror)
!
      use m_control_data_flines
      use m_control_params_4_fline
!
      integer(kind = kint), intent(inout) :: ierror
!
!
      num_fline = num_fline_ctl
      if (num_fline .gt. 0) call field_line_init_1st
      call time_prog_barrier
!
      ierror = ierr
!
      end subroutine init_visualize_fline
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_fline(istep_fline, ierror)
!
      use m_control_params_4_fline
!
      integer(kind = kint), intent(in) :: istep_fline
      integer(kind = kint), intent(inout) :: ierror
!
!
      if (num_fline.gt.0 .and. istep_fline .gt. 0) then
        call field_line_main_1st(istep_fline)
      end if
!
      ierror = ierr
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
     &    num_mat, num_mat_bc, mat_name, mat_istack, mat_item,          &
     &    num_surf, num_surf_bc, surf_name, surf_istack, surf_item,     &
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
     &       nnod_4_surf, inod_smp_stack, globalnodid,                  &
     &       xx, radius, a_radius, s_cylinder, a_s_cylinder,            &
     &       globalelmid, e_multi, ie_surf, isf_4_ele, iele_4_surf,     &
     &       x_surf, vnorm_surf, area_surf, interior_surf,              &
     &       num_mat, num_mat_bc, mat_istack,  mat_item,                &
     &       ntot_ele_4_node, iele_stack_4_node, iele_4_node,           &
     &       num_neib, ntot_import, ntot_export, id_neib,               &
     &       istack_import, istack_export, item_import, item_export,    &
     &       num_nod_phys, num_tot_nod_phys, istack_nod_component,      &
     &       d_nod)
!
      end subroutine field_line_main_1st
!
!  ---------------------------------------------------------------------
!
      end module fieldline_1st
