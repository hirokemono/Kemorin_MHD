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
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
!
      use fieldline
!
!
      call field_line_init                                              &
     &   (node1%numnod, ele1%numele, ele1%interior_ele,                 &
     &    ele_grp1, sf_grp1, nod_fld1%num_phys, phys_nod_name)
!
      end subroutine field_line_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main_1st(istep_psf)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_element_id_4_node
      use m_node_phys_data
!
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_psf
!
!
      call field_line_main                                              &
     &   (istep_psf, node1%numnod, ele1%numele, surf1%numsurf,          &
     &    surf1%nnod_4_surf, node1%istack_nod_smp, node1%inod_global,   &
     &    node1%xx, node1%rr, node1%a_r, node1%ss, node1%a_s,           &
     &    ele1%iele_global, ele1%interior_ele, surf1%ie_surf,           &
     &    surf1%isf_4_ele, surf1%iele_4_surf, surf1%x_surf,             &
     &    surf1%vnorm_surf, surf1%area_surf, surf1%interior_surf,       &
     &    ele_grp1, ele_4_nod1, nod_fld1%num_phys, num_tot_nod_phys,    &
     &    istack_nod_component, d_nod, nod_comm)
!
      end subroutine field_line_main_1st
!
!  ---------------------------------------------------------------------
!
      end module fieldline_1st
