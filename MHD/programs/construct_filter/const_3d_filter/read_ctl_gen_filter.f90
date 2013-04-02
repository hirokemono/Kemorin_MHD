!
!      module read_ctl_gen_filter
!
!     Written by H. Matsui on July, 2006
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine read_control_4_gen_filter
!
      module read_ctl_gen_filter
!
      use m_precision
      use m_machine_parameter
      use m_read_control_elements
      use m_ctl_data_gen_3d_filter
      use skip_comment_f
!
      implicit none
!
      private :: read_const_filter_ctl_data
      private :: read_filter_area_ctl
      private :: read_element_size_ctl, read_dx_solver_param_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_gen_filter
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_filter_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_filter_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_gen_filter
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_sort_filter
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_sort_flt_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_filter_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_sort_filter
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_const_filter_ctl_data
!
      use m_ctl_data_gen_filter
      use m_ctl_data_filter_files
      use m_ctl_data_org_filter_name
      use m_ctl_data_4_platforms
!
!
      if(right_begin_flag(hd_filter_control) .eq. 0) return
      if (i_filter_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_control, i_filter_control)
        if(i_filter_control .gt. 0) exit
!
!
        call read_ctl_data_4_platform
!
        call read_filter_param_ctl
        call read_filter_fnames_ctl
        call read_org_filter_fnames_ctl
!
        call read_filter_area_ctl
        call read_element_size_ctl
      end do
!
      end subroutine read_const_filter_ctl_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_filter_area_ctl
!
!
      if(right_begin_flag(hd_filter_area_ctl) .eq. 0) return
      if (i_filter_area_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_area_ctl,                  &
     &      i_filter_area_ctl)
        if(i_filter_area_ctl .gt. 0) exit
!
!
        call find_control_array_flag(hd_num_filter_area,                &
     &      num_filtering_grp_ctl)
        if(num_filtering_grp_ctl.gt.0                                   &
     &      .and. i_num_filter_area.eq.0) then
          call allocate_filtering_area_ctl
          call read_control_array_chara_list(hd_num_filter_area,        &
     &        num_filtering_grp_ctl, i_num_filter_area,                 &
     &        filter_area_name_ctl)
        end if
      end do
!
      end subroutine read_filter_area_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_element_size_ctl
!
!
      if(right_begin_flag(hd_deltax_ctl) .eq. 0) return
      if (i_deltax_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_deltax_ctl, i_deltax_ctl)
        if(i_deltax_ctl .gt. 0) exit
!
        call read_dx_solver_param_ctl
!
!
        call read_character_ctl_item(hd_mass_matrix_type,               &
     &          i_mass_matrix_type, mass_matrix_type_ctl)
      end do
!
      end subroutine read_element_size_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_dx_solver_param_ctl
!
!
      if(right_begin_flag(hd_esize_solver) .eq. 0) return
      if (i_esize_solver_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_esize_solver, i_esize_solver_ctl)
        if(i_esize_solver_ctl .gt. 0) exit
!
!
        call read_character_ctl_item(hd_method_esize,                   &
     &          i_method_esize, method_esize_ctl)
        call read_character_ctl_item(hd_precond_esize,                  &
     &          i_precond_esize, precond_esize_ctl)
!
        call read_real_ctl_item(hd_eps_esize,                           &
     &          i_eps_esize, eps_esize_ctl)
        call read_real_ctl_item(hd_sigma_esize,                         &
     &          i_sigma_esize, sigma_esize_ctl)
        call read_real_ctl_item(hd_sigma_diag_esize,                    &
     &          i_sigma_diag_esize, sigma_diag_esize_ctl)
!
        call read_integer_ctl_item(hd_itr_esize,                        &
     &          i_itr_esize, itr_esize_ctl)
      end do
!
      end subroutine read_dx_solver_param_ctl
!
!   --------------------------------------------------------------------
!
      end module read_ctl_gen_filter
