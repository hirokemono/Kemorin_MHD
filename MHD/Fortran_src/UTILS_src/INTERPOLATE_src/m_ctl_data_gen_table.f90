!>@file   m_ctl_data_gen_table.f90
!!@brief  module m_ctl_data_gen_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Structure for reading parameters to generate interpolate table
!!
!!@verbatim
!!      subroutine read_control_4_gen_itp_table
!!      subroutine read_control_4_interpolate
!!      subroutine read_control_4_distribute_itp
!!@endverbatim
!
!     required module for 3rd level
!
      module m_ctl_data_gen_table
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind = kint), parameter :: table_ctl_file_code = 11
!
      character(len = kchara), parameter                                &
     &                 :: fname_table_ctl = "ctl_gen_table"
      character(len = kchara), parameter                                &
     &                 :: fname_itp_ctl = "ctl_interpolate"
      character(len = kchara), parameter                                &
     &                 :: fname_dist_itp_ctl = "ctl_distribute_itp"
!
!
!>      file prefix for interpolation table
      type(read_character_item), save :: table_head_ctl
!
!>      file format for interpolation table
      type(read_character_item), save :: fmt_itp_table_file_ctl
!
!>      file prefix for interpolation table
      type(read_character_item), save :: itp_node_head_ctl
!
!>      switch for element interpolation table
      type(read_character_item), save :: reverse_element_table_ctl
!
!>      file prefix for single interpolation table
      type(read_character_item), save :: single_itp_tbl_head_ctl
!
!>      Structure for element hash type
      type(read_character_item), save :: ele_hash_type_ctl
!
!>      Structure for element grouping in radial direction
      type(read_integer_item), save :: num_radial_divide_ctl
!>      Structure for element grouping in meridional direction
      type(read_integer_item), save :: num_theta_divide_ctl
!>      Structure for element grouping in zonal direction
      type(read_integer_item), save :: num_phi_divide_ctl
!
!>      Structure for element grouping in x direction
      type(read_integer_item), save :: num_x_divide_ctl
!>      Structure for element grouping in y direction
      type(read_integer_item), save :: num_y_divide_ctl
!>      Structure for element grouping in z direction
      type(read_integer_item), save :: num_z_divide_ctl
!
!>      Structure for error torrance for refine interpolation
!!@n      eps_4_itp_ctl%ivec:  level for interpolation
!!@n      eps_4_itp_ctl%vect:  Error torrance for interpolation
      type(ctl_array_ir), save :: eps_4_itp_ctl
!
!>      Structure for maximum iteration counts
      type(read_integer_item), save :: itr_refine_ctl
!
!>      Structure for error torrance
      type(read_real_item), save :: eps_refine_ctl
!
!
!
!     Top level
!
      character(len=kchara), parameter :: hd_table_control              &
     &                   = 'construct_table'
      integer (kind=kint) :: i_table_control = 0
!
      character(len=kchara), parameter :: hd_distribute_itp             &
     &                   = 'parallel_table'
      integer (kind=kint) :: i_distribute_itp = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_itp_files = 'interpolate_tbl_ctl'
!
      character(len=kchara), parameter                                  &
     &         :: hd_itp_model = 'model_4_interpolate'
!
      character(len=kchara), parameter                                  &
     &         :: hd_element_hash  =  'element_hash'
      character(len=kchara), parameter                                  &
     &         :: hd_iteration_ctl = 'iteration_ctl'
!
      integer (kind=kint) :: i_itp_files = 0
      integer (kind=kint) :: i_itp_model = 0
!
      integer (kind=kint) :: i_element_hash = 0
      integer (kind=kint) :: i_iteration_ctl =    0
!
!     3rd level for file header
!
      character(len=kchara), parameter                                  &
     &         :: hd_table_head_ctl =    'interpolate_list_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_itp_node_head_ctl = 'interpolated_node_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_reverse_ele_tbl =   'reverse_element_table_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_single_itp_tbl =    'single_interpolate_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_fmt_itp_tbl =    'interpolate_table_format_ctl'
!
!     3rd level for element hash
!
      character(len=kchara), parameter                                  &
     &         ::  hd_hash_type =       'hash_type_name'
      character(len=kchara), parameter                                  &
     &         ::  hd_search_radius =   'num_radius_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_num_hash_elev =   'num_elevation_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_num_hash_azim =   'num_azimuth_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_num_hash_x =      'num_x_divide_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_num_hash_y =      'num_y_divide_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_num_hash_z =      'num_z_divide_ctl'
!
!     3rd level for iteration  control
!
      character(len=kchara), parameter                                  &
     &         ::  hd_eps_4_itp = 'search_level_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_itr =        'maxiter'
      character(len=kchara), parameter                                  &
     &         ::  hd_eps =        'eps_4_refine'
!
      private :: table_ctl_file_code, fname_table_ctl, fname_itp_ctl
      private :: hd_table_control, i_table_control
      private :: hd_itp_files, hd_itp_model, i_itp_files, i_itp_model
      private :: hd_iteration_ctl, i_iteration_ctl, hd_search_radius
      private :: hd_element_hash, i_element_hash, hd_fmt_itp_tbl
      private :: hd_table_head_ctl, hd_itp_node_head_ctl
      private :: hd_reverse_ele_tbl, hd_single_itp_tbl
      private :: hd_eps_4_itp, hd_itr, hd_eps
      private :: hd_num_hash_x, hd_num_hash_y, hd_num_hash_z
!
      private :: read_const_itp_tbl_ctl_data
      private :: read_control_dist_itp_data
      private :: read_itp_files_ctl, read_element_hash_ctl
      private :: read_itaration_param_ctl, read_itaration_model_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_gen_itp_table
!
!
      ctl_file_code = table_ctl_file_code
      open(ctl_file_code, file=fname_table_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_itp_tbl_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_gen_itp_table
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_interpolate
!
!
      ctl_file_code = table_ctl_file_code
      open(ctl_file_code, file=fname_itp_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_itp_tbl_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_interpolate
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_distribute_itp
!
!
      ctl_file_code = table_ctl_file_code
      open(ctl_file_code, file=fname_dist_itp_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_control_dist_itp_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_distribute_itp
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_const_itp_tbl_ctl_data
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
!
!
      if(right_begin_flag(hd_table_control) .eq. 0) return
      if (i_table_control.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_table_control, i_table_control)
        if(i_table_control .gt. 0) exit
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_new_data
!
        call read_itp_files_ctl
        call read_element_hash_ctl
        call read_itaration_param_ctl
        call read_itaration_model_ctl
      end do
!
      end subroutine read_const_itp_tbl_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine read_control_dist_itp_data
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
!
!
      if(right_begin_flag(hd_distribute_itp) .eq. 0) return
      if (i_distribute_itp.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_distribute_itp, i_distribute_itp)
        if(i_distribute_itp .gt. 0) exit
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_new_data
!
        call read_itp_files_ctl
      end do
!
      end subroutine read_control_dist_itp_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_itp_files_ctl
!
!
      if(right_begin_flag(hd_itp_files) .eq. 0) return
      if (i_itp_files.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_itp_files, i_itp_files)
        if(i_itp_files .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_table_head_ctl, table_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_itp_node_head_ctl, itp_node_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_single_itp_tbl, single_itp_tbl_head_ctl)
        call read_chara_ctl_type                                        &
     &      (hd_reverse_ele_tbl, reverse_element_table_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_fmt_itp_tbl, fmt_itp_table_file_ctl)
      end do
!
      end subroutine read_itp_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_itaration_model_ctl
!
      use m_ctl_data_4_fields
      use m_ctl_data_4_time_steps
!
!
      if(right_begin_flag(hd_itp_model) .eq. 0) return
      if (i_itp_model.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_itp_model, i_itp_model)
        if(i_itp_model .gt. 0) exit
!
        call read_phys_values
        call read_time_step_ctl
      end do
!
      end subroutine read_itaration_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_itaration_param_ctl
!
!
      if(right_begin_flag(hd_iteration_ctl) .eq. 0) return
      if (i_iteration_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_iteration_ctl, i_iteration_ctl)
        if(i_iteration_ctl .gt. 0) exit
!
        call read_control_array_i_r(hd_eps_4_itp, eps_4_itp_ctl)
!
        call read_integer_ctl_type(hd_itr, itr_refine_ctl)
!
        call read_real_ctl_type(hd_eps, eps_refine_ctl)
      end do
!
      end subroutine read_itaration_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_element_hash_ctl
!
!
      if(right_begin_flag(hd_element_hash) .eq. 0) return
      if (i_element_hash .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_element_hash, i_element_hash)
        if(i_element_hash .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_hash_type, ele_hash_type_ctl)
! 
        call read_integer_ctl_type(hd_search_radius,                    &
     &      num_radial_divide_ctl)
        call read_integer_ctl_type(hd_num_hash_elev,                    &
     &      num_theta_divide_ctl)
        call read_integer_ctl_type(hd_num_hash_azim,                    &
     &      num_phi_divide_ctl)
!
        call read_integer_ctl_type(hd_num_hash_x, num_x_divide_ctl)
        call read_integer_ctl_type(hd_num_hash_y, num_y_divide_ctl)
        call read_integer_ctl_type(hd_num_hash_z, num_z_divide_ctl)
      end do
!
      end subroutine read_element_hash_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_gen_table
