!>@file   t_ctl_data_gen_table.f90
!!@brief  module t_ctl_data_gen_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Structure for reading parameters to generate interpolate table
!!
!!@verbatim
!!      subroutine read_control_4_gen_itp_table(gtbl_ctl)
!!      subroutine read_control_4_interpolate(gtbl_ctl)
!!      subroutine read_control_4_distribute_itp(gtbl_ctl)
!!
!!      subroutine dealloc_ctl_data_gen_table(gtbl_ctl)
!!        type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!!@endverbatim
!
!     required module for 3rd level
!
      module t_ctl_data_gen_table
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_control_elements
      use t_read_control_arrays
      use t_control_array_intreal
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
      type ctl_data_gen_table
!>        Structure for original mesh file controls
        type(platform_data_control) :: src_plt
!>        Structure for target mesh file controls
        type(platform_data_control) :: dst_plt
!
!>        Structure for field information control
        type(field_control) :: fld_gt_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_gt_ctl
!
!>        file prefix for interpolation table
        type(read_character_item) :: table_head_ctl
!
!>        file format for interpolation table
        type(read_character_item) :: fmt_itp_table_file_ctl
!
!>        file prefix for interpolation table
        type(read_character_item) :: itp_node_head_ctl
!
!>        switch for element interpolation table
        type(read_character_item) :: reverse_element_table_ctl
!
!>        file prefix for single interpolation table
        type(read_character_item) :: single_itp_tbl_head_ctl
!
!>        Structure for element hash type
        type(read_character_item) :: ele_hash_type_ctl
!
!>        Structure for element grouping in radial direction
        type(read_integer_item) :: num_radial_divide_ctl
!>        Structure for element grouping in meridional direction
        type(read_integer_item) :: num_theta_divide_ctl
!>        Structure for element grouping in zonal direction
        type(read_integer_item) :: num_phi_divide_ctl
!
!>        Structure for element grouping in x direction
        type(read_integer_item) :: num_x_divide_ctl
!>        Structure for element grouping in y direction
        type(read_integer_item) :: num_y_divide_ctl
!>        Structure for element grouping in z direction
        type(read_integer_item) :: num_z_divide_ctl
!
!>        Structure for error torrance for refine interpolation
!!@n        eps_4_itp_ctl%ivec:  level for interpolation
!!@n        eps_4_itp_ctl%vect:  Error torrance for interpolation
        type(ctl_array_ir) :: eps_4_itp_ctl
!
!>        Structure for maximum iteration counts
        type(read_integer_item) :: itr_refine_ctl
!
!>        Structure for error torrance
        type(read_real_item) :: eps_refine_ctl
      end type ctl_data_gen_table
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
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_new_data =      0
      integer (kind=kint) :: i_phys_values =   0
      integer (kind=kint) :: i_tstep =      0
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
      private :: hd_phys_values, i_phys_values
      private :: hd_time_step, i_tstep
      private :: hd_platform, i_platform
      private :: hd_new_data, i_new_data
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
      subroutine read_control_4_gen_itp_table(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      ctl_file_code = table_ctl_file_code
      open(ctl_file_code, file=fname_table_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_itp_tbl_ctl_data(gtbl_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_gen_itp_table
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_interpolate(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      ctl_file_code = table_ctl_file_code
      open(ctl_file_code, file=fname_itp_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_itp_tbl_ctl_data(gtbl_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_interpolate
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_distribute_itp(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      ctl_file_code = table_ctl_file_code
      open(ctl_file_code, file=fname_dist_itp_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_control_dist_itp_data(gtbl_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_distribute_itp
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_gen_table(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call dealloc_control_array_i_r(gtbl_ctl%eps_4_itp_ctl)
!
      gtbl_ctl%table_head_ctl%iflag = 0
      gtbl_ctl%fmt_itp_table_file_ctl%iflag = 0
      gtbl_ctl%itp_node_head_ctl%iflag = 0
      gtbl_ctl%reverse_element_table_ctl%iflag = 0
      gtbl_ctl%single_itp_tbl_head_ctl%iflag = 0
      gtbl_ctl%ele_hash_type_ctl%iflag = 0
      gtbl_ctl%num_radial_divide_ctl%iflag = 0
      gtbl_ctl%num_theta_divide_ctl%iflag = 0
      gtbl_ctl%num_phi_divide_ctl%iflag = 0
!
      gtbl_ctl%num_x_divide_ctl%iflag = 0
      gtbl_ctl%num_y_divide_ctl%iflag = 0
      gtbl_ctl%num_z_divide_ctl%iflag = 0
!
      gtbl_ctl%itr_refine_ctl%iflag = 0
      gtbl_ctl%eps_refine_ctl%iflag = 0
!
      end subroutine dealloc_ctl_data_gen_table
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_const_itp_tbl_ctl_data(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      if(right_begin_flag(hd_table_control) .eq. 0) return
      if (i_table_control.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_table_control = find_control_end_flag(hd_table_control)
        if(i_table_control .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, gtbl_ctl%src_plt)
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, gtbl_ctl%dst_plt)
!
        call read_itp_files_ctl(gtbl_ctl)
        call read_element_hash_ctl(gtbl_ctl)
        call read_itaration_param_ctl(gtbl_ctl)
        call read_itaration_model_ctl(gtbl_ctl)
      end do
!
      end subroutine read_const_itp_tbl_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine read_control_dist_itp_data(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      if(right_begin_flag(hd_distribute_itp) .eq. 0) return
      if (i_distribute_itp.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_distribute_itp = find_control_end_flag(hd_distribute_itp)
        if(i_distribute_itp .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, gtbl_ctl%src_plt)
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, gtbl_ctl%dst_plt)
!
        call read_itp_files_ctl(gtbl_ctl)
      end do
!
      end subroutine read_control_dist_itp_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_itp_files_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      if(right_begin_flag(hd_itp_files) .eq. 0) return
      if (i_itp_files.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_itp_files = find_control_end_flag(hd_itp_files)
        if(i_itp_files .gt. 0) exit
!
!
        call read_chara_ctl_type                                        &
     &     (hd_table_head_ctl, gtbl_ctl%table_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_itp_node_head_ctl, gtbl_ctl%itp_node_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_single_itp_tbl, gtbl_ctl%single_itp_tbl_head_ctl)
        call read_chara_ctl_type                                        &
     &      (hd_reverse_ele_tbl, gtbl_ctl%reverse_element_table_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_fmt_itp_tbl, gtbl_ctl%fmt_itp_table_file_ctl)
      end do
!
      end subroutine read_itp_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_itaration_model_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      if(right_begin_flag(hd_itp_model) .eq. 0) return
      if (i_itp_model.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_itp_model = find_control_end_flag(hd_itp_model)
        if(i_itp_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, gtbl_ctl%fld_gt_ctl)
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, gtbl_ctl%t_gt_ctl)
      end do
!
      end subroutine read_itaration_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_itaration_param_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      if(right_begin_flag(hd_iteration_ctl) .eq. 0) return
      if (i_iteration_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_iteration_ctl = find_control_end_flag(hd_iteration_ctl)
        if(i_iteration_ctl .gt. 0) exit
!
        call read_control_array_i_r(ctl_file_code,                      &
     &      hd_eps_4_itp, gtbl_ctl%eps_4_itp_ctl, c_buf1)
!
        call read_integer_ctl_type(hd_itr, gtbl_ctl%itr_refine_ctl)
!
        call read_real_ctl_type(hd_eps, gtbl_ctl%eps_refine_ctl)
      end do
!
      end subroutine read_itaration_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_element_hash_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      if(right_begin_flag(hd_element_hash) .eq. 0) return
      if (i_element_hash .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_element_hash = find_control_end_flag(hd_element_hash)
        if(i_element_hash .gt. 0) exit
!
!
        call read_chara_ctl_type                                        &
     &     (hd_hash_type, gtbl_ctl%ele_hash_type_ctl)
! 
        call read_integer_ctl_type(hd_search_radius,                    &
     &      gtbl_ctl%num_radial_divide_ctl)
        call read_integer_ctl_type(hd_num_hash_elev,                    &
     &      gtbl_ctl%num_theta_divide_ctl)
        call read_integer_ctl_type(hd_num_hash_azim,                    &
     &      gtbl_ctl%num_phi_divide_ctl)
!
        call read_integer_ctl_type                                      &
     &     (hd_num_hash_x, gtbl_ctl%num_x_divide_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_num_hash_y, gtbl_ctl%num_y_divide_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_num_hash_z, gtbl_ctl%num_z_divide_ctl)
      end do
!
      end subroutine read_element_hash_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_gen_table
