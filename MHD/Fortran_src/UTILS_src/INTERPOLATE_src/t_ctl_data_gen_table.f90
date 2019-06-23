!>@file   t_ctl_data_gen_table.f90
!!@brief  module t_ctl_data_gen_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Structure for reading parameters to generate interpolate table
!!
!!@verbatim
!!      subroutine read_control_4_distribute_itp(gtbl_ctl)
!!
!!      subroutine dealloc_ctl_data_gen_table(gtbl_ctl)
!!        type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!!
!!      subroutine read_const_itp_tbl_ctl_data                          &
!!     &         (id_control, hd_block, gtbl_ctl, c_buf)
!!@endverbatim
!
      module t_ctl_data_gen_table
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_control_elements
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
!
        integer (kind=kint) :: i_itp_files = 0
        integer (kind=kint) :: i_itp_model = 0
        integer (kind=kint) :: i_iteration_ctl = 0
        integer(kind = kint) :: i_element_hash = 0
!
        integer (kind=kint) :: i_table_control = 0
        integer (kind=kint) :: i_distribute_itp = 0
      end type ctl_data_gen_table
!
!
!
!     Top level
!
      character(len=kchara), parameter :: hd_distribute_itp             &
     &                   = 'parallel_table'
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
      private :: hd_itp_files, hd_itp_model
      private :: hd_iteration_ctl, hd_search_radius
      private :: hd_element_hash, hd_fmt_itp_tbl
      private :: hd_table_head_ctl, hd_itp_node_head_ctl
      private :: hd_reverse_ele_tbl, hd_single_itp_tbl
      private :: hd_eps_4_itp, hd_itr, hd_eps
      private :: hd_num_hash_x, hd_num_hash_y, hd_num_hash_z
      private :: hd_phys_values, hd_time_step
      private :: hd_platform, hd_new_data
!
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
      subroutine read_control_4_distribute_itp(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
        open(table_ctl_file_code, file=fname_dist_itp_ctl,status='old')
        do
          call load_one_line_from_control(table_ctl_file_code, c_buf1)
          call read_control_dist_itp_data                               &
     &       (table_ctl_file_code, hd_distribute_itp, gtbl_ctl, c_buf1)
          if(gtbl_ctl%i_distribute_itp .gt. 0) exit
        end do
        close(table_ctl_file_code)
!
      end subroutine read_control_4_distribute_itp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_const_itp_tbl_ctl_data                            &
     &         (id_control, hd_block, gtbl_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gtbl_ctl%i_distribute_itp .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, gtbl_ctl%src_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_new_data, gtbl_ctl%dst_plt, c_buf)
!
        call read_itp_files_ctl                                         &
     &     (id_control, hd_itp_files, gtbl_ctl, c_buf)
        call read_element_hash_ctl                                      &
     &     (id_control, hd_element_hash, gtbl_ctl, c_buf)
        call read_itaration_param_ctl                                   &
     &     (id_control, hd_iteration_ctl, gtbl_ctl, c_buf)
        call read_itaration_model_ctl                                   &
     &     (id_control, hd_itp_model, gtbl_ctl, c_buf)
      end do
      gtbl_ctl%i_table_control = 1
!
      end subroutine read_const_itp_tbl_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_gen_table(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call reset_itp_files_ctl(gtbl_ctl)
      call dealloc_itaration_model_ctl(gtbl_ctl)
      call dealloc_itaration_param_ctl(gtbl_ctl)
      call reset_element_hash_ctl(gtbl_ctl)
      gtbl_ctl%i_table_control = 0
!
      end subroutine dealloc_ctl_data_gen_table
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_dist_itp_data                             &
     &         (id_control, hd_block, gtbl_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gtbl_ctl%i_distribute_itp .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, gtbl_ctl%src_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_new_data, gtbl_ctl%dst_plt, c_buf)
!
        call read_itp_files_ctl                                         &
     &     (id_control, hd_itp_files, gtbl_ctl, c_buf)
      end do
      gtbl_ctl%i_distribute_itp = 1
!
      end subroutine read_control_dist_itp_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_control_dist_itp_data(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call reset_itp_files_ctl(gtbl_ctl)
      gtbl_ctl%i_distribute_itp = 0
!
      end subroutine reset_control_dist_itp_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_itp_files_ctl                                     &
     &         (id_control, hd_block, gtbl_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gtbl_ctl%i_itp_files .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_table_head_ctl, gtbl_ctl%table_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_itp_node_head_ctl, gtbl_ctl%itp_node_head_ctl)
        call read_chara_ctl_type(c_buf, hd_single_itp_tbl,              &
     &      gtbl_ctl%single_itp_tbl_head_ctl)
        call read_chara_ctl_type(c_buf, hd_reverse_ele_tbl,             &
     &      gtbl_ctl%reverse_element_table_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_fmt_itp_tbl, gtbl_ctl%fmt_itp_table_file_ctl)
      end do
      gtbl_ctl%i_itp_files = 1
!
      end subroutine read_itp_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_itp_files_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      gtbl_ctl%table_head_ctl%iflag = 0
      gtbl_ctl%fmt_itp_table_file_ctl%iflag = 0
      gtbl_ctl%itp_node_head_ctl%iflag = 0
      gtbl_ctl%reverse_element_table_ctl%iflag = 0
      gtbl_ctl%single_itp_tbl_head_ctl%iflag = 0
!
      gtbl_ctl%i_itp_files = 0
!
      end subroutine reset_itp_files_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_itaration_model_ctl                               &
     &         (id_control, hd_block, gtbl_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gtbl_ctl%i_itp_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, gtbl_ctl%fld_gt_ctl, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, gtbl_ctl%t_gt_ctl, c_buf)
      end do
      gtbl_ctl%i_itp_model = 1
!
      end subroutine read_itaration_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_itaration_model_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call dealloc_phys_control(gtbl_ctl%fld_gt_ctl)
      gtbl_ctl%i_itp_model = 0
!
      end subroutine dealloc_itaration_model_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_itaration_param_ctl                               &
     &         (id_control, hd_block, gtbl_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gtbl_ctl%i_iteration_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_i_r(id_control,                         &
     &      hd_eps_4_itp, gtbl_ctl%eps_4_itp_ctl, c_buf)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_itr, gtbl_ctl%itr_refine_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_eps, gtbl_ctl%eps_refine_ctl)
      end do
      gtbl_ctl%i_iteration_ctl = 1
!
      end subroutine read_itaration_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_itaration_param_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call dealloc_control_array_i_r(gtbl_ctl%eps_4_itp_ctl)
!
      gtbl_ctl%itr_refine_ctl%iflag = 0
      gtbl_ctl%eps_refine_ctl%iflag = 0
!
      gtbl_ctl%i_iteration_ctl = 0
!
      end subroutine dealloc_itaration_param_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_element_hash_ctl                                  &
     &         (id_control, hd_block, gtbl_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gtbl_ctl%i_element_hash .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_hash_type, gtbl_ctl%ele_hash_type_ctl)
! 
        call read_integer_ctl_type(c_buf, hd_search_radius,             &
     &      gtbl_ctl%num_radial_divide_ctl)
        call read_integer_ctl_type(c_buf, hd_num_hash_elev,             &
     &      gtbl_ctl%num_theta_divide_ctl)
        call read_integer_ctl_type(c_buf, hd_num_hash_azim,             &
     &      gtbl_ctl%num_phi_divide_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_hash_x, gtbl_ctl%num_x_divide_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_hash_y, gtbl_ctl%num_y_divide_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_hash_z, gtbl_ctl%num_z_divide_ctl)
      end do
      gtbl_ctl%i_element_hash = 1
!
      end subroutine read_element_hash_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_element_hash_ctl(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      gtbl_ctl%ele_hash_type_ctl%iflag = 0
!
      gtbl_ctl%num_radial_divide_ctl%iflag = 0
      gtbl_ctl%num_theta_divide_ctl%iflag = 0
      gtbl_ctl%num_phi_divide_ctl%iflag = 0
!
      gtbl_ctl%num_x_divide_ctl%iflag = 0
      gtbl_ctl%num_y_divide_ctl%iflag = 0
      gtbl_ctl%num_z_divide_ctl%iflag = 0
!
      gtbl_ctl%i_element_hash = 0
!
      end subroutine reset_element_hash_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_gen_table
