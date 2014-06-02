!
!      module m_ctl_data_gen_table
!
!      Written by H. Matsui on July, 2006
!
!      subroutine deallocate_search_param_ctl
!      subroutine read_control_4_gen_itp_table
!      subroutine read_control_4_interpolate
!      subroutine read_control_4_distribute_itp
!
!     required module for 3rd level
!
      module m_ctl_data_gen_table
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
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
      character(len = kchara) :: table_head_ctl = "mesh/itp_table"
      character(len = kchara) :: ifmt_itp_table_file_ctl = "ascii"
!
      character(len = kchara) :: itp_node_head_ctl = "node_test_itp"
      character(len = kchara) :: reverse_element_table_ctl = "OFF"
!
      character(len = kchara) :: single_itp_tbl_head_ctl = "single_itp"
!
      character(len = kchara) :: ele_hash_type_ctl = "sphere"
      integer(kind = kint) :: num_theta_divide_ctl = 0
      integer(kind = kint) :: num_phi_divide_ctl = 0
!
!!      Structure for element grouping in meridional direction
!!@n      r_ele_grouping_ctl%vect:  Radius data for searching
      type(ctl_array_real) :: radial_divide_ctl
!
      integer(kind = kint) :: num_search_times_ctl = 0
      integer(kind = kint), allocatable :: i_search_sleeve_ctl(:)
      real(kind = kreal), allocatable :: search_error_level_ctl(:)
!
      integer (kind=kint) :: itr_refine_ctl = 20000
      real (kind=kreal) :: eps_refine_ctl = 1.0d-15
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
      integer (kind=kint) :: i_table_head_ctl =     0
      integer (kind=kint) :: i_itp_node_head_ctl =  0
      integer (kind=kint) :: i_reverse_ele_tbl =    0
      integer (kind=kint) :: i_single_itp_tbl =     0
      integer (kind=kint) :: i_fmt_itp_tbl =        0
!
!     3rd level for element hash
!
      character(len=kchara), parameter                                  &
     &         ::  hd_hash_type =       'hash_type_name'
      character(len=kchara), parameter                                  &
     &         ::  hd_search_radius = 'radius_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_num_hash_elev =   'num_elevation_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_num_hash_azim =   'num_azimuth_ctl'
!
      integer (kind=kint) :: i_hash_type =       0
      integer (kind=kint) :: i_num_hash_elev =   0
      integer (kind=kint) :: i_num_hash_azim =   0
!
!     3rd level for iteration  control
!
      character(len=kchara), parameter                                  &
     &         ::  hd_num_search = 'search_level_ctl'
      character(len=kchara), parameter                                  &
     &         ::  hd_itr =        'maxiter'
      character(len=kchara), parameter                                  &
     &         ::  hd_eps =        'eps_4_refine'
!
      integer (kind=kint) :: i_num_search = 0
      integer (kind=kint) :: i_itr =        0
      integer (kind=kint) :: i_eps =        0
!
      private :: table_ctl_file_code, fname_table_ctl, fname_itp_ctl
      private :: hd_table_control, i_table_control
      private :: hd_itp_files, hd_itp_model, i_itp_files, i_itp_model
      private :: hd_iteration_ctl, i_iteration_ctl, hd_search_radius
      private :: hd_element_hash, i_element_hash, hd_fmt_itp_tbl
      private :: hd_table_head_ctl, hd_itp_node_head_ctl
      private :: hd_reverse_ele_tbl, hd_single_itp_tbl
      private :: hd_num_search, hd_itr, hd_eps
!
      private :: allocate_search_param_ctl
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
      subroutine allocate_search_param_ctl
!
      allocate(i_search_sleeve_ctl(num_search_times_ctl) )
      allocate(search_error_level_ctl(num_search_times_ctl) )
      i_search_sleeve_ctl = 0
      search_error_level_ctl = 0.0d0
!
      end subroutine allocate_search_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_search_param_ctl
!
      deallocate(i_search_sleeve_ctl)
      deallocate(search_error_level_ctl)
!
      end subroutine deallocate_search_param_ctl
!
!   --------------------------------------------------------------------
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
        call read_character_ctl_item(hd_table_head_ctl,                 &
     &        i_table_head_ctl, table_head_ctl)
        call read_character_ctl_item(hd_itp_node_head_ctl,              &
     &        i_itp_node_head_ctl, itp_node_head_ctl)
        call read_character_ctl_item(hd_single_itp_tbl,                 &
     &        i_single_itp_tbl, single_itp_tbl_head_ctl)
        call read_character_ctl_item(hd_reverse_ele_tbl,                &
     &        i_reverse_ele_tbl, reverse_element_table_ctl)
!
        call read_character_ctl_item(hd_fmt_itp_tbl, i_fmt_itp_tbl,     &
     &      ifmt_itp_table_file_ctl)
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
        call find_control_array_flag(hd_num_search,                     &
     &      num_search_times_ctl)
        if(num_search_times_ctl.gt.0 .and. i_num_search.eq.0) then
          call allocate_search_param_ctl
          call read_control_array_int_r_list(hd_num_search,             &
     &        num_search_times_ctl, i_num_search,                       &
     &        i_search_sleeve_ctl, search_error_level_ctl)
        end if
!
        call read_integer_ctl_item(hd_itr, i_itr, itr_refine_ctl)
!
        call read_real_ctl_item(hd_eps, i_eps, eps_refine_ctl)
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
        call read_control_array_real                                    &
     &     (hd_search_radius, radial_divide_ctl)
!
!
        call read_character_ctl_item(hd_hash_type,                      &
     &        i_hash_type, ele_hash_type_ctl)
! 
        call read_integer_ctl_item(hd_num_hash_elev,                    &
     &          i_num_hash_elev, num_theta_divide_ctl)
        call read_integer_ctl_item(hd_num_hash_azim,                    &
     &          i_num_hash_azim, num_phi_divide_ctl)
      end do
!
      end subroutine read_element_hash_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_gen_table
