!
!      module m_control_data_4_refine
!
!      Written by Kemorin on Oct., 2007
!
!      subroutine read_control_data_4_refiner
!      subroutine read_ctl_data_4_refine_mesh
!
      module m_control_data_4_refine
!
      use m_precision
!
      use t_ctl_data_4_platforms
      use t_read_control_arrays
      use m_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_refine'
!
!
      type(platform_data_control), save :: source_plt
      type(platform_data_control), save :: refined_plt
!
      type(read_character_item), save :: coarse_2_fine_head_ctl
      type(read_character_item), save :: fine_2_course_head_ctl
      type(read_character_item), save :: refine_info_head_ctl
      type(read_character_item), save :: old_refine_info_head_ctl
!
      type(read_character_item), save :: interpolate_type_ctl
!
      type(ctl_array_c2), save :: refined_ele_grp_ctl
      type(ctl_array_ci), save :: refine_i_ele_grp_ctl
!
!
!   Top level
!
      character(len=kchara), parameter :: hd_refine_ctl                 &
     &                      = 'refine_control'
      integer (kind=kint) :: i_refine_ctl = 0
!
!   2nd level for partitioner_control
!
      character(len=kchara), parameter :: hd_single_refine_files        &
     &                      = 'single_refined_table_ctl'
      character(len=kchara), parameter :: hd_refine_param               &
     &                      = 'refine_parameter_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      integer (kind=kint) :: i_single_refine_files =  0
      integer (kind=kint) :: i_refine_param = 0
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_new_data =      0
!
!   3rd level for partitioner_control
!
      character(len=kchara), parameter :: hd_course_to_fine_ctl         &
     &                      = 'cource_to_fine_table_ctl'
      character(len=kchara), parameter :: hd_fine_to_course_ctl         &
     &                      = 'fine_to_cource_table_ctl'
      character(len=kchara), parameter :: hd_refine_info_ctl            &
     &                      = 'refine_info_head_ctl'
      character(len=kchara), parameter :: hd_old_refine_info_ctl        &
     &                      = 'old_refine_info_head_ctl'
!
!   3rd level for refine_parameter_ctl
!
      character(len=kchara), parameter :: hd_itp_type                   &
     &                      = 'interpolate_type_ctl'
      character(len=kchara), parameter :: hd_num_ref_type               &
     &                      = 'refine_data_ctl'
      character(len=kchara), parameter :: hd_num_ref_code               &
     &                      = 'refine_code_ctl'
!
!
      private :: control_file_name
      private :: hd_refine_ctl, i_refine_ctl
      private :: hd_single_refine_files, i_single_refine_files
      private :: hd_refine_param, hd_new_data, i_new_data
      private :: i_refine_param
      private :: hd_platform, i_platform
      private :: hd_course_to_fine_ctl, hd_fine_to_course_ctl
      private :: hd_refine_info_ctl, hd_old_refine_info_ctl
!
      private :: read_refine_control_data
      private :: read_ctl_data_4_refine_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_4_refiner
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_refine_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_4_refiner
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_refine_control_data
!
!
      if(right_begin_flag(hd_refine_ctl) .eq. 0) return
      if (i_refine_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_refine_ctl, i_refine_ctl)
        if(i_refine_ctl .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, source_plt)
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, refined_plt)
        call read_ctl_data_4_refine_mesh
        call read_ctl_data_4_refine_type
      end do
!
      end subroutine read_refine_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_refine_mesh
!
!
      if(right_begin_flag(hd_single_refine_files) .eq. 0) return
      if (i_single_refine_files .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_single_refine_files,              &
     &      i_single_refine_files)
        if(i_single_refine_files .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_course_to_fine_ctl,                 &
     &      coarse_2_fine_head_ctl)
        call read_chara_ctl_type(hd_fine_to_course_ctl,                 &
     &      fine_2_course_head_ctl)
!
        call read_chara_ctl_type(hd_refine_info_ctl,                    &
     &      refine_info_head_ctl)
        call read_chara_ctl_type(hd_old_refine_info_ctl,                &
     &      old_refine_info_head_ctl)
      end do
!
      end subroutine read_ctl_data_4_refine_mesh
!
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_refine_type
!
!
      if(right_begin_flag(hd_refine_param) .eq. 0) return
      if (i_refine_param .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_refine_param, i_refine_param)
        if(i_refine_param .gt. 0) exit
!
!
        call read_control_array_c2                                      &
     &     (hd_num_ref_type, refined_ele_grp_ctl)
!
        call read_control_array_c_i                                     &
     &     (hd_num_ref_code, refine_i_ele_grp_ctl)
!
        call read_chara_ctl_type(hd_itp_type, interpolate_type_ctl)
      end do
!
      end subroutine read_ctl_data_4_refine_type
!
! -----------------------------------------------------------------------!
      end module m_control_data_4_refine
