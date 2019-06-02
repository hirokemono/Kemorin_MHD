!t_control_data_add_ele_grp.f90
!      module t_control_data_add_ele_grp
!
!      Written by H. Matsui on Mar., 2008
!
!       subroutine read_control_add_elegrp(addgrp_c)
!!        type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
      module t_control_data_add_ele_grp
!
      use m_precision
!
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_read_control_arrays
      use t_control_elements
      use t_control_array_charareal2
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_add_ele_grp'
!
      type control_data_add_ele_grp
        type(platform_data_control) :: source_plt
        type(platform_data_control) :: added_plt
!
        type(read_character_item) :: sph_grp_direction_ctl
!
!>       Structure for element grouping in radial direction
!!@n      r_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      r_ele_grouping_ctl%vec1:  Minimum radius for each grouping
!!@n      r_ele_grouping_ctl%vec2:  Maximum radius for each grouping
        type(ctl_array_cr2) :: r_ele_grouping_ctl
!>       Structure for element grouping in meridional direction
!!@n      t_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      t_ele_grouping_ctl%vec1:  Minimum colatitude for each grouping
!!@n      t_ele_grouping_ctl%vec2:  Maximum colatitude for each grouping
        type(ctl_array_cr2) :: t_ele_grouping_ctl
!>       Structure for element grouping in cylindrical direction
!!@n      s_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      s_ele_grouping_ctl%vec1:  Minimum cylindorical radius
!!                                  for each grouping
!!@n      s_ele_grouping_ctl%vec2:  Maximum cylindorical radius
!!                                  for each grouping
        type(ctl_array_cr2) :: s_ele_grouping_ctl
!>       Structure for element grouping in z direction
!!@n      z_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      z_ele_grouping_ctl%vec1:  Minimum z for each grouping
!!@n      z_ele_grouping_ctl%vec2:  Maximum z for each grouping
        type(ctl_array_cr2) :: z_ele_grouping_ctl
      end type control_data_add_ele_grp
!
!   Top level
!
      character(len=kchara), parameter :: hd_add_ele_grp_ctl            &
     &                      = 'add_element_groups'
      integer (kind=kint) :: i_add_ele_grp_ctl = 0
!
!   2nd level for add_element_groups
!
      character(len=kchara), parameter :: hd_files_ctl                  &
     &                      = 'file_name_ctl'
      character(len=kchara), parameter :: hd_add_ele_grp_para           &
     &                      = '2d_grouping_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      integer (kind=kint) :: i_files_ctl =        0
      integer (kind=kint) :: i_add_ele_grp_para = 0
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_new_data =      0
!
!   3rd level for element_group_ctl
!
      character(len=kchara), parameter :: hd_2nd_grp_direction          &
     &                      = '2nd_grp_direction_ctl'
      character(len=kchara), parameter :: hd_num_r_ele_grping           &
     &                      = 'radial_range_ctl'
      character(len=kchara), parameter :: hd_num_t_ele_grping           &
     &                      = 'theta_range_ctl'
      character(len=kchara), parameter :: hd_num_s_ele_grping           &
     &                      = 's_range_ctl'
      character(len=kchara), parameter :: hd_num_z_ele_grping           &
     &                      = 'z_range_ctl'
!
      private :: hd_add_ele_grp_ctl, i_add_ele_grp_ctl
      private :: hd_files_ctl, i_files_ctl
      private :: hd_add_ele_grp_para, i_add_ele_grp_para
      private :: hd_2nd_grp_direction
      private :: hd_platform, i_platform
      private :: hd_new_data, i_new_data
      private :: hd_num_r_ele_grping, hd_num_t_ele_grping
      private :: hd_num_s_ele_grping, hd_num_z_ele_grping
!
      private :: read_control_4_add_egrp_data
      private :: read_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_add_elegrp(addgrp_c)
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_control_4_add_egrp_data(addgrp_c)
!
      close(ctl_file_code)
!
      end subroutine read_control_add_elegrp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_control_add_elegrp(addgrp_c)
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call dealloc_control_array_c_r2(addgrp_c%r_ele_grouping_ctl)
      call dealloc_control_array_c_r2(addgrp_c%s_ele_grouping_ctl)
      call dealloc_control_array_c_r2(addgrp_c%t_ele_grouping_ctl)
      call dealloc_control_array_c_r2(addgrp_c%z_ele_grouping_ctl)
!
      end subroutine dealloc_control_add_elegrp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_control_4_add_egrp_data(addgrp_c)
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      if(right_begin_flag(hd_add_ele_grp_ctl) .eq. 0) return
      if (i_add_ele_grp_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_add_ele_grp_ctl = find_control_end_flag(hd_add_ele_grp_ctl)
        if(i_add_ele_grp_ctl .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, addgrp_c%source_plt)
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, addgrp_c%added_plt)
!
        call read_ctl_data_4_add_2d_egrp(addgrp_c)
      end do
!
      end subroutine read_control_4_add_egrp_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_add_2d_egrp(addgrp_c)
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      if(right_begin_flag(hd_add_ele_grp_para) .eq. 0) return
      if (i_add_ele_grp_para .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_add_ele_grp_para = find_control_end_flag(hd_add_ele_grp_para)
        if(i_add_ele_grp_para .gt. 0) exit
!
        call read_control_array_c_r2(ctl_file_code,                     &
     &      hd_num_r_ele_grping, addgrp_c%r_ele_grouping_ctl, c_buf1)
        call read_control_array_c_r2(ctl_file_code,                     &
     &      hd_num_t_ele_grping, addgrp_c%t_ele_grouping_ctl, c_buf1)
        call read_control_array_c_r2(ctl_file_code,                     &
     &      hd_num_s_ele_grping, addgrp_c%s_ele_grouping_ctl, c_buf1)
        call read_control_array_c_r2(ctl_file_code,                     &
     &      hd_num_z_ele_grping, addgrp_c%z_ele_grouping_ctl, c_buf1)
!
!
        call read_chara_ctl_type(hd_2nd_grp_direction,                  &
     &      addgrp_c%sph_grp_direction_ctl)
      end do
!
      end subroutine read_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------!
      end module t_control_data_add_ele_grp

