!m_control_data_add_ele_grp.f90
!      module m_control_data_add_ele_grp
!
      module m_control_data_add_ele_grp
!
!      Written by H. Matsui on Mar., 2008
!
      use m_precision
!
      use m_read_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_add_ele_grp'
!
      character (len = kchara) :: sph_grp_direction_ctl
!
!>      Structure for element grouping in radial direction
!!@n      r_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      r_ele_grouping_ctl%vec1:  Minimum radius for each grouping
!!@n      r_ele_grouping_ctl%vec2:  Maximum radius for each grouping
      type(ctl_array_cr2) :: r_ele_grouping_ctl
!>      Structure for element grouping in meridional direction
!!@n      t_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      t_ele_grouping_ctl%vec1:  Minimum colatitude for each grouping
!!@n      t_ele_grouping_ctl%vec2:  Maximum colatitude for each grouping
      type(ctl_array_cr2) :: t_ele_grouping_ctl
!>      Structure for element grouping in cylindrical direction
!!@n      s_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      s_ele_grouping_ctl%vec1:  Minimum cylindorical radius
!!                                  for each grouping
!!@n      s_ele_grouping_ctl%vec2:  Maximum cylindorical radius
!!                                  for each grouping
      type(ctl_array_cr2) :: s_ele_grouping_ctl
!>      Structure for element grouping in z direction
!!@n      z_ele_grouping_ctl%c_tbl: Name of each grouping
!!@n      z_ele_grouping_ctl%vec1:  Minimum z for each grouping
!!@n      z_ele_grouping_ctl%vec2:  Maximum z for each grouping
      type(ctl_array_cr2) :: z_ele_grouping_ctl
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
      integer (kind=kint) :: i_files_ctl =        0
      integer (kind=kint) :: i_add_ele_grp_para = 0
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
      integer (kind=kint) :: i_layered_mesh_head = 0
      integer (kind=kint) :: i_2nd_grp_direction = 0
!
      private :: hd_add_ele_grp_ctl, i_add_ele_grp_ctl
      private :: hd_files_ctl, i_files_ctl
      private :: hd_add_ele_grp_para, i_add_ele_grp_para
      private :: hd_2nd_grp_direction
      private :: hd_num_r_ele_grping, hd_num_t_ele_grping
      private :: hd_num_s_ele_grping, hd_num_z_ele_grping
!
      private :: read_control_4_add_egrp_data
      private :: read_ctl_data_4_add_2d_egrp
!
!       subroutine read_control_add_elegrp
!
!      subroutine dealloc_r_ele_grp_list_ctl
!      subroutine dealloc_t_ele_grp_list_ctl
!      subroutine dealloc_s_ele_grp_list_ctl
!      subroutine dealloc_z_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_add_elegrp
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_control_4_add_egrp_data
!
      close(ctl_file_code)
!
      end subroutine read_control_add_elegrp
!
! -----------------------------------------------------------------------!
      subroutine read_control_4_add_egrp_data
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
!
      if(right_begin_flag(hd_add_ele_grp_ctl) .eq. 0) return
      if (i_add_ele_grp_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_add_ele_grp_ctl,                  &
     &      i_add_ele_grp_ctl)
        if(i_add_ele_grp_ctl .gt. 0) exit
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_new_data
!
        call read_ctl_data_4_add_2d_egrp
      end do
!
      end subroutine read_control_4_add_egrp_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_add_2d_egrp
!
!
      if(right_begin_flag(hd_add_ele_grp_para) .eq. 0) return
      if (i_add_ele_grp_para .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_add_ele_grp_para,                 &
     &      i_add_ele_grp_para)
        if(i_add_ele_grp_para .gt. 0) exit
!
        call read_control_array_c_r2                                    &
     &      (hd_num_r_ele_grping, r_ele_grouping_ctl)
        call read_control_array_c_r2                                    &
     &      (hd_num_t_ele_grping, t_ele_grouping_ctl)
        call read_control_array_c_r2                                    &
     &      (hd_num_s_ele_grping, s_ele_grouping_ctl)
        call read_control_array_c_r2                                    &
     &      (hd_num_z_ele_grping, z_ele_grouping_ctl)
!
!
        call read_character_ctl_item(hd_2nd_grp_direction,              &
     &          i_2nd_grp_direction, sph_grp_direction_ctl)
      end do
!
      end subroutine read_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------!
      end module m_control_data_add_ele_grp

