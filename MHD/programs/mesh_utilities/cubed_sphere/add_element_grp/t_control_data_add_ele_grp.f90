!t_control_data_add_ele_grp.f90
!      module t_control_data_add_ele_grp
!
!      Written by H. Matsui on Mar., 2008
!
!!      subroutine read_control_add_elegrp(addgrp_c)
!!      subroutine dealloc_control_add_elegrp(addgrp_c)
!!        type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
      module t_control_data_add_ele_grp
!
      use m_precision
!
      use calypso_mpi
      use t_read_control_elements
      use t_ctl_data_4_platforms
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
!
        integer(kind = kint) :: i_add_ele_grp_ctl = 0
        integer(kind = kint) :: i_add_ele_grp_para = 0
      end type control_data_add_ele_grp
!
!   Top level
!
      character(len=kchara), parameter :: hd_add_ele_grp_ctl            &
     &                      = 'add_element_groups'
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
      private :: hd_add_ele_grp_ctl, hd_files_ctl
      private :: hd_add_ele_grp_para
      private :: hd_2nd_grp_direction
      private :: hd_platform, hd_new_data
      private :: hd_num_r_ele_grping, hd_num_t_ele_grping
      private :: hd_num_s_ele_grping, hd_num_z_ele_grping
!
      private :: read_control_4_add_egrp_data
      private :: bcast_control_add_elegrp
      private :: read_ctl_data_4_add_2d_egrp
      private :: bcast_ctl_data_4_add_2d_egrp
      private :: dealloc_ctl_data_4_add_2d_egrp
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
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(control_file_code, file = control_file_name)
        do
          call load_one_line_from_control(control_file_code, c_buf1)
          call read_control_4_add_egrp_data                             &
     &       (control_file_code, hd_add_ele_grp_ctl, addgrp_c, c_buf1)
          if(addgrp_c%i_add_ele_grp_ctl .gt. 0) exit
        end do
        close(control_file_code)
      end if
!
      call bcast_control_add_elegrp(addgrp_c)
!
      end subroutine read_control_add_elegrp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_control_4_add_egrp_data                           &
     &         (id_control, hd_block, addgrp_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(addgrp_c%i_add_ele_grp_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, addgrp_c%source_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_new_data, addgrp_c%added_plt, c_buf)
!
        call read_ctl_data_4_add_2d_egrp                                &
     &     (id_control, hd_add_ele_grp_para, addgrp_c, c_buf)
      end do
      addgrp_c%i_add_ele_grp_ctl = 1
!
      end subroutine read_control_4_add_egrp_data
!
! -----------------------------------------------------------------------
!
      subroutine bcast_control_add_elegrp(addgrp_c)
!
      use bcast_4_platform_ctl
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call bcast_ctl_data_4_add_2d_egrp(addgrp_c)
!
      call bcast_ctl_data_4_platform(addgrp_c%source_plt)
      call bcast_ctl_data_4_platform(addgrp_c%added_plt)
!
      call MPI_BCAST(addgrp_c%i_add_ele_grp_ctl, 1,                     &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_control_add_elegrp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_control_add_elegrp(addgrp_c)
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call dealloc_ctl_data_4_add_2d_egrp(addgrp_c)
!
      call reset_control_platforms(addgrp_c%source_plt)
      call reset_control_platforms(addgrp_c%added_plt)
!
      addgrp_c%i_add_ele_grp_ctl = 0
!
      end subroutine dealloc_control_add_elegrp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_add_2d_egrp                            &
     &         (id_control, hd_block, addgrp_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(addgrp_c%i_add_ele_grp_para .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r2(id_control,                        &
     &      hd_num_r_ele_grping, addgrp_c%r_ele_grouping_ctl, c_buf)
        call read_control_array_c_r2(id_control,                        &
     &      hd_num_t_ele_grping, addgrp_c%t_ele_grouping_ctl, c_buf)
        call read_control_array_c_r2(id_control,                        &
     &      hd_num_s_ele_grping, addgrp_c%s_ele_grouping_ctl, c_buf)
        call read_control_array_c_r2(id_control,                        &
     &      hd_num_z_ele_grping, addgrp_c%z_ele_grouping_ctl, c_buf)
!
!
        call read_chara_ctl_type(c_buf, hd_2nd_grp_direction,           &
     &      addgrp_c%sph_grp_direction_ctl)
      end do
      addgrp_c%i_add_ele_grp_para = 1
!
      end subroutine read_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------!
      subroutine bcast_ctl_data_4_add_2d_egrp(addgrp_c)
!
      use bcast_control_arrays
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call bcast_ctl_array_cr2(addgrp_c%r_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%s_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%t_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%z_ele_grouping_ctl)
!
      call bcast_ctl_type_c1(addgrp_c%sph_grp_direction_ctl)
!
      call MPI_BCAST(addgrp_c%i_add_ele_grp_para, 1,                    &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------!
      subroutine dealloc_ctl_data_4_add_2d_egrp(addgrp_c)
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call dealloc_control_array_c_r2(addgrp_c%r_ele_grouping_ctl)
      call dealloc_control_array_c_r2(addgrp_c%s_ele_grouping_ctl)
      call dealloc_control_array_c_r2(addgrp_c%t_ele_grouping_ctl)
      call dealloc_control_array_c_r2(addgrp_c%z_ele_grouping_ctl)
!
      addgrp_c%sph_grp_direction_ctl%iflag = 0
!
      addgrp_c%i_add_ele_grp_para = 0
!
      end subroutine dealloc_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------!
      end module t_control_data_add_ele_grp

