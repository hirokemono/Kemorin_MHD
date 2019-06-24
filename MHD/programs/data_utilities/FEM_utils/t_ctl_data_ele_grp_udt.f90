!t_ctl_data_ele_grp_udt.f90
!      module t_ctl_data_ele_grp_udt
!
!      Written by H. Matsui
!
!!      subroutine read_control_ele_grp_udt(egrp_udt_ctl)
!!        type(ctl_ele_grp_udt), intent(inout) :: egrp_udt_ctl
!
      module t_ctl_data_ele_grp_udt
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_time_steps
      use t_control_elements
!
      implicit none
!
!
      integer(kind = kint), parameter :: grp_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                 :: fname_grp_data_ctl = 'ctl_ele_group_data'
!
!
!    parameter for grouping plots
!
       type ctl_ele_grp_udt
!>      Structure for time stepping control
        type(time_data_control) :: t_egu_ctl
!
        type(read_character_item) :: group_mesh_head_ctl
        type(read_character_item) :: grp_evo_data_ctl
        type(read_character_item) :: grp_ucd_data_head_ctl
!
        type(read_character_item) :: start_ele_grp_name_ctl
        type(read_integer_item) :: ngrp_ele_grp_ctl
!
        type(read_character_item) :: time_average_data_ctl
!
        integer(kind= kint) :: i_ele_grp_udt =    0
        integer(kind= kint) :: i_grouping_plot =   0
      end type ctl_ele_grp_udt
!
!     top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_udt_ele_grp = 'element_grping_data_ctl'
!
!     flags for 2nd level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_grouping_plot = 'grouping_plot_ctl'
!
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
!
!     flags for grouping plot
!
      character(len=kchara), parameter :: hd_group_mesh_head            &
     &                    = 'grouping_mesh_head_ctl'
      character(len=kchara), parameter :: hd_group_data_name            &
     &                    = 'evolution_data_name_ctl'
      character(len=kchara), parameter :: hd_group_udt_head             &
     &                    = 'udt_data_head_ctl'
      character(len=kchara), parameter :: hd_start_ele_grp_name         &
     &                     = 'start_element_grp_name_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_ngrp_ele_grp = 'num_element_grp_ctl'
      character(len=kchara), parameter :: hd_time_average_data          &
     &                     = 'time_average_data_ctl'
!
!
      private :: fname_grp_data_ctl
!
      private :: hd_udt_ele_grp, hd_grouping_plot
      private :: hd_group_mesh_head, hd_group_data_name
      private :: hd_group_udt_head, hd_start_ele_grp_name
      private :: hd_ngrp_ele_grp, hd_time_step
!
      private :: read_ctl_data_ele_grp_udt
      private :: read_ctl_data_4_drmd_grp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_control_ele_grp_udt(egrp_udt_ctl)
!
      use skip_comment_f
!
      type(ctl_ele_grp_udt), intent(inout) :: egrp_udt_ctl
      type(buffer_for_control) :: c_buf1
!
!
      open ( grp_ctl_file_code, file=fname_grp_data_ctl)
      do
        call load_one_line_from_control(grp_ctl_file_code, c_buf1)
        call read_ctl_data_ele_grp_udt                                  &
     &     (grp_ctl_file_code, hd_udt_ele_grp, egrp_udt_ctl, c_buf1)
        if(egrp_udt_ctl%i_ele_grp_udt .gt. 0) exit
      end do
      close(grp_ctl_file_code)
!
      end subroutine read_control_ele_grp_udt
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_ele_grp_udt                              &
     &         (id_control, hd_block, egrp_udt_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_ele_grp_udt), intent(inout) :: egrp_udt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(egrp_udt_ctl%i_ele_grp_udt .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, egrp_udt_ctl%t_egu_ctl, c_buf)
        call read_ctl_data_4_drmd_grp                                   &
     &     (id_control, hd_grouping_plot, egrp_udt_ctl, c_buf)
      end do
      egrp_udt_ctl%i_ele_grp_udt = 0
!
      end subroutine read_ctl_data_ele_grp_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_drmd_grp                               &
     &         (id_control, hd_block, egrp_udt_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_ele_grp_udt), intent(inout) :: egrp_udt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(egrp_udt_ctl%i_grouping_plot .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_group_mesh_head,             &
     &      egrp_udt_ctl%group_mesh_head_ctl)
        call read_chara_ctl_type(c_buf, hd_group_data_name,             &
     &      egrp_udt_ctl%grp_evo_data_ctl)
        call read_chara_ctl_type(c_buf, hd_group_udt_head,              &
     &      egrp_udt_ctl%grp_ucd_data_head_ctl)
        call read_chara_ctl_type(c_buf, hd_start_ele_grp_name,          &
     &      egrp_udt_ctl%start_ele_grp_name_ctl)
        call read_chara_ctl_type(c_buf, hd_time_average_data,           &
     &      egrp_udt_ctl%time_average_data_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_ngrp_ele_grp, egrp_udt_ctl%ngrp_ele_grp_ctl)
      end do
      egrp_udt_ctl%i_grouping_plot = 1
!
      end subroutine read_ctl_data_4_drmd_grp
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_ele_grp_udt
