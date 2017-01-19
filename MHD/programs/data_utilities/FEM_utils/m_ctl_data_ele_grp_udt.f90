!m_ctl_data_ele_grp_udt.f90
!      module m_ctl_data_ele_grp_udt
!
!      Written by H. Matsui
!
!      subroutine read_control_ele_grp_udt
!
      module m_ctl_data_ele_grp_udt
!
      use m_precision
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
!>      Structure for time stepping control
      type(time_data_control), save :: t_egu_ctl
!
      type(read_character_item), save :: group_mesh_head_ctl
      type(read_character_item), save :: grp_evo_data_ctl
      type(read_character_item), save :: grp_ucd_data_head_ctl
!
      type(read_character_item), save :: start_ele_grp_name_ctl
      type(read_integer_item), save :: ngrp_ele_grp_ctl
!
      type(read_character_item), save :: time_average_data_ctl
!
!     top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_udt_ele_grp = 'element_grping_data_ctl'
      integer(kind= kint) :: i_ele_grp_udt =    0
!
!     flags for 2nd level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_grouping_plot = 'grouping_plot_ctl'
!
      integer(kind= kint) :: i_grouping_plot =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      integer (kind=kint) :: i_tstep =      0
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
      private :: hd_udt_ele_grp, i_ele_grp_udt
      private :: hd_grouping_plot,  i_grouping_plot
      private :: hd_group_mesh_head, hd_group_data_name
      private :: hd_group_udt_head, hd_start_ele_grp_name
      private :: hd_ngrp_ele_grp
      private :: hd_time_step, i_tstep
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
      subroutine read_control_ele_grp_udt
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = grp_ctl_file_code
      open ( ctl_file_code, file=fname_grp_data_ctl)
!
      call load_ctl_label_and_line
      call read_ctl_data_ele_grp_udt
!
      close(ctl_file_code)
!
      end subroutine read_control_ele_grp_udt
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_ele_grp_udt
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_udt_ele_grp) .eq. 0) return
      if (i_ele_grp_udt .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_udt_ele_grp, i_ele_grp_udt)
        if(i_ele_grp_udt .gt. 0) exit
!
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, t_egu_ctl)
        call read_ctl_data_4_drmd_grp
      end do
!
      end subroutine read_ctl_data_ele_grp_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_drmd_grp
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_grouping_plot) .eq. 0) return
      if (i_grouping_plot .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_grouping_plot, i_grouping_plot)
        if(i_grouping_plot .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_group_mesh_head,                    &
     &      group_mesh_head_ctl)
        call read_chara_ctl_type(hd_group_data_name, grp_evo_data_ctl)
        call read_chara_ctl_type(hd_group_udt_head,                     &
     &      grp_ucd_data_head_ctl)
        call read_chara_ctl_type(hd_start_ele_grp_name,                 &
     &      start_ele_grp_name_ctl)
        call read_chara_ctl_type(hd_time_average_data,                  &
     &      time_average_data_ctl)
!
        call read_integer_ctl_type(hd_ngrp_ele_grp, ngrp_ele_grp_ctl)
      end do
!
      end subroutine read_ctl_data_4_drmd_grp
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_ele_grp_udt
