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
      character(len=kchara) :: group_mesh_head_ctl = 'grouping_mesh'
      character(len=kchara) :: grp_evo_data_ctl = 'correlation.dat'
      character(len=kchara) :: grp_ucd_data_head_ctl = 'correlation'
!
      character(len=kchara) :: start_ele_grp_name_ctl
      integer(kind = kint) :: ngrp_ele_grp_ctl
!
      character(len=kchara) :: time_average_data_ctl = 'NO'
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
      integer(kind= kint) :: i_group_mesh_head = 0
      integer(kind= kint) :: i_group_data_name = 0
      integer(kind= kint) :: i_group_udt_head = 0
      integer(kind= kint) :: i_start_ele_grp_name = 0
      integer(kind= kint) :: i_ngrp_ele_grp = 0
      integer(kind= kint) :: i_time_average_data = 0
!
!
      private :: fname_grp_data_ctl
!
      private :: hd_udt_ele_grp, i_ele_grp_udt
      private :: hd_grouping_plot,  i_grouping_plot
      private :: hd_group_mesh_head, hd_group_data_name
      private :: hd_group_udt_head, hd_start_ele_grp_name
      private :: hd_ngrp_ele_grp
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
      use m_ctl_data_4_time_steps
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
        call  read_time_step_ctl
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
        call read_character_ctl_item(hd_group_mesh_head,                &
     &        i_group_mesh_head, group_mesh_head_ctl)
        call read_character_ctl_item(hd_group_data_name,                &
     &        i_group_data_name, grp_evo_data_ctl)
        call read_character_ctl_item(hd_group_udt_head,                 &
     &        i_group_udt_head, grp_ucd_data_head_ctl)
        call read_character_ctl_item(hd_start_ele_grp_name,             &
     &        i_start_ele_grp_name, start_ele_grp_name_ctl)
        call read_character_ctl_item(hd_time_average_data,              &
     &        i_time_average_data, time_average_data_ctl)
!
        call read_integer_ctl_item(hd_ngrp_ele_grp,                     &
     &        i_ngrp_ele_grp, ngrp_ele_grp_ctl)
      end do
!
      end subroutine read_ctl_data_4_drmd_grp
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_ele_grp_udt
