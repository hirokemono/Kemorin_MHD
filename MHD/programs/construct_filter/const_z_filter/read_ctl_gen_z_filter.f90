!
!      module read_ctl_gen_z_filter
!
!     Written by H. Matsui on July, 2006
!     Modified by H. Matsui on Sep., 2007
!
!      subroutine read_control_4_z_filter
!
      module read_ctl_gen_z_filter
!
      use m_precision
      use m_read_control_elements
      use m_ctl_data_gen_z_filter
      use skip_comment_f
!
      implicit none
!
      private :: read_ctl_z_filter_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_z_filter
!
      integer(kind = kint) :: iflag
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_zfilter_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_ctl_z_filter_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_z_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_z_filter_ctl_data
!
      use m_machine_parameter
      use m_ctl_data_4_plane_model
      use m_ctl_data_gen_filter
!
      integer(kind = kint) :: iflag
!
!
      if(right_begin_flag(hd_filter_control) .eq. 0) return
      if (i_filter_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_control, i_filter_control)
        if(i_filter_control .gt. 0) exit
!
!
        call read_plane_model_param_ctl
        call read_filter_param_ctl
!
!
        call read_character_ctl_item(hd_filter_head_ctl,                &
     &        i_filter_head_ctl, filter_head_ctl)
!
        call read_integer_ctl_item(hd_ip_smp_p_ctl,                     &
     &        i_ip_smp_p_ctl, ip_smp_p_ctl)
      end do
!
      end subroutine read_ctl_z_filter_ctl_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      end module read_ctl_gen_z_filter
