!
!      module m_control_plane_fft
!
!       subroutine read_control_data_fft_plane
!
!      Written by Kemorin
!
      module m_control_plane_fft
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint) :: control_file_code = 11
      character (len = kchara) :: control_file_name='ctl_fft'
!
!   Top level
!
      character(len=kchara) :: hd_fft_plane_ctl = 'plane_fft_control'
      integer (kind=kint) :: i_fft_plane_ctl = 0
!
!  label for entry for model section
      character(len=kchara), parameter :: hd_model =   'model'
      integer (kind=kint) :: i_model = 0
!
!  label for entry for control section
      character(len=kchara), parameter :: hd_control = 'control'
      integer (kind=kint) :: i_control = 0
!
      private :: hd_fft_plane_ctl, i_fft_plane_ctl
      private :: hd_model, hd_control, i_model, i_control
!
      private :: read_fft_plane_control_data
      private :: read_merge_field_data, read_merge_step_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_fft_plane
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_fft_plane_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_fft_plane
!
! -----------------------------------------------------------------------
!
      subroutine read_fft_plane_control_data
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_plane_model
      use m_ctl_data_plane_spec_file
      use m_ctl_data_2nd_plane
      use m_ctl_data_4_2nd_data
!
!
      if(right_begin_flag(hd_fft_plane_ctl) .eq. 0) return
      if (i_fft_plane_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_fft_plane_ctl, i_fft_plane_ctl)
        if(i_fft_plane_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_platform(plt1)
        call read_ctl_data_4_new_data
!
        call read_ctl_data_plane_spec_file
        call read_merge_field_data
        call read_merge_step_data
!
        call read_plane_model_param_ctl
        call read_2nd_plane_model_param_ctl
      end do
!
      end subroutine read_fft_plane_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data
!
       use m_ctl_data_4_fields
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_model, i_model)
        if(i_model .gt. 0) exit
!
        call read_phys_values
      end do
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_step_data
!
       use m_ctl_data_4_time_steps
!
!   2 begin time_step_ctl
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
        call read_time_step_ctl
      end do
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
      end module m_control_plane_fft
