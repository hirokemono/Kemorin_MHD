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
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_fields
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint) :: control_file_code = 11
      character (len = kchara) :: control_file_name='ctl_fft'
!
      type(platform_data_control), save :: new_p_plt
!
!>      Structure for field information control
      type(field_control), save :: fld_zfft_ctl
!>      Structure for time stepping control
      type(time_data_control), save :: t_zfft_ctl
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
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_new_data =      0
!
!>      label for block
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
!>      Number of field
      integer (kind=kint) :: i_phys_values =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      integer (kind=kint) :: i_tstep =      0
!
      private :: hd_fft_plane_ctl, i_fft_plane_ctl
      private :: hd_model, hd_control, i_model, i_control
      private :: hd_new_data, i_new_data
      private :: hd_phys_values, i_phys_values
      private :: hd_time_step, i_tstep
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
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, new_p_plt)
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
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_model, i_model)
        if(i_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, fld_zfft_ctl)
      end do
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_step_data
!
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, t_zfft_ctl)
      end do
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
      end module m_control_plane_fft
