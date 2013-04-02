!
!      module m_ctl_data_4_zonal_fft
!
      module m_ctl_data_4_zonal_fft
!
!        programmed by H.Matsui on Oct., 2007
!
      use m_precision
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara) :: control_file_name='ctl_fft'
!
!
      character(len = kchara) :: ene_spec_head_ctl
      character(len = kchara) :: tave_ene_spec_head_ctl
      character(len = kchara) :: vector_trans_ctl
!
!   Top level
!
      character(len=kchara) :: hd_zonal_fft_ctl = 'zonal_fft'
      integer (kind=kint) :: i_zonal_fft_ctl = 0
!
!   1st level
!
      character(len=kchara) :: hd_z_fft_params = 'zonal_fft_ctl'
      integer (kind=kint) :: i_z_fft_params = 0
!
!   2nd level
!
      character(len=kchara) :: hd_z_e_sp_file = 'energy_spec_head_ctl'
      character(len=kchara) :: hd_tz_esp_file = 'ave_ene_spec_head_ctl'
      character(len=kchara) :: hd_vec_trans =   'vector_transfer_ctl'
      integer (kind=kint) :: i_z_e_sp_file = 0
      integer (kind=kint) :: i_tz_esp_file = 0
      integer (kind=kint) :: i_vec_trans = 0
!
!
      private :: control_file_code, control_file_name
      private :: hd_zonal_fft_ctl, i_zonal_fft_ctl
      private :: hd_z_fft_params, i_z_fft_params
      private :: hd_vec_trans
      private :: read_zonal_fft_control_data, read_zonal_fft_params_ctl
!
!       subroutine read_control_data_zonal_fft
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine read_control_data_zonal_fft
!
      use m_machine_parameter
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_zonal_fft_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_zonal_fft
!
! -----------------------------------------------------------------------
!
      subroutine read_zonal_fft_control_data
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_zonal_fft_ctl) .eq. 0) return
      if (i_zonal_fft_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_zonal_fft_ctl, i_zonal_fft_ctl)
        if(i_zonal_fft_ctl .gt. 0) exit
!
        call read_ctl_data_4_platform
        call read_time_step_ctl
!
        call read_zonal_fft_params_ctl
      end do
!
      end subroutine read_zonal_fft_control_data
!
! -----------------------------------------------------------------------
!
      subroutine read_zonal_fft_params_ctl
!
!
      if(right_begin_flag(hd_z_fft_params) .eq. 0) return
      if (i_z_fft_params .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_z_fft_params, i_z_fft_params)
        if(i_z_fft_params .gt. 0) exit
!
!
        call read_character_ctl_item(hd_z_e_sp_file,                    &
     &          i_z_e_sp_file, ene_spec_head_ctl)
        call read_character_ctl_item(hd_tz_esp_file,                    &
     &          i_tz_esp_file, tave_ene_spec_head_ctl)
        call read_character_ctl_item(hd_vec_trans,                      &
     &          i_vec_trans, vector_trans_ctl)
      end do
!
      end subroutine read_zonal_fft_params_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_zonal_fft
