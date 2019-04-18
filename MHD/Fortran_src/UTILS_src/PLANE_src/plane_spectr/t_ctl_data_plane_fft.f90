!
!      module t_ctl_data_plane_fft
!
!       subroutine read_control_data_fft_plane(pfft_c)
!
!      Written by Kemorin
!
      module t_ctl_data_plane_fft
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_fields
      use t_ctl_data_4_plane_model
      use t_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint) :: control_file_code = 11
      character (len = kchara) :: control_file_name='ctl_fft'
!
      type ctl_data_plane_fft
        type(platform_data_control) :: new_p_plt
!
!>        Structure for field information control
        type(field_control) :: fld_zfft_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_zfft_ctl
!
!>        Structure for cube domain
        type(ctl_data_4_plane_model) :: cube_c_fft
        type(ctl_data_4_plane_model) :: cube2nd_cf
!
        type(read_character_item) :: plane_spectr_mode_head_ctl
        type(read_character_item) :: plane_spectr_data_head_ctl
        type(read_character_item) :: plane_spectr_ene_head_ctl
        type(read_character_item) :: plane_spectr_h_ene_head_ctl


        integer (kind=kint) :: i_fft_plane_ctl = 0
        integer (kind=kint) :: i_control = 0
        integer (kind=kint) :: i_platform =   0
        integer (kind=kint) :: i_new_data =      0
        integer (kind=kint) :: i_phys_values =   0
        integer (kind=kint) :: i_tstep =      0
        integer (kind=kint) :: i_spec_file = 0
      end type ctl_data_plane_fft
!
!   Top level
!
      character(len=kchara) :: hd_fft_plane_ctl = 'plane_fft_control'
!
!  label for entry for model section
      character(len=kchara), parameter :: hd_model =   'model'
      integer (kind=kint) :: i_model = 0
!
!  label for entry for control section
!
      character(len=kchara), parameter                                  &
     &             :: hd_plane_def = 'plane_mesh_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_2nd_plane_def = 'new_plane_mesh_ctl'

      character(len=kchara), parameter :: hd_control = 'control'
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
!
!>      label for block
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
      character(len=kchara), parameter                                  &
     &                      :: hd_spec_file = 'plane_spectr_file_def'
!
!
!   read flags
!
      character(len=kchara), parameter                                  &
     &       :: hd_plane_spec_mode_head = 'plane_spectr_mode_head'
      character(len=kchara), parameter                                  &
     &       :: hd_plane_spec_data_head = 'plane_spectr_data_head'
      character(len=kchara), parameter                                  &
     &       :: hd_plane_spec_ene_head =  'plane_spectr_ene_head'
      character(len=kchara), parameter                                  &
     &       :: hd_plane_sp_h_ene_head =  'plane_spectr_horiz_ene_head'
!
      private :: hd_spec_file
      private :: hd_plane_spec_mode_head, hd_plane_spec_data_head
      private :: hd_plane_spec_ene_head, hd_plane_sp_h_ene_head
!
      private :: hd_fft_plane_ctl
      private :: hd_model, hd_control, i_model
      private :: hd_new_data
      private :: hd_phys_values
      private :: hd_time_step, hd_plane_def
!
      private :: read_fft_plane_control_data
      private :: read_merge_field_data, read_merge_step_data
      private :: read_ctl_data_plane_spec_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_fft_plane(pfft_c)
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_fft_plane_control_data(pfft_c)
!
      close(ctl_file_code)
!
      end subroutine read_control_data_fft_plane
!
! -----------------------------------------------------------------------
!
      subroutine read_fft_plane_control_data(pfft_c)
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
!
!
      if(right_begin_flag(hd_fft_plane_ctl) .eq. 0) return
      if (pfft_c%i_fft_plane_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        pfft_c%i_fft_plane_ctl                                          &
     &        = find_control_end_flag(hd_fft_plane_ctl)
        if(pfft_c%i_fft_plane_ctl .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_new_data, pfft_c%i_new_data, pfft_c%new_p_plt)
!
        call read_ctl_data_plane_spec_file(pfft_c)
        call read_merge_field_data(pfft_c)
        call read_merge_step_data(pfft_c)
!
        call read_plane_model_param_ctl                                 &
     &     (hd_plane_def, pfft_c%cube_c_fft)
        call read_plane_model_param_ctl                                 &
     &     (hd_2nd_plane_def, pfft_c%cube2nd_cf)
      end do
!
      end subroutine read_fft_plane_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data(pfft_c)
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
!
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_model = find_control_end_flag(hd_model)
        if(i_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, pfft_c%i_phys_values, pfft_c%fld_zfft_ctl)
      end do
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_step_data(pfft_c)
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
!
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (pfft_c%i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        pfft_c%i_control = find_control_end_flag(hd_control)
        if(pfft_c%i_control .gt. 0) exit
!
        call read_control_time_step_data                                &
     &     (hd_time_step, pfft_c%i_tstep, pfft_c%t_zfft_ctl)
      end do
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_plane_spec_file(pfft_c)
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
!
!
      if(right_begin_flag(hd_spec_file) .eq. 0) return
      if (pfft_c%i_spec_file .gt. 0) return
      do
        call load_ctl_label_and_line
!
        pfft_c%i_spec_file = find_control_end_flag(hd_spec_file)
        if(pfft_c%i_spec_file .gt. 0) exit
!
        call read_chara_ctl_type(hd_plane_spec_mode_head,               &
     &      pfft_c%plane_spectr_mode_head_ctl)
        call read_chara_ctl_type(hd_plane_spec_data_head,               &
     &      pfft_c%plane_spectr_data_head_ctl)
        call read_chara_ctl_type(hd_plane_spec_ene_head,                &
     &      pfft_c%plane_spectr_ene_head_ctl)
        call read_chara_ctl_type(hd_plane_sp_h_ene_head,                &
     &      pfft_c%plane_spectr_h_ene_head_ctl)
      end do
!
      end subroutine read_ctl_data_plane_spec_file
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_plane_fft
