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
      use t_read_control_elements
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


        integer (kind=kint) :: i_model = 0
        integer (kind=kint) :: i_fft_plane_ctl = 0
        integer (kind=kint) :: i_control = 0
        integer (kind=kint) :: i_spec_file = 0
      end type ctl_data_plane_fft
!
!   Top level
!
      character(len=kchara) :: hd_fft_plane_ctl = 'plane_fft_control'
!
!  label for entry for model section
      character(len=kchara), parameter :: hd_model =   'model'
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
      private :: hd_model, hd_control, hd_new_data
      private :: hd_phys_values, hd_time_step, hd_plane_def
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
      type(buffer_for_control) :: c_buf1
!
!
      open (control_file_code, file = control_file_name)
      do
        call load_one_line_from_control(control_file_code, c_buf1)
        call read_fft_plane_control_data                                &
     &     (control_file_code, hd_fft_plane_ctl, pfft_c, c_buf1)
        if (pfft_c%i_fft_plane_ctl .gt. 0) exit
      end do
      close(control_file_code)
!
      end subroutine read_control_data_fft_plane
!
! -----------------------------------------------------------------------
!
      subroutine read_fft_plane_control_data                            &
     &         (id_control, hd_block, pfft_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (pfft_c%i_fft_plane_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_new_data, pfft_c%new_p_plt, c_buf)
!
        call read_ctl_data_plane_spec_file                              &
     &     (id_control, hd_spec_file, pfft_c, c_buf)
        call read_merge_field_data                                      &
     &     (id_control, hd_model, pfft_c, c_buf)
        call read_merge_step_data                                       &
     &     (id_control, hd_control, pfft_c, c_buf)
!
        call read_plane_model_param_ctl                                 &
     &     (id_control, hd_plane_def, pfft_c%cube_c_fft, c_buf)
        call read_plane_model_param_ctl                                 &
     &     (id_control, hd_2nd_plane_def, pfft_c%cube2nd_cf, c_buf)
      end do
      pfft_c%i_fft_plane_ctl = 1
!
      end subroutine read_fft_plane_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data                                 &
     &         (id_control, hd_block, pfft_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pfft_c%i_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, pfft_c%fld_zfft_ctl, c_buf)
      end do
      pfft_c%i_model = 1
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_step_data                                  &
     &         (id_control, hd_block, pfft_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pfft_c%i_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, pfft_c%t_zfft_ctl, c_buf)
      end do
      pfft_c%i_control = 1
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_plane_spec_file                          &
     &         (id_control, hd_block, pfft_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_fft), intent(inout) :: pfft_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pfft_c%i_spec_file .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_plane_spec_mode_head,        &
     &      pfft_c%plane_spectr_mode_head_ctl)
        call read_chara_ctl_type(c_buf, hd_plane_spec_data_head,        &
     &      pfft_c%plane_spectr_data_head_ctl)
        call read_chara_ctl_type(c_buf, hd_plane_spec_ene_head,         &
     &      pfft_c%plane_spectr_ene_head_ctl)
        call read_chara_ctl_type(c_buf, hd_plane_sp_h_ene_head,         &
     &      pfft_c%plane_spectr_h_ene_head_ctl)
      end do
      pfft_c%i_spec_file = 1
!
      end subroutine read_ctl_data_plane_spec_file
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_plane_fft
