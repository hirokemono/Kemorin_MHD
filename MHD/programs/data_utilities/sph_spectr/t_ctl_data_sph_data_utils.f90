!>@file   t_ctl_data_sph_data_utils.f90
!!@brief  module t_ctl_data_sph_data_utils
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  control data to take difference of psectr data
!!
!!@verbatim
!!      subroutine read_control_file_sph_util(control_file_name, ctl)
!!      subroutine dealloc_spectr_util_control(ctl)
!!
!!  begin spectr_dat_util_ctl
!!    begin data_files_def
!!    begin time_step_ctl
!!
!!    begin file_definition
!!      org_sprctr_prefix        'flux_overline/spectr'
!!      sub_sprctr_prefix        'flux_barbar/spectr'
!!      out_sprctr_prefix        'direct_est/spectr'
!!
!!      org_sprctr_format        'merged_gz'
!!      sub_sprctr_format        'merged_gz'
!!      out_sprctr_format        'merged_gz'
!!    end file_definition
!!
!!    begin rename_field_ctl
!!      array field_to_rename   2
!!        field_to_rename     heat_flux    SGS_heat_flux
!!        field_to_rename     inertia      SGS_inertia
!!      end array field_to_rename
!!    end begin rename_field_ctl
!!  end spectr_dat_util_ctl
!!
!!@endverbatim
!
      module t_ctl_data_sph_data_utils
!
      use m_precision
      use calypso_mpi
!
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_array_character
      use t_control_array_character2
!
      type diff_spectrum_ctl
        type(read_character_item) :: org_field_head_ctl
        type(read_character_item) :: sub_field_head_ctl
        type(read_character_item) :: out_field_head_ctl
!
        type(read_character_item) :: org_spec_file_fmt_ctl
        type(read_character_item) :: sub_spec_file_fmt_ctl
        type(read_character_item) :: out_spec_file_fmt_ctl
!
        integer(kind = kint) :: iflag = 0
      end type diff_spectrum_ctl
!
      type rename_spectr_ctl
        type(ctl_array_c2) :: field_to_rename_ctl
!
        integer(kind = kint) :: iflag = 0
      end type rename_spectr_ctl
!
      type spectr_data_util_ctl
!>      Structure for file settings
        type(platform_data_control) :: plt
!>      Structure for time stepping control
        type(time_data_control) :: tctl
        type(diff_spectrum_ctl) :: file_list
        type(rename_spectr_ctl) :: field_list
!
        integer(kind = kint) :: iflag = 0
      end type spectr_data_util_ctl
!
      character(len=kchara), parameter                                  &
     &       :: hd_control_d_sph = 'spectr_dat_util_ctl'
!
!
      character(len=kchara), parameter                                  &
     &       :: hd_file_def = 'file_definition'
      character(len=kchara), parameter                                  &
     &       :: hd_rename_def = 'rename_field_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_org_field_prefix =     'org_sprctr_prefix'
      character(len=kchara), parameter                                  &
     &      :: hd_sub_field_prefix =     'sub_sprctr_prefix'
      character(len=kchara), parameter                                  &
     &      :: hd_out_field_prefix =     'out_sprctr_prefix'
!
      character(len=kchara), parameter                                  &
     &      :: hd_org_field_format =     'org_sprctr_format'
      character(len=kchara), parameter                                  &
     &      :: hd_sub_field_format =     'sub_sprctr_format'
      character(len=kchara), parameter                                  &
     &      :: hd_out_field_format =     'out_sprctr_format'
!
      character(len=kchara), parameter                                  &
     &      :: hd_field_to_rename =     'field_to_rename'
!
      private :: hd_org_field_prefix, hd_org_field_format
      private :: hd_sub_field_prefix, hd_sub_field_format
      private :: hd_out_field_prefix, hd_out_field_format
      private :: hd_rename_def, hd_field_to_rename
      private :: hd_time_step, hd_platform
!
      private :: read_spectr_util_control, bcast_spectr_util_control
      private :: read_diff_spectr_file_control
      private :: bcast_diff_spectr_file_control
      private :: reset_diff_spectr_file_control
      private :: read_rename_spectr_control
      private :: bcast_rename_spectr_control
      private :: dealloc_rename_spectr_control
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine read_control_file_sph_util(control_file_name, ctl)
!
      character(len=kchara), intent(in) :: control_file_name
      type(spectr_data_util_ctl), intent(inout) :: ctl
!
      integer(kind = kint), parameter :: control_file_code = 11
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open (control_file_code, file = control_file_name)
        do
          call load_one_line_from_control(control_file_code, c_buf1)
          call read_spectr_util_control                                 &
     &       (control_file_code, hd_control_d_sph, ctl, c_buf1)
          if(ctl%iflag .gt. 0) exit
        end do
        close(control_file_code)
      end if
!
      call bcast_spectr_util_control(ctl)
!
      end subroutine read_control_file_sph_util
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_spectr_util_control                               &
     &         (id_control, hd_block, ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spectr_data_util_ctl), intent(inout) :: ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
!   2 begin time_step_ctl
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(ctl%iflag .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, ctl%plt, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, ctl%tctl, c_buf)
        call read_diff_spectr_file_control                              &
     &     (id_control, hd_file_def, ctl%file_list, c_buf)
        call read_rename_spectr_control                                 &
     &     (id_control, hd_rename_def, ctl%field_list, c_buf)
      end do
      ctl%iflag = 1
!
      end subroutine read_spectr_util_control
!
! -------------------------------------------------------------------
!
      subroutine bcast_spectr_util_control(ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
!
      type(spectr_data_util_ctl), intent(inout) :: ctl
!
!
      call bcast_diff_spectr_file_control(ctl%file_list)
      call bcast_rename_spectr_control(ctl%field_list)
!
      call bcast_ctl_data_4_platform(ctl%plt)
      call bcast_ctl_data_4_time_step(ctl%tctl)
!
      call calypso_mpi_bcast_one_int(ctl%iflag, 0)
!
      end subroutine bcast_spectr_util_control
!
! -------------------------------------------------------------------
!
      subroutine dealloc_spectr_util_control(ctl)
!
      type(spectr_data_util_ctl), intent(inout) :: ctl
!
!
      call reset_diff_spectr_file_control(ctl%file_list)
      call dealloc_rename_spectr_control(ctl%field_list)
!
      call reset_control_platforms(ctl%plt)
      ctl%iflag = 0
!
      end subroutine dealloc_spectr_util_control
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine read_diff_spectr_file_control                          &
     &         (id_control, hd_block, file_list, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(diff_spectrum_ctl), intent(inout) :: file_list
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(file_list%iflag .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_org_field_prefix,            &
     &      file_list%org_field_head_ctl)
        call read_chara_ctl_type(c_buf, hd_sub_field_prefix,            &
     &      file_list%sub_field_head_ctl)
        call read_chara_ctl_type(c_buf, hd_out_field_prefix,            &
     &      file_list%out_field_head_ctl)
        call read_chara_ctl_type(c_buf, hd_org_field_format,            &
     &      file_list%org_spec_file_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_sub_field_format,            &
     &      file_list%sub_spec_file_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_out_field_format,            &
     &      file_list%out_spec_file_fmt_ctl)
      end do
      file_list%iflag = 1
!
      end subroutine read_diff_spectr_file_control
!
! -----------------------------------------------------------------------
!
      subroutine bcast_diff_spectr_file_control(file_list)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(diff_spectrum_ctl), intent(inout) :: file_list
!
!
      call bcast_ctl_type_c1(file_list%org_field_head_ctl)
      call bcast_ctl_type_c1(file_list%sub_field_head_ctl)
      call bcast_ctl_type_c1(file_list%out_field_head_ctl)
!
      call bcast_ctl_type_c1(file_list%org_spec_file_fmt_ctl)
      call bcast_ctl_type_c1(file_list%sub_spec_file_fmt_ctl)
      call bcast_ctl_type_c1(file_list%out_spec_file_fmt_ctl)
!
      call calypso_mpi_bcast_one_int(file_list%iflag, 0)
!
      end subroutine bcast_diff_spectr_file_control
!
! -----------------------------------------------------------------------
!
      subroutine reset_diff_spectr_file_control(file_list)
!
      type(diff_spectrum_ctl), intent(inout) :: file_list
!
!
      file_list%org_field_head_ctl%iflag = 0
      file_list%sub_field_head_ctl%iflag = 0
      file_list%out_field_head_ctl%iflag = 0
!
      file_list%org_spec_file_fmt_ctl%iflag = 0
      file_list%sub_spec_file_fmt_ctl%iflag = 0
      file_list%out_spec_file_fmt_ctl%iflag = 0
!
      file_list%iflag = 0
!
      end subroutine reset_diff_spectr_file_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_rename_spectr_control                             &
     &         (id_control, hd_block, field_list, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(rename_spectr_ctl), intent(inout) :: field_list
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(field_list%iflag .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c2(id_control,                          &
     &      hd_field_to_rename, field_list%field_to_rename_ctl, c_buf)
      end do
      field_list%iflag = 1
!
      end subroutine read_rename_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine bcast_rename_spectr_control(field_list)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(rename_spectr_ctl), intent(inout) :: field_list
!
      call bcast_ctl_array_c2(field_list%field_to_rename_ctl)
!
      call calypso_mpi_bcast_one_int(field_list%iflag, 0)
!
      end subroutine bcast_rename_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rename_spectr_control(field_list)
!
      type(rename_spectr_ctl), intent(inout) :: field_list
!
      call dealloc_control_array_c2(field_list%field_to_rename_ctl)
      field_list%iflag = 0
!
      end subroutine dealloc_rename_spectr_control
!
! -----------------------------------------------------------------------
!
      end module  t_ctl_data_sph_data_utils
