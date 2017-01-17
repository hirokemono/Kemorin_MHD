!>@file   t_ctl_data_sph_data_utils.f90
!!@brief  module t_ctl_data_sph_data_utils
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  control data to take difference of psectr data
!!
!!@verbatim
!!      subroutine read_diff_spectr_file_control(f_ctl)
!!      subroutine bcast_diff_spectr_file_control(f_ctl)
!!        type(diff_spectrum_ctl), intent(inout) :: f_ctl
!!      subroutine read_rename_spectr_control(list)
!!      subroutine bcast_rename_spectr_control(list)
!!        type(rename_spectr_ctl), intent(inout) :: list
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
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_elements
      use t_read_control_arrays
!
      type diff_spectrum_ctl
        type(read_character_item) :: org_field_head_ctl
        type(read_character_item) :: sub_field_head_ctl
        type(read_character_item) :: out_field_head_ctl
!
        type(read_character_item) :: org_spec_file_fmt_ctl
        type(read_character_item) :: sub_spec_file_fmt_ctl
        type(read_character_item) :: out_spec_file_fmt_ctl
      end type diff_spectrum_ctl
!
      type rename_spectr_ctl
        type(ctl_array_c2) :: field_to_rename_ctl
      end type rename_spectr_ctl
!
      type spectr_data_util_ctl
!>      Structure for file settings
        type(platform_data_control) :: plt
!>      Structure for time stepping control
        type(time_data_control) :: tctl
        type(diff_spectrum_ctl) :: file_list
        type(rename_spectr_ctl) :: field_list
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
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_tstep =      0
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
      private :: hd_time_step, i_tstep
      private :: hd_platform, i_platform
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine read_spectr_util_control(ctl)
!
      type(spectr_data_util_ctl), intent(inout) :: ctl
      integer(kind = kint) :: i_hard = 0
!
!
!   2 begin time_step_ctl
!
      if(right_begin_flag(hd_control_d_sph) .eq. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control_d_sph, i_hard)
        if(i_hard .gt. 0) exit
!
        call read_control_platforms(hd_platform, i_platform, ctl%plt)
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, ctl%tctl)
        call read_diff_spectr_file_control(ctl%file_list)
        call read_rename_spectr_control(ctl%field_list)
      end do
!
      end subroutine read_spectr_util_control
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine read_diff_spectr_file_control(file_list)
!
      type(diff_spectrum_ctl), intent(inout) :: file_list
      integer(kind = kint) :: i_hard = 0
!
!
      if(right_begin_flag(hd_file_def) .eq. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_file_def, i_hard)
        if(i_hard .gt. 0) exit
!
        call read_chara_ctl_type                                        &
     &     (hd_org_field_prefix, file_list%org_field_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_sub_field_prefix, file_list%sub_field_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_out_field_prefix, file_list%out_field_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_org_field_format, file_list%org_spec_file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_sub_field_format, file_list%sub_spec_file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_out_field_format, file_list%out_spec_file_fmt_ctl)
      end do
!
      end subroutine read_diff_spectr_file_control
!
! -----------------------------------------------------------------------
!
      subroutine bcast_diff_spectr_file_control(file_list)
!
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
      end subroutine bcast_diff_spectr_file_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_rename_spectr_control(field_list)
!
      type(rename_spectr_ctl), intent(inout) :: field_list
      integer(kind = kint) :: i_hard = 0
!
!
      if(right_begin_flag(hd_rename_def) .eq. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_rename_def, i_hard)
        if(i_hard .gt. 0) exit
!
!
        call read_control_array_c2                                      &
     &     (hd_field_to_rename, field_list%field_to_rename_ctl)
      end do
!
      end subroutine read_rename_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine bcast_rename_spectr_control(field_list)
!
      use bcast_control_arrays
!
      type(rename_spectr_ctl), intent(inout) :: field_list
!
      call bcast_ctl_array_c2(field_list%field_to_rename_ctl)
!
      end subroutine bcast_rename_spectr_control
!
! -----------------------------------------------------------------------
!
      end module  t_ctl_data_sph_data_utils
