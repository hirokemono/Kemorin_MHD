!>@file   bcast_control_sph_data_util.f90
!!@brief  module bcast_control_sph_data_util
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
      module bcast_control_sph_data_util
!
      use m_precision
      use calypso_mpi
      use t_ctl_data_sph_data_utils
!
      private :: bcast_spectr_util_control
      private :: bcast_diff_spectr_file_control
      private :: bcast_rename_spectr_control
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine load_control_file_sph_util(control_file_name, ctl)
!
      use ctl_file_sph_data_utils_IO
!
      character(len=kchara), intent(in) :: control_file_name
      type(spectr_data_util_ctl), intent(inout) :: ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_control_file_sph_util(control_file_name, ctl, c_buf1)
      end if
!
      call bcast_spectr_util_control(ctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(ctl%iflag, 'control file is broken')
      end if
!
      end subroutine load_control_file_sph_util
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
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
      end module  bcast_control_sph_data_util
