!>@file   analyzer_rename_sph_field.f90
!!@brief  module analyzer_rename_sph_field
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to change field names
!!
!!@verbatim
!!      subroutine evolution_rename_sph_field
!!
!!  begin difference_spectr_ctl
!!    begin data_files_def
!!      debug_flag_ctl            'ON'
!!      num_subdomain_ctl           2
!!    end data_files_def
!!
!!    begin file_definition
!!      org_sprctr_prefix        'flux_overline/spectr'
!!      out_sprctr_prefix        'direct_est/spectr'
!!
!!      org_sprctr_format        'merged_gz'
!!      out_sprctr_format        'merged_gz'
!!    end file_definition
!!
!!    begin rename_field_ctl
!!      array field_to_rename   2
!!        field_to_rename     heat_flux    SGS_heat_flux
!!        field_to_rename     inertia      SGS_inertia
!!      end array field_to_rename
!!    end begin rename_field_ctl
!!
!!    begin time_step_ctl
!!      i_step_init_ctl       0
!!      i_step_finish_ctl     2000
!!      i_step_field_ctl         1
!!    end time_step_ctl
!!  end difference_spectr_ctl
!!
!!@endverbatim
!
      module analyzer_rename_sph_field
!
      use m_precision
      use calypso_mpi
!
      use t_control_elements
      use t_field_data_IO
      use t_ctl_data_sph_data_utils
      use t_ctl_param_sph_data_utils
!
!
      character(len=kchara), parameter                                  &
     &            :: control_file_name = 'ctl_rename_sph'
!
      type(spectr_data_util_ctl), save :: ctl1
      type(diff_spectrum_file_param), save :: files1
      type(rename_list), save :: rename1
!
      private :: control_file_name
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine evolution_rename_sph_field
!
      integer(kind = kint) :: istep_fld, istart, iend, increment
!
!
      call read_control_data_diff_spectr(ctl1)
!
      call set_control_diff_sph_field                                   &
     &   (ctl1%file_list, files1, istart, iend, increment)
      call set_control_rename_sph_fld(ctl1%field_list, rename1)
!
      do istep_fld = istart, iend, increment
        call difference_of_two_spectr(istep_fld, files1, rename1)
      end do
!
      call dealloc_rename_field_list(rename1)
!
      end subroutine evolution_rename_sph_field
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine difference_of_two_spectr(istep_fld, files, fld_list)
!
      use calypso_mpi
      use m_time_data_IO
      use field_IO_select
      use const_global_element_ids
!
      integer(kind = kint), intent(in) :: istep_fld
      type(diff_spectrum_file_param), intent(in) :: files
      type(rename_list), intent(in) :: fld_list
!
      type(field_IO) :: sph_fst_IO
!
!
      call set_field_file_fmt_prefix(files%org_file_param%iflag_format, &
     &    files%org_file_param%file_prefix, sph_fst_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, istep_fld, t1_IO, sph_fst_IO)
!
!
      call calypso_mpi_barrier
      call overwrt_new_field_name(sph_fst_IO, fld_list)
      call calypso_mpi_barrier
!
      call alloc_merged_field_stack(nprocs, sph_fst_IO)
      call count_number_of_node_stack                                   &
     &   (sph_fst_IO%nnod_IO, sph_fst_IO%istack_numnod_IO)
!
      call set_field_file_fmt_prefix(files%out_file_param%iflag_format, &
     &    files%out_file_param%file_prefix, sph_fst_IO)
      call calypso_mpi_barrier
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, istep_fld, t1_IO, sph_fst_IO)
      call calypso_mpi_barrier
!
      call dealloc_merged_field_stack(sph_fst_IO)
      call dealloc_phys_data_IO(sph_fst_IO)
      call dealloc_phys_name_IO(sph_fst_IO)
!
      end subroutine difference_of_two_spectr
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
       subroutine read_control_data_diff_spectr(ctl)
!
      type(spectr_data_util_ctl), intent(inout) :: ctl
      integer(kind = kint), parameter :: control_file_code = 11
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_spectr_util_control(ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_data_diff_spectr
!
! -----------------------------------------------------------------------
!
      end module  analyzer_rename_sph_field
