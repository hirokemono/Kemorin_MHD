!>@file   analyzer_diff_sph_spectr.f90
!!@brief  module analyzer_diff_sph_spectr
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to take difference of psectr data
!!
!!@verbatim
!!      subroutine evolution_diff_sph_spectr
!!
!!  begin spectr_dat_util_ctl
!!    begin data_files_def
!!      debug_flag_ctl            'ON'
!!      num_subdomain_ctl           2
!!    end data_files_def
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
!!    begin time_step_ctl
!!      i_step_init_ctl       0
!!      i_step_finish_ctl     2000
!!      i_step_field_ctl         1
!!    end time_step_ctl
!!  end spectr_dat_util_ctl
!!
!!@endverbatim
!
      module analyzer_diff_sph_spectr
!
      use m_precision
      use calypso_mpi
!
      use t_control_elements
      use t_field_data_IO
      use t_ctl_data_sph_data_utils
      use t_ctl_param_sph_data_utils
!
      character(len=kchara), parameter                                  &
     &            :: control_file_name = 'ctl_diff_sph'
!
      type(spectr_data_util_ctl), save :: ctl1
      type(diff_spectrum_file_param), save :: files1
!
      private :: control_file_name
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine evolution_diff_sph_spectr
!
      integer(kind = kint) :: istep_fld, istart, iend, increment
!
!
      call read_control_data_diff_spectr(ctl1)
!
      call set_control_diff_sph_field                                   &
     &   (ctl1%file_list, files1, istart, iend, increment)
!
      do istep_fld = istart, iend, increment
        call difference_of_two_spectr(istep_fld, files1)
      end do
!
      end subroutine evolution_diff_sph_spectr
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine difference_of_two_spectr(istep_fld, files)
!
      use calypso_mpi
      use m_time_data_IO
      use field_IO_select
      use const_global_element_ids
!
      integer(kind = kint), intent(in) :: istep_fld
      type(diff_spectrum_file_param), intent(in) :: files
!
      type(field_IO) :: sph_fst_IO, sph_sub_IO
!
!
      call set_field_file_fmt_prefix(files%org_file_param%iflag_format, &
     &    files%org_file_param%file_prefix, sph_fst_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, istep_fld, t1_IO, sph_fst_IO)
!
!
      call set_field_file_fmt_prefix(files%sub_file_param%iflag_format, &
     &    files%sub_file_param%file_prefix, sph_sub_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, istep_fld, t1_IO, sph_sub_IO)
!
      call calypso_mpi_barrier
      call overwrt_subtract_field_IO(sph_fst_IO, sph_sub_IO)
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
      call dealloc_phys_data_IO(sph_sub_IO)
      call dealloc_phys_name_IO(sph_sub_IO)
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
      end module  analyzer_diff_sph_spectr
