!>@file   analyzer_diff_sph_spectr.f90
!!@brief  module analyzer_diff_sph_spectr
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to take difference of psectr data
!!
!!@verbatim
!!      subroutine initialize_sph_snap
!!      subroutine evolution_sph_snap
!!      subroutine evolution_sph_snap_badboy
!!
!!  begin difference_spectr_ctl
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
!!  end difference_spectr_ctl
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
!
      character(len=kchara), parameter                                  &
     &            :: control_file_name = 'ctl_diff_sph'
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
      type diff_spectrum_file_param
        type(field_IO_params) :: org_file_param
        type(field_IO_params) :: sub_file_param
        type(field_IO_params) :: out_file_param
      end type diff_spectrum_file_param
!
      type(diff_spectrum_ctl), save :: ctl1
      type(diff_spectrum_file_param), save :: files1
!
      character(len=kchara), parameter                                  &
     &       :: hd_control_d_sph = 'difference_spectr_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_file_def = 'file_definition'
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
      private :: control_file_name
      private :: hd_org_field_prefix, hd_org_field_format
      private :: hd_sub_field_prefix, hd_sub_field_format
      private :: hd_out_field_prefix, hd_out_field_format
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
     &   (ctl1, files1, istart, iend, increment)
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
      subroutine set_control_diff_sph_field                             &
     &         (ctl, files, istart, iend, increment)
!
      use m_file_format_switch
      use m_ctl_data_4_time_steps
      use set_ctl_parallel_platform
      use set_control_platform_data
!
      type(diff_spectrum_ctl), intent(in) :: ctl
      type(diff_spectrum_file_param), intent(inout) :: files
      integer(kind = kint), intent(inout) :: istart, iend, increment
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
!      call check_control_num_domains
!
      call choose_para_file_format                                      &
     &   (ctl%org_spec_file_fmt_ctl, files%org_file_param%iflag_format)
      if(ctl%org_field_head_ctl%iflag .gt. 0) then
        files%org_file_param%file_prefix                                &
     &          = ctl%org_field_head_ctl%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (ctl%sub_spec_file_fmt_ctl, files%sub_file_param%iflag_format)
      if(ctl%sub_field_head_ctl%iflag .gt. 0) then
        files%sub_file_param%file_prefix                                &
     &          = ctl%sub_field_head_ctl%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (ctl%out_spec_file_fmt_ctl, files%out_file_param%iflag_format)
      if(ctl%out_field_head_ctl%iflag .gt. 0) then
          files%out_file_param%file_prefix                              &
     &          = ctl%out_field_head_ctl%charavalue
      end if
!
!
        if(i_step_init_ctl%iflag .gt. 0)                                &
     &             istart = i_step_init_ctl%intvalue
        if(i_step_number_ctl%iflag .gt. 0)                              &
     &             iend = i_step_number_ctl%intvalue
        if(i_step_ucd_ctl%iflag .gt. 0)                                 &
     &             increment = i_step_ucd_ctl%intvalue
!
      end subroutine set_control_diff_sph_field
!
! -------------------------------------------------------------------
!
      subroutine difference_of_two_spectr(istep_fld, files)
!
      use calypso_mpi
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
     &   (nprocs, my_rank, istep_fld, sph_fst_IO)
!
!
      call set_field_file_fmt_prefix(files%sub_file_param%iflag_format, &
     &    files%sub_file_param%file_prefix, sph_sub_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, istep_fld, sph_sub_IO)
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
     &   (nprocs, my_rank, istep_fld, sph_fst_IO)
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
!
      subroutine overwrt_subtract_field_IO(sph_fst_IO, sph_sub_IO)
!
      type(field_IO), intent(inout) :: sph_fst_IO
      type(field_IO), intent(in) :: sph_sub_IO
!
      integer(kind = kint) :: i_fld, j_IO, ist, ndir, jst, nd
!
!
      do i_fld = 1, sph_fst_IO%num_field_IO
        ist = sph_fst_IO%istack_comp_IO(i_fld-1)
        ndir = sph_fst_IO%istack_comp_IO(i_fld) - ist
        do j_IO = 1, sph_sub_IO%num_field_IO
          if (sph_fst_IO%fld_name(i_fld)                                &
    &         .eq. sph_sub_IO%fld_name(j_IO)) then
            jst = sph_sub_IO%istack_comp_IO(j_IO-1)
!$omp parallel private(nd)
            do nd = 1, ndir
!$omp workshare
              sph_fst_IO%d_IO(1:sph_fst_IO%nnod_IO,ist+nd)              &
     &            = sph_fst_IO%d_IO(1:sph_fst_IO%nnod_IO,ist+nd)        &
     &             - sph_sub_IO%d_IO(1:sph_fst_IO%nnod_IO,ist+nd)
!$omp end workshare nowait
            end do
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine overwrt_subtract_field_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
       subroutine read_control_data_diff_spectr(ctl)
!
      type(diff_spectrum_ctl), intent(inout) :: ctl
      integer(kind = kint), parameter :: control_file_code = 11
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_diff_spectr_control(ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_data_diff_spectr
!
! -----------------------------------------------------------------------
!
      subroutine read_diff_spectr_control(ctl)
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
!
      type(diff_spectrum_ctl), intent(inout) :: ctl
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
        call read_ctl_data_4_platform
        call read_time_step_ctl
        call read_diff_spectr_file_control(ctl)
      end do
!
      end subroutine read_diff_spectr_control
!
! -------------------------------------------------------------------
!
      subroutine read_diff_spectr_file_control(ctl)
!
      type(diff_spectrum_ctl), intent(inout) :: ctl
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
     &     (hd_org_field_prefix, ctl%org_field_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_sub_field_prefix, ctl%sub_field_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_out_field_prefix, ctl%out_field_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_org_field_format, ctl%org_spec_file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_sub_field_format, ctl%sub_spec_file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_out_field_format, ctl%out_spec_file_fmt_ctl)
      end do
!
      end subroutine read_diff_spectr_file_control
!
! -----------------------------------------------------------------------
!
      end module  analyzer_diff_sph_spectr
