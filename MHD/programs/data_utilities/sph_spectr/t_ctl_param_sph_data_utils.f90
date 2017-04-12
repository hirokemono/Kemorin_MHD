!>@file   t_ctl_param_sph_data_utils.f90
!!@brief  module t_ctl_param_sph_data_utils
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to take difference of psectr data
!!
!!@verbatim
!!      subroutine evolution_diff_sph_spectr
!!      subroutine set_control_diff_sph_field                           &
!!     &         (plt, tctl, f_ctl, files, istart, iend, increment)
!!        type(platform_data_control), intent(in) :: plt
!!        type(time_data_control), intent(in) :: tctl
!!        type(diff_spectrum_ctl), intent(in) :: f_ctl
!!        type(diff_spectrum_file_param), intent(inout) :: files
!!@endverbatim
!
      module t_ctl_param_sph_data_utils
!
      use m_precision
      use calypso_mpi
      use t_file_IO_parameter
      use t_field_data_IO
!
      implicit none
!
      type diff_spectrum_file_param
        type(field_IO_params) :: org_file_param
        type(field_IO_params) :: sub_file_param
        type(field_IO_params) :: out_file_param
      end type diff_spectrum_file_param
!
      type rename_list
        integer(kind = kint) :: num_list
        character(len=kchara), allocatable :: org_name(:)
        character(len=kchara), allocatable :: new_name(:)
      end type rename_list
!
      private :: alloc_rename_field_list
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine alloc_rename_field_list(fld_list)
!
      type(rename_list), intent(inout) :: fld_list
!
      allocate(fld_list%org_name(fld_list%num_list))
      allocate(fld_list%new_name(fld_list%num_list))
!
      end subroutine alloc_rename_field_list
!
! -------------------------------------------------------------------
!
      subroutine dealloc_rename_field_list(fld_list)
!
      type(rename_list), intent(inout) :: fld_list
!
      deallocate(fld_list%org_name, fld_list%new_name)
!
      end subroutine dealloc_rename_field_list
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_control_diff_sph_field                             &
     &         (plt, tctl, f_ctl, files, istart, iend, increment)
!
      use t_ctl_data_sph_data_utils
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use m_file_format_switch
      use set_ctl_parallel_platform
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: plt
      type(time_data_control), intent(in) :: tctl
      type(diff_spectrum_ctl), intent(in) :: f_ctl
      type(diff_spectrum_file_param), intent(inout) :: files
      integer(kind = kint), intent(inout) :: istart, iend, increment
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
!      call check_control_num_domains(plt)
!
      call choose_para_file_format(f_ctl%org_spec_file_fmt_ctl,         &
     &   files%org_file_param%iflag_format)
      if(f_ctl%out_field_head_ctl%iflag .gt. 0) then
        files%org_file_param%file_prefix                                &
     &          = f_ctl%org_field_head_ctl%charavalue
      end if
!
      call choose_para_file_format(f_ctl%sub_spec_file_fmt_ctl,         &
     &    files%sub_file_param%iflag_format)
      if(f_ctl%sub_field_head_ctl%iflag .gt. 0) then
        files%sub_file_param%file_prefix                                &
     &          = f_ctl%sub_field_head_ctl%charavalue
      end if
!
      call choose_para_file_format(f_ctl%out_spec_file_fmt_ctl,         &
     &    files%out_file_param%iflag_format)
      if(f_ctl%out_field_head_ctl%iflag .gt. 0) then
          files%out_file_param%file_prefix                              &
     &          = f_ctl%out_field_head_ctl%charavalue
      end if
!
      write(*,*) 'f_ctl%org_field_head_ctl%iflag',                      &
     &          f_ctl%org_field_head_ctl%iflag
      write(*,*) 'f_ctl%sub_field_head_ctl%iflag',                      &
     &          f_ctl%sub_field_head_ctl%iflag
      write(*,*) 'f_ctl%out_field_head_ctl%iflag',                      &
     &          f_ctl%out_field_head_ctl%iflag
!
        if(tctl%i_step_init_ctl%iflag .gt. 0)                           &
     &             istart = tctl%i_step_init_ctl%intvalue
        if(tctl%i_step_number_ctl%iflag .gt. 0)                         &
     &             iend = tctl%i_step_number_ctl%intvalue
        if(tctl%i_step_ucd_ctl%iflag .gt. 0)                            &
     &             increment = tctl%i_step_ucd_ctl%intvalue
!
      end subroutine set_control_diff_sph_field
!
! -------------------------------------------------------------------
!
      subroutine set_control_rename_sph_fld(field_list, rename)
!
      use t_ctl_data_sph_data_utils
      use m_file_format_switch
      use set_ctl_parallel_platform
      use set_control_platform_data
!
      type(rename_spectr_ctl), intent(inout) :: field_list
      type(rename_list), intent(inout) :: rename
!
      integer(kind = kint) :: i
!
!
      rename%num_list = field_list%field_to_rename_ctl%num
      call alloc_rename_field_list(rename)
!
      do i = 1, rename%num_list
        rename%org_name(i) = field_list%field_to_rename_ctl%c1_tbl(i)
        rename%new_name(i) = field_list%field_to_rename_ctl%c2_tbl(i)
      end do
!
      call dealloc_control_array_c2(field_list%field_to_rename_ctl)
!
      end subroutine set_control_rename_sph_fld
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrt_new_field_name(sph_fst_IO, fld_list)
!
      type(field_IO), intent(inout) :: sph_fst_IO
      type(rename_list), intent(in) :: fld_list
!
      integer(kind = kint) :: i_fld, j_IO
!
!
      do i_fld = 1, sph_fst_IO%num_field_IO
        do j_IO = 1, fld_list%num_list
          if (sph_fst_IO%fld_name(i_fld)                                &
    &         .eq. fld_list%org_name(j_IO)) then
            if(my_rank .eq. 0) write(*,*) 'Rename from ',               &
    &           trim(sph_fst_IO%fld_name(i_fld)), ' to ',               &
    &           trim(fld_list%new_name(j_IO))
            sph_fst_IO%fld_name(i_fld) = fld_list%new_name(j_IO)
            exit
          end if
        end do
      end do
!
      end subroutine overwrt_new_field_name
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
!
      end module  t_ctl_param_sph_data_utils
