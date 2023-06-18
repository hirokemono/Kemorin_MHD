!>@file   ctl_file_sph_data_utils_IO.f90
!!@brief  module ctl_file_sph_data_utils_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  control data to take difference of psectr data
!!
!!@verbatim
!!      subroutine read_control_file_sph_util(file_name, ctl)
!!      subroutine write_control_file_sph_util(file_name, ctl)
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
      module ctl_file_sph_data_utils_IO
!
      use m_precision
!
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_array_character
      use t_ctl_data_sph_data_utils
!
      character(len=kchara), parameter, private                         &
     &       :: hd_control_d_sph = 'spectr_dat_util_ctl'
!
!
      character(len=kchara), parameter, private                         &
     &       :: hd_file_def = 'file_definition'
      character(len=kchara), parameter, private                         &
     &       :: hd_rename_def = 'rename_field_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &      :: hd_time_step = 'time_step_ctl'
!
      private :: read_spectr_util_control, write_spectr_util_control
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine read_control_file_sph_util(file_name, ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(spectr_data_util_ctl), intent(inout) :: ctl
!
      integer(kind = kint), parameter :: control_file_code = 11
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
        open (control_file_code, file = file_name)
        do
          call load_one_line_from_control(control_file_code, c_buf1)
          call read_spectr_util_control                                 &
     &       (control_file_code, hd_control_d_sph, ctl, c_buf1)
          if(ctl%iflag .gt. 0) exit
        end do
        close(control_file_code)
!
      end subroutine read_control_file_sph_util
!
! -----------------------------------------------------------------------
!
      subroutine write_control_file_sph_util(file_name, ctl)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(spectr_data_util_ctl), intent(in) :: ctl
!
      integer(kind = kint), parameter :: control_file_code = 11
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write control file: ', trim(file_name)
      level1 = 0
      open (control_file_code, file = file_name)
      call write_spectr_util_control                                    &
     &   (control_file_code, hd_control_d_sph, ctl, level1)
      close(control_file_code)
!
      end subroutine write_control_file_sph_util
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_spectr_util_control                               &
     &         (id_control, hd_block, ctl, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
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
      subroutine write_spectr_util_control                              &
     &         (id_control, hd_block, ctl, level)
!
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(spectr_data_util_ctl), intent(in) :: ctl
      integer(kind = kint), intent(inout) :: level
!
!
      if(ctl%iflag .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, ctl%plt, level)
      call write_control_time_step_data                                 &
     &   (id_control, hd_time_step, ctl%tctl, level)
      call write_diff_spectr_file_control                               &
     &   (id_control, hd_file_def, ctl%file_list, level)
      call write_rename_spectr_control                                  &
     &   (id_control, hd_rename_def, ctl%field_list, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_spectr_util_control
!
! -------------------------------------------------------------------
!
      end module  ctl_file_sph_data_utils_IO
