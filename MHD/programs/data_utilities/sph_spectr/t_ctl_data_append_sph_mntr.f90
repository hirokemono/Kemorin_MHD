!>@file   t_ctl_data_append_sph_mntr.f90
!!        module t_ctl_data_append_sph_mntr
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief control data to append monitor files
!!
!!@verbatim
!!      subroutine read_ctl_file_add_sph_mntr(file_name, add_mtr_ctl)
!!      subroutine dealloc_ctl_data_add_sph_mntr(add_mtr_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(ctl_data_add_sph_monitor), intent(inout) :: add_mtr_ctl
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin monitor_data_connect_ctl
!!    folder_to_read_ctl    'no02'
!!    folder_to_add_ctl     'monitor'
!!
!!    begin monitor_file_list_ctl
!!      ...
!!    end monitor_file_list_ctl
!!  end monitor_data_connect_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_append_sph_mntr
!
      use m_precision
!
      use t_ctl_data_sph_monitor_list
      use t_control_array_character
      use t_control_array_real
!
      implicit none
!
      type ctl_data_add_sph_monitor
!>        directory of time series to be read
        type(read_character_item) :: folder_to_read_ctl
!>        directory of time series to add
        type(read_character_item) :: folder_to_add_ctl
!
!>        End time for trim time series
        type(read_real_item) :: end_time_ctl
!
!>        List of monitor file prefixes
        type(sph_monitor_files_ctl) :: monitor_list_ctl
!
        integer (kind = kint) :: i_add_sph_monitor = 0
      end type ctl_data_add_sph_monitor
!
      character(len=kchara), parameter, private                         &
     &      :: hd_monitor_data_connect = 'monitor_data_connect_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_folder_to_read_ctl = 'folder_to_read_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_folder_to_add_ctl = 'folder_to_add_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_end_time_ctl = 'end_time_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_monitor_file_list = 'monitor_file_list_ctl'
!
      private :: read_ctl_data_add_sph_mntr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_ctl_file_add_sph_mntr(file_name, add_mtr_ctl)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_name
      type(ctl_data_add_sph_monitor), intent(inout) :: add_mtr_ctl
!
      integer(kind=kint), parameter :: ctl_file_code = 11
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      open(ctl_file_code, file = file_name, status='old' )
      do
        call load_one_line_from_control                                 &
     &     (ctl_file_code, hd_monitor_data_connect, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call read_ctl_data_add_sph_mntr(ctl_file_code,                  &
     &      hd_monitor_data_connect, add_mtr_ctl, c_buf1)
        if(add_mtr_ctl%i_add_sph_monitor .gt. 0) exit
      end do
      close(ctl_file_code)
!
      end subroutine read_ctl_file_add_sph_mntr
!
! ----------------------------------------------------------------------
!
      subroutine read_ctl_data_add_sph_mntr                             &
     &         (id_control, hd_block, add_mtr_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(ctl_data_add_sph_monitor), intent(inout) :: add_mtr_ctl
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(add_mtr_ctl%i_add_sph_monitor  .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_folder_to_read_ctl,          &
     &      add_mtr_ctl%folder_to_read_ctl)
        call read_chara_ctl_type(c_buf, hd_folder_to_add_ctl,           &
     &      add_mtr_ctl%folder_to_add_ctl)
        call read_real_ctl_type(c_buf, hd_end_time_ctl,                 &
     &      add_mtr_ctl%end_time_ctl)
!
        call read_ctl_sph_monitor_list                                  &
     &     (id_control, hd_monitor_file_list,                           &
     &      add_mtr_ctl%monitor_list_ctl, c_buf)
      end do
      add_mtr_ctl%i_add_sph_monitor = 1
!
      end subroutine read_ctl_data_add_sph_mntr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_add_sph_mntr(add_mtr_ctl)
!
      type(ctl_data_add_sph_monitor), intent(inout) :: add_mtr_ctl
!
!
      add_mtr_ctl%folder_to_read_ctl%iflag = 0
      add_mtr_ctl%folder_to_add_ctl%iflag =  0
      add_mtr_ctl%end_time_ctl%iflag = 0
      add_mtr_ctl%i_add_sph_monitor =  0
!
      call dealloc_ctl_sph_monitor_list(add_mtr_ctl%monitor_list_ctl)
!
      end subroutine dealloc_ctl_data_add_sph_mntr
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_append_sph_mntr
