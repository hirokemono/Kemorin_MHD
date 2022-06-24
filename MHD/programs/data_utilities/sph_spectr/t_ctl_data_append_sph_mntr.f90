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
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin monitor_data_connect_ctl
!!    begin append_monitor_data_ctl
!!      volume_average_prefix        'no02/sph_ave_volume'
!!      ...
!!    end append_monitor_data_ctl
!!
!!    begin target_monitor_data_ctl
!!      volume_average_prefix        'connected/sph_ave_volume'
!!      ...
!!    end target_monitor_data_ctl
!!  begin monitor_data_connect_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_append_sph_mntr
!
      use m_precision
!
      use t_ctl_data_tave_sph_monitor
!
      implicit none
!
      type ctl_data_add_sph_monitor
        type(tave_sph_monitor_ctl) :: append_monitor_ctl
        type(tave_sph_monitor_ctl) :: target_monitor_ctl
!
        integer (kind = kint) :: i_add_sph_monitor = 0
      end type ctl_data_add_sph_monitor
!
      character(len=kchara), parameter, private                         &
     &      :: hd_monitor_data_connect = 'monitor_data_connect_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_append_monitor = 'append_monitor_data_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_target_monitor = 'target_monitor_data_ctl'
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
      open(ctl_file_code, file = file_name, status='old' )
      do
        call load_one_line_from_control(ctl_file_code, c_buf1)
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
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_ctl_tave_sph_monitor(id_control, hd_append_monitor,   &
     &      add_mtr_ctl%append_monitor_ctl, c_buf)
        call read_ctl_tave_sph_monitor(id_control, hd_target_monitor,   &
     &      add_mtr_ctl%target_monitor_ctl, c_buf)
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
      integer(kind = kint) :: i
!
!
      add_mtr_ctl%i_add_sph_monitor = 0
!
      call dealloc_ctl_tave_sph_monitor(add_mtr_ctl%append_monitor_ctl)
      call dealloc_ctl_tave_sph_monitor(add_mtr_ctl%target_monitor_ctl)
!
      end subroutine dealloc_ctl_data_add_sph_mntr
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_append_sph_mntr
