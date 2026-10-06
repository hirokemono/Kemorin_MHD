!>@file   t_ctl_data_tc_region_monitor.f90
!!        module t_ctl_data_tc_region_monitor
!!
!! @author T. Kera
!! @date   Programmed in Oct., 2026
!!
!!
!> @brief Control data for regional energy monitor
!!        (tangent cylinder and outer shell regions)
!!
!!@verbatim
!!      subroutine init_tc_region_monitor_ctl_label(hd_block, tc_ctl)
!!      subroutine read_tc_region_monitor_ctl                           &
!!     &         (id_control, hd_block, tc_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(tc_region_monitor_control), intent(inout) :: tc_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_tc_region_monitor_ctl(id_control, tc_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(tc_region_monitor_control), intent(in) :: tc_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine dealloc_tc_region_monitor_ctl(tc_ctl)
!!        type(tc_region_monitor_control), intent(inout) :: tc_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for regional energy monitor
!!      (put inside sph_monitor_ctl)
!!
!!  begin tc_region_monitor_ctl
!!    tc_monitor_file_prefix   'monitor/tc_region_monitor'
!!    i_step_tc_monitor        10
!!    r_split_outer_shell      1.35
!!
!!    array shear_fit_radii_ctl
!!      shear_fit_radii_ctl     0.9
!!      shear_fit_radii_ctl     1.1
!!      shear_fit_radii_ctl     1.3
!!    end array shear_fit_radii_ctl
!!  end tc_region_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_tc_region_monitor
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
!
!>        Structure for regional energy monitor setting
      type tc_region_monitor_control
!>        Block name
        character(len=kchara) :: block_name = 'tc_region_monitor_ctl'
!>        Structure for monitor file prefix
        type(read_character_item) :: tc_monitor_file_prefix_ctl
!>        Structure for monitor increment
        type(read_integer_item) :: i_step_tc_monitor_ctl
!>        Structure for radius to split outer shell regions
        type(read_real_item) :: r_split_ctl
!>        Structure for cylindrical radii for zonal shear fit
        type(ctl_array_real) :: shear_fit_radii_ctl
!
        integer (kind = kint) :: i_tc_region_monitor_ctl = 0
      end type tc_region_monitor_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &            :: hd_tc_file_prefix = 'tc_monitor_file_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_i_step_tc =      'i_step_tc_monitor'
      character(len=kchara), parameter, private                         &
     &            :: hd_r_split =        'r_split_outer_shell'
      character(len=kchara), parameter, private                         &
     &            :: hd_shear_radii =    'shear_fit_radii_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_tc_region_monitor_ctl                             &
     &         (id_control, hd_block, tc_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(tc_region_monitor_control), intent(inout) :: tc_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(tc_ctl%i_tc_region_monitor_ctl  .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_r1(id_control, hd_shear_radii,          &
     &      tc_ctl%shear_fit_radii_ctl, c_buf)
        call read_chara_ctl_type(c_buf, hd_tc_file_prefix,              &
     &      tc_ctl%tc_monitor_file_prefix_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_tc,                 &
     &      tc_ctl%i_step_tc_monitor_ctl)
        call read_real_ctl_type(c_buf, hd_r_split, tc_ctl%r_split_ctl)
      end do
      tc_ctl%i_tc_region_monitor_ctl = 1
!
      end subroutine read_tc_region_monitor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_tc_region_monitor_ctl(id_control, tc_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(tc_region_monitor_control), intent(in) :: tc_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(tc_ctl%i_tc_region_monitor_ctl .le. 0) return
!
      maxlen = len_trim(hd_tc_file_prefix)
      maxlen = max(maxlen, len_trim(hd_i_step_tc))
      maxlen = max(maxlen, len_trim(hd_r_split))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 tc_ctl%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    tc_ctl%tc_monitor_file_prefix_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    tc_ctl%i_step_tc_monitor_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    tc_ctl%r_split_ctl)
!
      call write_control_array_r1(id_control, level,                    &
     &    tc_ctl%shear_fit_radii_ctl)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                tc_ctl%block_name)
!
      end subroutine write_tc_region_monitor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_tc_region_monitor_ctl_label(hd_block, tc_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(tc_region_monitor_control), intent(inout) :: tc_ctl
!
      tc_ctl%block_name = hd_block
        call init_chara_ctl_item_label(hd_tc_file_prefix,               &
     &      tc_ctl%tc_monitor_file_prefix_ctl)
        call init_int_ctl_item_label(hd_i_step_tc,                      &
     &      tc_ctl%i_step_tc_monitor_ctl)
        call init_real_ctl_item_label(hd_r_split, tc_ctl%r_split_ctl)
        call init_real_ctl_array_label(hd_shear_radii,                  &
     &      tc_ctl%shear_fit_radii_ctl)
!
      end subroutine init_tc_region_monitor_ctl_label
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_tc_region_monitor_ctl(tc_ctl)
!
      type(tc_region_monitor_control), intent(inout) :: tc_ctl
!
!
      tc_ctl%i_tc_region_monitor_ctl = 0
!
      call dealloc_control_array_real(tc_ctl%shear_fit_radii_ctl)
      tc_ctl%tc_monitor_file_prefix_ctl%iflag = 0
      tc_ctl%i_step_tc_monitor_ctl%iflag =      0
      tc_ctl%r_split_ctl%iflag =                0
!
      end subroutine dealloc_tc_region_monitor_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_tc_region_monitor
