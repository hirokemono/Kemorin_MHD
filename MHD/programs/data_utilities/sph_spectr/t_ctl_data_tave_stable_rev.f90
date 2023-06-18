!>@file   t_ctl_data_tave_stable_rev.f90
!!        module t_ctl_data_tave_stable_rev
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_ctl_file_tave_stable_rev(my_rank, tave_svsr_ctl)
!!      subroutine read_ctl_tave_stable_rev                             &
!!        type(tave_stable_reverse_ctl), intent(inout) :: tave_svsr_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine reset_ctl_tave_stable_rev(tave_svsr_ctl)
!!        type(tave_stable_reverse_ctl), intent(inout) :: tave_svsr_ctl
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin time_averaging_stable_rev
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    stable_limit_g10_ctl     7e-3
!!
!!    volume_average_file_name         'sph_ave_volume'
!!    volume_mean_square_file_name     'sph_pwr_volume_s'
!!    volume_pwr_spectr_file_name      'sph_pwr_volume_m'
!!    gauss_coefs_file_name           'sph_spectr/gauss_coefs'
!!  end time_averaging_stable_rev
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_tave_stable_rev
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
!
      implicit  none
!
!>        Control file name
      character(len = kchara), parameter, private                       &
     &           :: fname_ctl_tave_s_vs_r = 'control_stable_rev_tave'
!>        Control file ID
      integer(kind = kint), parameter, private :: id_control = 11
!
!
      type tave_stable_reverse_ctl
!>        Structure for start time
        type(read_real_item) :: start_time_ctl
!>        Structure for end time
        type(read_real_item) :: end_time_ctl
!
!>        Structure for volume average file name
        type(read_character_item) :: volume_average_file_ctl
!>        Structure for volume mean square file name
        type(read_character_item) :: volume_power_file_ctl
!>        Structure for volume spectr file name
        type(read_character_item) :: volume_spectr_file_ctl
!>        Structure for gauss coefficient file name
        type(read_character_item) :: gauss_coefs_file_ctl
!
!>        lower limit of stable dipole
        type(read_real_item) :: stable_limit_g10_ctl
!
        integer (kind = kint) :: i_tave_stable_reverse = 0
      end type tave_stable_reverse_ctl
!
!
!    label for entry
!
      character(len=kchara), parameter, private                         &
     &         :: hd_tave_stable_rev =  'time_averaging_stable_rev'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_start_time_ctl = 'start_time_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_end_time_ctl = 'end_time_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_volume_average_file = 'volume_average_file_name'
      character(len=kchara), parameter, private                         &
     &      :: hd_volume_square_file = 'volume_mean_square_file_name'
      character(len=kchara), parameter, private                         &
     &      :: hd_volume_spectr_file = 'volume_pwr_spectr_file_name'
      character(len=kchara), parameter, private                         &
     &      :: hd_gauss_coefs_file = 'gauss_coefs_file_name'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_stable_limit_g10 = 'stable_limit_g10_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_ctl_file_tave_stable_rev(my_rank, tave_svsr_ctl)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(tave_stable_reverse_ctl), intent(inout) :: tave_svsr_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        open(id_control, file = fname_ctl_tave_s_vs_r, status='old')
        do
          call load_one_line_from_control(id_control, c_buf1)
          call read_ctl_tave_stable_rev                                 &
     &       (id_control, hd_tave_stable_rev, tave_svsr_ctl, c_buf1)
          if(tave_svsr_ctl%i_tave_stable_reverse .gt. 0) exit
        end do
        close(id_control)
      end if
!
      end subroutine read_ctl_file_tave_stable_rev
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_tave_stable_rev                               &
     &         (id_control, hd_block, tave_svsr_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(tave_stable_reverse_ctl), intent(inout) :: tave_svsr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(tave_svsr_ctl%i_tave_stable_reverse  .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_start_time_ctl, tave_svsr_ctl%start_time_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_end_time_ctl, tave_svsr_ctl%end_time_ctl)
        call read_real_ctl_type(c_buf, hd_stable_limit_g10,             &
     &      tave_svsr_ctl%stable_limit_g10_ctl)
!
        call read_chara_ctl_type(c_buf, hd_volume_average_file,         &
     &      tave_svsr_ctl%volume_average_file_ctl)
        call read_chara_ctl_type(c_buf, hd_volume_square_file,          &
     &      tave_svsr_ctl%volume_power_file_ctl)
        call read_chara_ctl_type(c_buf, hd_volume_spectr_file,          &
     &      tave_svsr_ctl%volume_spectr_file_ctl)
!
        call read_chara_ctl_type(c_buf, hd_gauss_coefs_file,            &
     &      tave_svsr_ctl%gauss_coefs_file_ctl)
      end do
      tave_svsr_ctl%i_tave_stable_reverse = 1
!
      end subroutine read_ctl_tave_stable_rev
!
! -----------------------------------------------------------------------
!
      subroutine reset_ctl_tave_stable_rev(tave_svsr_ctl)
!
      type(tave_stable_reverse_ctl), intent(inout) :: tave_svsr_ctl
!
!
      tave_svsr_ctl%start_time_ctl%iflag =   0
      tave_svsr_ctl%end_time_ctl%iflag =     0
      tave_svsr_ctl%stable_limit_g10_ctl%iflag =     0
      tave_svsr_ctl%volume_average_file_ctl%iflag =   0
      tave_svsr_ctl%volume_power_file_ctl%iflag = 0
      tave_svsr_ctl%volume_spectr_file_ctl%iflag = 0
      tave_svsr_ctl%gauss_coefs_file_ctl%iflag = 0
!
      tave_svsr_ctl%i_tave_stable_reverse = 0
!
      end subroutine reset_ctl_tave_stable_rev
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_tave_stable_rev
