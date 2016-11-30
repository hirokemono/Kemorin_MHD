!>@file   t_ctl_data_sph_vol_spectr.f90
!!        module t_ctl_data_sph_vol_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief control date for volume averaged spectr data
!!
!!@verbatim
!!      subroutine dealloc_pick_gauss_ctl(g_pwr)
!!      subroutine dealloc_pick_gauss_l_ctl(g_pwr)
!!      subroutine dealloc_pick_gauss_m_ctl(g_pwr)
!!
!!      subroutine read_each_vol_spectr_ctl(v_pwr)
!!      subroutine read_gauss_spectr_ctl(g_pwr)
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!
!!    array volume_spectrum_ctl      2
!!      begin volume_spectrum_ctl
!!        volume_average_prefix        'sph_ave_convective'
!!        volume_pwr_spectr_prefix     'sph_pwr_convective'
!!        inner_radius_ctl           0.55
!!        outer_radius_ctl           1.4
!!      end volume_spectrum_ctl
!!
!!      begin volume_spectrum_ctl
!!        volume_average_prefix        'sph_ave_inner_core'
!!        volume_pwr_spectr_prefix     'sph_pwr_outer_core'
!!        inner_radius_ctl           0.0
!!        outer_radius_ctl           0.538
!!      end volume_spectrum_ctl
!!    end array volume_spectrum_ctl
!!
!!    begin gauss_coefficient_ctl
!!      gauss_coefs_prefix           'sph_spectr/gauss_coefs'
!!      gauss_coefs_radius_ctl    2.91
!!
!!      array pick_gauss_coefs_ctl  2
!!        pick_gauss_coefs_ctl   2  -2
!!        pick_gauss_coefs_ctl   2   2
!!      end array pick_gauss_coefs_ctl
!!
!!      array pick_gauss_coef_degree_ctl  2
!!        pick_gauss_coef_degree_ctl   2
!!        pick_gauss_coef_degree_ctl   2
!!      end array pick_gauss_coef_degree_ctl
!!
!!      array pick_gauss_coef_order_ctl  2
!!        pick_gauss_coef_order_ctl   -2
!!        pick_gauss_coef_order_ctl    2
!!      end array pick_gauss_coef_order_ctl
!!    end   gauss_coefficient_ctl
!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_sph_vol_spectr
!
      use m_precision
!
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!
      type volume_spectr_control
!>        filew name for volume average
        type(read_character_item) :: volume_spec_file_ctl
!>        filew name for volume average
        type(read_character_item) :: volume_ave_file_ctl
!
!>        Structure for inner boundary radius
        type(read_real_item) :: inner_radius_ctl
!>        Structure for outer boundary radius
        type(read_real_item) :: outer_radius_ctl
      end type volume_spectr_control
!
      type gauss_spectr_control
!>        Structure for gauss coefficient file prefix
        type(read_character_item) :: gauss_coefs_prefix
!
!>        Structure for reference radus 
        type(read_real_item) :: gauss_coefs_radius_ctl
!
!>        Structure for list of mode of Gauss coefficients output
!!@n        idx_gauss_ctl%num:   Number of mode
!!@n        idx_gauss_ctl%int1: list of degree of Gauss coefficients
!!@n        idx_gauss_ctl%int2: list of order of Gauss coefficients
        type(ctl_array_i2) :: idx_gauss_ctl
!
!>        Structure for list of degree of Gauss coefficient output
!!@n        idx_gauss_l_ctl%num:   Number of degree
!!@n        idx_gauss_l_ctl%ivec: list of degree of gauss coefficient
        type(ctl_array_int) :: idx_gauss_l_ctl
!
!>        Structure for list of order of Gauss coefficient output
!!@n        idx_gauss_m_ctl%num:   Number of order
!!@n        idx_gauss_m_ctl%ivec: list of order of gauss coefficient
        type(ctl_array_int) :: idx_gauss_m_ctl
      end type gauss_spectr_control
!
!
!   labels for item
!
      character(len=kchara), parameter                                  &
     &            :: hd_vol_spec_block =   'volume_spectrum_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_gauss_spec_block = 'gauss_coefficient_ctl'
!
!
      character(len=kchara), parameter                                  &
     &            :: hd_vol_pwr = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter                                  &
     &            :: hd_vol_ave = 'volume_average_prefix'
      character(len=kchara), parameter                                  &
     &            :: hd_inner_r = 'inner_radius_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_outer_r = 'outer_radius_ctl'
!
!
      character(len=kchara), parameter                                  &
     &           :: hd_gauss_coefs_head = 'gauss_coefs_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_gauss_coefs_r =    'gauss_coefs_radius_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_gauss_lm =   'pick_gauss_coefs_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_gauss_l = 'pick_gauss_coef_degree_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_gauss_m = 'pick_gauss_coef_order_ctl'
!
      private :: hd_vol_pwr, hd_vol_ave, hd_inner_r, hd_outer_r
      private :: hd_gauss_coefs_head, hd_gauss_coefs_r
      private :: hd_pick_gauss_lm, hd_pick_gauss_l, hd_pick_gauss_m
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_gauss_ctl(g_pwr)
!
      type(gauss_spectr_control), intent(inout) :: g_pwr
!
      call dealloc_control_array_i2(g_pwr%idx_gauss_ctl)
!
      end subroutine dealloc_pick_gauss_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_gauss_l_ctl(g_pwr)
!
      type(gauss_spectr_control), intent(inout) :: g_pwr
!
      call dealloc_control_array_int(g_pwr%idx_gauss_l_ctl)
!
      end subroutine dealloc_pick_gauss_l_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_gauss_m_ctl(g_pwr)
!
      type(gauss_spectr_control), intent(inout) :: g_pwr
!
      call dealloc_control_array_int(g_pwr%idx_gauss_m_ctl)
!
      end subroutine dealloc_pick_gauss_m_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_each_vol_spectr_ctl(v_pwr)
!
      type(volume_spectr_control), intent(inout) :: v_pwr
      integer (kind=kint) :: iflag = 0
!
!
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_vol_spec_block, iflag)
        if(iflag .gt. 0) exit
!
        call read_chara_ctl_type(hd_vol_pwr, v_pwr%volume_spec_file_ctl)
        call read_chara_ctl_type(hd_vol_ave, v_pwr%volume_ave_file_ctl)
        call read_real_ctl_type(hd_inner_r,  v_pwr%inner_radius_ctl)
        call read_real_ctl_type(hd_outer_r,  v_pwr%outer_radius_ctl)
      end do
!
      end subroutine read_each_vol_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_spectr_ctl(g_pwr)
!
      type(gauss_spectr_control), intent(inout) :: g_pwr
      integer (kind=kint) :: iflag = 0
!
!
      if(right_begin_flag(hd_gauss_spec_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_gauss_spec_block, iflag)
        if(iflag .gt. 0) exit
!
!
        call read_control_array_i2                                      &
     &     (hd_pick_gauss_lm, g_pwr%idx_gauss_ctl)
        call read_control_array_i1                                      &
     &     (hd_pick_gauss_l, g_pwr%idx_gauss_l_ctl)
        call read_control_array_i1                                      &
     &     (hd_pick_gauss_m, g_pwr%idx_gauss_m_ctl)
!
        call read_real_ctl_type(hd_gauss_coefs_r,                       &
     &      g_pwr%gauss_coefs_radius_ctl)
        call read_chara_ctl_type(hd_gauss_coefs_head,                   &
     &      g_pwr%gauss_coefs_prefix)
      end do
!
      end subroutine read_gauss_spectr_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_vol_spectr
