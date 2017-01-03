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
!!      subroutine read_each_vol_spectr_ctl(hd_block, iflag, v_pwr)
!!        type(volume_spectr_control), intent(inout) :: v_pwr
!!      subroutine read_layerd_spectr_ctl(hd_block, iflag, lp_ctl)
!!      subroutine dealloc_num_spec_layer_ctl(lp_ctl)
!!        type(layerd_spectr_control), intent(inout) :: lp_ctl
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
!!  begin layered_spectrum_ctl
!!    layered_pwr_spectr_prefix    'sph_pwr_layer'
!!
!!    degree_spectr_switch         'On'
!!    order_spectr_switch          'On'
!!    diff_lm_spectr_switch        'On'
!!    axisymmetric_spectr_switch   'On'
!!
!!   if spectr_layer_ctl = 0 or negative: 
!!           No output
!!    array spectr_layer_ctl  1
!!      spectr_layer_ctl  62
!!    end array spectr_layer_ctl
!!
!!  end layered_spectrum_ctl
!!
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
!
      type layerd_spectr_control
!>        Structure for layered spectrum file prefix
        type(read_character_item) :: layered_pwr_spectr_prefix
!
!>        Structure for degree spectrum switch
        type(read_character_item) :: degree_spectr_switch
!>        Structure for order spectrum switch
        type(read_character_item) :: order_spectr_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: diff_lm_spectr_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: axis_spectr_switch
!
!>        Structure for list of radial grid of spectr energy data output
!!@n        idx_spec_layer_ctl%num:   Number of grid
!!@n        idx_spec_layer_ctl%ivec: list of radial ID of spectr data
         type(ctl_array_int) :: idx_spec_layer_ctl
      end type layerd_spectr_control
!
!
!   labels for item
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
     &           :: hd_layer_rms_head = 'layered_pwr_spectr_prefix'
!
      character(len=kchara), parameter                                  &
     &           :: hd_spctr_layer = 'spectr_layer_ctl'
!
      character(len=kchara), parameter                                  &
     &           :: hd_degree_spectr_switch = 'degree_spectr_switch'
      character(len=kchara), parameter                                  &
     &           :: hd_order_spectr_switch = 'order_spectr_switch'
      character(len=kchara), parameter                                  &
     &           :: hd_diff_lm_spectr_switch                            &
     &                              = 'axisymmetric_spectr_switch'
      character(len=kchara), parameter                                  &
     &           :: hd_axis_spectr_switch = 'diff_lm_spectr_switch'
!
      private :: hd_vol_pwr, hd_vol_ave, hd_inner_r, hd_outer_r
      private :: hd_axis_spectr_switch, hd_diff_lm_spectr_switch
      private :: hd_degree_spectr_switch, hd_order_spectr_switch
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_each_vol_spectr_ctl(hd_block, iflag, v_pwr)
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(volume_spectr_control), intent(inout) :: v_pwr
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if(iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_block, iflag)
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
      subroutine read_layerd_spectr_ctl(hd_block, iflag, lp_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(layerd_spectr_control), intent(inout) :: lp_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_block, iflag)
        if(iflag .gt. 0) exit
!
!
        call read_control_array_i1                                      &
     &     (hd_spctr_layer, lp_ctl%idx_spec_layer_ctl)
!
        call read_chara_ctl_type(hd_layer_rms_head,                     &
     &      lp_ctl%layered_pwr_spectr_prefix)
!
        call read_chara_ctl_type(hd_degree_spectr_switch,               &
     &      lp_ctl%degree_spectr_switch)
        call read_chara_ctl_type(hd_order_spectr_switch,                &
     &      lp_ctl%order_spectr_switch)
        call read_chara_ctl_type(hd_diff_lm_spectr_switch,              &
     &      lp_ctl%diff_lm_spectr_switch)
        call read_chara_ctl_type(hd_axis_spectr_switch,                 &
     &      lp_ctl%axis_spectr_switch)
      end do
!
      end subroutine read_layerd_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_spec_layer_ctl(lp_ctl)
!
      type(layerd_spectr_control), intent(inout) :: lp_ctl
!
!
      call dealloc_control_array_int(lp_ctl%idx_spec_layer_ctl)
!
      end subroutine dealloc_num_spec_layer_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_vol_spectr
