!>@file   m_ctl_data_4_pickup_sph.f90
!!        module m_ctl_data_4_pickup_sph
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine deallocate_num_spec_layer_ctl
!!      subroutine deallocate_num_pick_layer_ctl
!!
!!      subroutine deallocate_pick_sph_ctl
!!      subroutine deallocate_pick_sph_l_ctl
!!      subroutine deallocate_pick_sph_m_ctl
!!
!!      subroutine deallocate_pick_gauss_ctl
!!      subroutine deallocate_pick_gauss_l_ctl
!!      subroutine deallocate_pick_gauss_m_ctl
!!
!!      subroutine deallocate_vol_sopectr_ctl
!!
!!      subroutine read_pickup_sph_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_monitor_ctl
!!    volume_average_prefix        'sph_ave_volume'
!!    volume_pwr_spectr_prefix     'sph_pwr_volume'
!!
!!    nusselt_number_prefix        'Nusselt'
!!!
!!    array volume_spectrum_ctl      2
!!      ...
!!    end array volume_spectrum_ctl
!!
!!    begin layered_spectrum_ctl
!!      ...
!!    end   layered_spectrum_ctl
!!
!!    begin gauss_coefficient_ctl
!!      ...
!!    end   gauss_coefficient_ctl
!!
!!    begin pickup_spectr_ctl
!!      ...
!!    end   pickup_spectr_ctl
!!
!!    pick_circle_coord_ctl         spherical
!!    nphi_mid_eq_ctl               500
!!    pick_cylindrical_radius_ctl   0.75
!!    pick_vertical_position_ctl    0.6
!!  end sph_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_pickup_sph
!
      use m_precision
!
      use t_control_elements
      use t_read_control_arrays
      use t_ctl_data_sph_vol_spectr
      use t_ctl_data_pick_sph_spectr
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind = kint) :: num_vol_spectr_ctl = 0
      type(volume_spectr_control), allocatable, save                    &
     &                            :: vol_pwr_spectr_ctl(:)
!
      type(layerd_spectr_control), save :: layer_pwr_spectr_ctl1
!
      type(gauss_spectr_control), save :: gauss_coef_ctl1
!
!>      Structure for spectr data pickup
      type(pick_spectr_control), save :: pick_spetr_ctl1
!
!
!>      Structure for layered spectrum file prefix
      type(read_character_item), save :: volume_average_prefix
!
!>      Structure for layered spectrum file prefix
      type(read_character_item), save :: volume_pwr_spectr_prefix
!
!>      Structure for picked spectrum file prefix
      type(read_character_item), save :: Nusselt_file_prefix
!
!
!
!>      Structure for coordiniate system for circled data
      type(read_character_item), save :: pick_circle_coord_ctl
!
!>      Structure for Number of zonal points for benchamek check
      type(read_integer_item), save :: nphi_mid_eq_ctl
!
!>      Structure for position for s
      type(read_real_item), save :: pick_s_ctl
!
!>      Structure for position for z
      type(read_real_item), save :: pick_z_ctl
!
!    label for entry
!
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      integer(kind = kint) :: i_pick_sph = 0
!
      character(len=kchara), parameter                                  &
     &            :: hd_vol_spec_block =   'volume_spectrum_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_layer_spec_block = 'layered_spectrum_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_gauss_spec_block = 'gauss_coefficient_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_sph_ctl =     'pickup_spectr_ctl'
      integer(kind = kint) :: i_vol_spectr_ctl =   0
      integer(kind = kint) :: i_layer_spectr_ctl = 0
      integer(kind = kint) :: i_gauss_pwr_ctl = 0
      integer(kind = kint) :: i_pick_sph_ctl =  0
!
!   labels for item
!
      character(len=kchara), parameter                                  &
     &           :: hd_voume_ave_head = 'volume_average_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_voume_rms_head = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_Nusselt_file_head = 'nusselt_number_prefix'
!
      character(len=kchara), parameter                                  &
     &            :: hd_nphi_mid_eq = 'nphi_mid_eq_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_s_ctl = 'pick_cylindrical_radius_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_z_ctl =  'pick_vertical_position_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_circle_coord = 'pick_circle_coord_ctl'
!
!
      private :: hd_pick_sph, i_pick_sph
      private :: hd_vol_spec_block, hd_layer_spec_block
      private :: i_vol_spectr_ctl, i_layer_spectr_ctl
      private :: hd_gauss_spec_block, i_gauss_pwr_ctl
      private :: hd_pick_sph_ctl, i_pick_sph_ctl
      private :: hd_Nusselt_file_head
      private :: hd_voume_ave_head, hd_voume_rms_head
      private :: hd_nphi_mid_eq, hd_pick_s_ctl, hd_pick_z_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_pickup_sph_ctl
!
!
      if(right_begin_flag(hd_pick_sph) .eq. 0) return
      if (i_pick_sph .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pick_sph, i_pick_sph)
        if(i_pick_sph .gt. 0) exit
!
        call read_gauss_spectr_ctl                                      &
     &     (hd_gauss_spec_block, i_gauss_pwr_ctl, gauss_coef_ctl1)
        call read_pickup_spectr_ctl                                     &
     &     (hd_pick_sph_ctl, i_pick_sph_ctl, pick_spetr_ctl1)
        call read_layerd_spectr_ctl                                     &
     &     (hd_layer_spec_block, i_layer_spectr_ctl,                    &
     &      layer_pwr_spectr_ctl1)
!
        call find_control_array_flag                                    &
     &     (hd_vol_spec_block, num_vol_spectr_ctl)
        if(num_vol_spectr_ctl .gt. 0) call read_volume_spectr_ctl
!
!
        call read_real_ctl_type(hd_pick_s_ctl, pick_s_ctl)
        call read_real_ctl_type(hd_pick_z_ctl, pick_z_ctl)
!
        call read_integer_ctl_type(hd_nphi_mid_eq, nphi_mid_eq_ctl)
!
        call read_chara_ctl_type(hd_Nusselt_file_head,                  &
     &          Nusselt_file_prefix)
!
        call read_chara_ctl_type(hd_voume_ave_head,                     &
     &          volume_average_prefix)
        call read_chara_ctl_type(hd_voume_rms_head,                     &
     &          volume_pwr_spectr_prefix)
!
        call read_chara_ctl_type(hd_circle_coord,                       &
     &          pick_circle_coord_ctl)
      end do
!
      end subroutine read_pickup_sph_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_volume_spectr_ctl
!
      integer(kind = kint) :: iflag
!
!
      if (i_vol_spectr_ctl .gt. 0) return
      allocate(vol_pwr_spectr_ctl(num_vol_spectr_ctl))
!
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag                                &
     &     (hd_vol_spec_block, num_vol_spectr_ctl, i_vol_spectr_ctl)
        if(i_vol_spectr_ctl .ge. num_vol_spectr_ctl) exit
!
        if(right_begin_flag(hd_vol_spec_block) .gt. 0) then
          i_vol_spectr_ctl = i_vol_spectr_ctl + 1
          iflag = 0
          call read_each_vol_spectr_ctl(hd_vol_spec_block, iflag,       &
     &        vol_pwr_spectr_ctl(i_vol_spectr_ctl))
        end if
      end do
!
      end subroutine read_volume_spectr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_vol_sopectr_ctl
!
      deallocate(vol_pwr_spectr_ctl)
      num_vol_spectr_ctl = 0
!
      end subroutine deallocate_vol_sopectr_ctl
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_pickup_sph
